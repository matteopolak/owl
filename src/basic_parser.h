#pragma once

#include <array>
#include <cstddef>
#include <variant>
#include <vector>

#include "token.h"

class BasicParser;

class ParserTransaction {
public:
	ParserTransaction(BasicParser &parser);
	~ParserTransaction();

	void commit() { rollback = false; }

private:
	std::size_t index;
	BasicParser &parser;
	bool rollback = true;
};

template <typename First, typename... Rest>
First get_first(First first, Rest...) {
	return first;
}

template <typename First, typename... Rest>
std::array<std::common_type_t<Rest...>, sizeof...(Rest)>
get_rest(First, Rest... rest) {
	return {rest...};
}

class BasicParser {
public:
	BasicParser(std::vector<Token> tokens) : tokens(tokens) {}

	ParserTransaction tx() { return ParserTransaction(*this); }

	template <typename T> T consume() {
		if (index >= tokens.size()) {
			throw std::runtime_error("unexpected eof");
		}

		auto token = get(index);

		if (auto t = std::get_if<T>(&token)) {
			index++;
			return *t;
		} else {
			std::string type =
					std::visit([](auto &&arg) { return arg.type(); }, token);

			throw std::runtime_error(
					fmt::format("unexpected token (basic): {}", type));
		}
	}

	template <typename T, typename... Args> T consume(Args... args) {
		static_assert(sizeof...(Args) > 0);

		auto t = consume();

		if (auto token = std::get_if<T>(&t)) {
			for (auto arg : {args...}) {
				if (token->variant == arg) {
					return *token;
				}
			}

			--index;

			if constexpr (sizeof...(Args) == 1) {
				auto arg = get_first(args...);

				throw std::runtime_error(
						fmt::format("expected `{}`, found `{}`", arg, token->variant));
			} else {
				std::string expected = fmt::format("`{}`", get_first(args...));

				for (auto arg : get_rest(args...)) {
					expected += fmt::format(", `{}`", arg);
				}

				throw std::runtime_error(fmt::format("expected one of {}, found `{}`",
																						 expected, token->variant));
			}
		}

		--index;

		if constexpr (sizeof...(Args) == 1) {
			std::string type = std::visit([](auto &&arg) { return arg.type(); }, t);
			auto variant = get_first(args...);

			throw std::runtime_error(
					fmt::format("expected `{}`, found {}", variant, type));
		} else {
			std::string type = std::visit([](auto &&arg) { return arg.type(); }, t);
			std::string expected = fmt::format("`{}`", get_first(args...));

			for (auto arg : get_rest(args...)) {
				expected += fmt::format(", `{}`", arg);
			}

			throw std::runtime_error(
					fmt::format("expected one of {}, found {}", expected, type));
		}
	}

	Token consume() {
		if (index >= tokens.size()) {
			throw std::runtime_error("unexpected eof");
		}

		return get(index++);
	}

	// forward tryConsume to consume but wrap it in a try/catch to return
	// optional<T> instead
	template <typename T, typename... Args>
	std::optional<T> tryConsume(Args... args) {
		try {
			return consume<T>(args...);
		} catch (std::runtime_error &e) {
			return std::nullopt;
		}
	}

	template <typename T, typename... Args> std::optional<T> peek(Args... args) {
		auto token = tryConsume<T>(args...);

		if (token) {
			--index;
		}

		return token;
	}

	bool isEmpty() { return index >= tokens.size(); }

	std::size_t index = 0;
	std::vector<Token> tokens;

private:
	friend class ParserTransaction;

	Token get(std::size_t i) { return tokens[i]; }
};

ParserTransaction::~ParserTransaction() {
	if (rollback) {
		parser.index = index;
	}
};

ParserTransaction::ParserTransaction(BasicParser &parser)
		: parser(parser), index(parser.index) {}

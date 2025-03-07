#pragma once

#include <array>
#include <cstddef>
#include <variant>
#include <vector>

#include "token.hpp"
#include "tokenizer.hpp"

class BasicParser;

class ParserTransaction {
public:
	ParserTransaction(BasicParser &parser);
	~ParserTransaction();

	BasicParser &parser;

	void commit() { rollback = false; }
	void anchor() { canRollback = false; }

	template <typename T, typename... Args> T consume(Args... args);
	Token consume();

private:
	std::size_t index;
	bool rollback = true;
	bool canRollback = true;
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
	BasicParser(Tokenizer tokenizer) : tokenizer(tokenizer) {}

	std::string_view source() { return tokenizer.source(); }

	ParserTransaction tx() { return ParserTransaction(*this); }

	// forward tryConsume to consume but wrap it in a try/catch to return
	// optional<T> instead
	template <typename T, typename... Args>
	std::optional<T> tryConsume(Args... args) {
		try {
			return consume<T>(args...);
		} catch (std::runtime_error &) {
			return std::nullopt;
		} catch (Error &) {
			return std::nullopt;
		}
	}

	std::optional<Token> peek() {
		if (index >= tokens.size()) {
			return std::nullopt;
		}

		return get(index);
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
	Tokenizer tokenizer;
	std::vector<Token> tokens;

private:
	friend class ParserTransaction;

	Token get(std::size_t i) { return tokens[i]; }

public:
	template <typename T> T consume() {
		if (index >= tokens.size()) {
			throw Error("unexpected eof",
									{
											{tokenizer.endSpan(), "unexpected eof here"},
									});
		}

		auto token = get(index);

		if (auto t = std::get_if<T>(&token)) {
			index++;
			return *t;
		} else {
			std::string expected = T::type();

			std::string type =
					std::visit([](auto &&arg) { return arg.type(); }, token);
			Span span = std::visit([](auto &&arg) { return arg.span(); }, token);

			throw Error(fmt::format("expected {}, found {}", expected, type),
									{{span, "unexpected token here"}});
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

				throw Error(
						fmt::format("expected `{}`, found `{}`", arg, token->variant),
						{{token->span(), "unexpected token here"}});
			} else {
				std::string expected = fmt::format("`{}`", get_first(args...));

				for (auto arg : get_rest(args...)) {
					expected += fmt::format(", `{}`", arg);
				}

				throw Error(fmt::format("expected one of {}, found `{}`", expected,
																token->variant),
										{{token->span(), "unexpected token here"}});
			}
		}

		--index;

		if constexpr (sizeof...(Args) == 1) {
			std::string type = std::visit([](auto &&arg) { return arg.type(); }, t);
			auto variant = get_first(args...);

			throw Error(fmt::format("expected `{}`, found {}", variant, type),
									{{std::visit([](auto &&arg) { return arg.span(); }, t),
										"unexpected token here"}});
		} else {
			std::string type = std::visit([](auto &&arg) { return arg.type(); }, t);
			Span span = std::visit([](auto &&arg) { return arg.span(); }, t);
			std::string expected = fmt::format("`{}`", get_first(args...));

			for (auto arg : get_rest(args...)) {
				expected += fmt::format(", `{}`", arg);
			}

			throw Error(fmt::format("expected one of {}, found {}", expected, type),
									{{span, "unexpected token here"}});
		}
	}

	Token consume() {
		if (index >= tokens.size()) {
			throw Error("unexpected eof",
									{
											{tokenizer.endSpan(), "unexpected eof here"},
									});
		}

		return get(index++);
	}
};

ParserTransaction::~ParserTransaction() {
	if (rollback) {
		parser.index = index;
	}
};

ParserTransaction::ParserTransaction(BasicParser &parser)
		: parser(parser), index(parser.index) {}

template <typename T, typename... Args>
T ParserTransaction::consume(Args... args) {
	return parser.consume<T>(args...);
}

Token ParserTransaction::consume() { return parser.consume(); }

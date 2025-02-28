#pragma once

#include <cstddef>
#include <variant>
#include <vector>

#include "token.h"

class BasicParser {
public:
	BasicParser(std::vector<Token> tokens) : tokens(tokens) {}

	template <typename T> T consume() {
		if (index >= tokens.size()) {
			throw std::runtime_error("unexpected eof");
		}

		auto token = tokens[index];

		if (auto t = std::get_if<T>(&token)) {
			index++;
			return *t;
		} else {
			std::string type =
					std::visit([](auto &&arg) { return arg.type(); }, token);

			throw std::runtime_error(fmt::format("unexpected token: {}", type));
		}
	}

	Token consume() {
		if (index >= tokens.size()) {
			throw std::runtime_error("unexpected eof");
		}

		return tokens[index++];
	}

	template <typename T> std::optional<T> tryConsume() {
		if (index >= tokens.size()) {
			return std::nullopt;
		}

		auto token = tokens[index];

		std::visit([&](auto &&arg) { fmt::print("{}\n", arg.type()); }, token);

		if (auto t = std::get_if<T>(&token)) {
			index++;
			return *t;
		} else {
			return std::nullopt;
		}
	}

	template <typename T> std::optional<T> tryPeek() {
		if (index >= tokens.size()) {
			return std::nullopt;
		}

		auto token = tokens[index];

		if (auto t = std::get_if<T>(&token)) {
			return *t;
		} else {
			return std::nullopt;
		}
	}

	bool isEmpty() { return index >= tokens.size(); }

private:
	std::vector<Token> tokens;
	std::size_t index = 0;
};

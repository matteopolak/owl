#pragma once

#include <fmt/core.h>
#include <optional>
#include <stdexcept>
#include <string>
#include <variant>
#include <vector>

#include "basic_tokenizer.h"
#include "span.h"
#include "token.h"

class Tokenizer : public BasicTokenizer {
public:
	Tokenizer(std::string source) : BasicTokenizer(source) {}

	std::vector<Token> collect() {
		std::vector<Token> tokens;

		for (;;) {
			try {
				auto token = next();
				tokens.push_back(token);

				if (std::holds_alternative<TokenEof>(token)) {
					break;
				}
			} catch (std::runtime_error &e) {
				auto span = endSpan();

				fmt::print("error at {}:{} {}\n", span.start.line, span.end.line,
									 e.what());

				throw e;
			}
		}

		return tokens;
	}

private:
	Token next() {
		// parse order: comment, op, delim, lit, keyword, ident

		// skip whitespace
		skipWhitespace();
		startSpan();

		if (isEmpty()) {
			return TokenEof(endSpan());
		}

		// comment
		if (auto comment = TokenComment::parse(*this)) {
			return *comment;
		}

		// op
		if (auto op = TokenOp::parse(*this)) {
			return *op;
		}

		// delim
		if (auto delim = TokenDelim::parse(*this)) {
			return *delim;
		}

		// lit
		if (auto lit = TokenLit::parse(*this)) {
			return *lit;
		}

		// ident (then special case keywords)
		if (auto ident = TokenIdent::parse(*this)) {
			if (auto keyword = TokenKeyword::tryFrom(*ident)) {
				return *keyword;
			}

			return *ident;
		}

		throw std::runtime_error("unexpected character");
	}
};

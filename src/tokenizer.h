#pragma once

#include <optional>
#include <stdexcept>
#include <string>
#include <variant>
#include <vector>

#include <fmt/core.h>

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

				if (std::holds_alternative<TokenDelim>(token) &&
						std::get<TokenDelim>(token).variant == Delim::COLON) {
					auto t = std::get<TokenDelim>(token);
					auto nextToken = next();

					if (std::holds_alternative<TokenDelim>(nextToken) &&
							std::get<TokenDelim>(nextToken).variant == Delim::COLON) {
						auto nextT = std::get<TokenDelim>(nextToken);
						tokens.push_back(
								TokenDelim(t.span().merge(nextT.span()), Delim::COLON_COLON));
						continue;
					} else {
						tokens.push_back(token);
						tokens.push_back(nextToken);
						continue;
					}
				}

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
			// do nothing with it
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
				if (keyword->variant == Keyword::AND) {
					return TokenOp{ident->span(), Op::AND};
				} else if (keyword->variant == Keyword::OR) {
					return TokenOp{ident->span(), Op::OR};
				} else if (keyword->variant == Keyword::NOT) {
					return TokenOp{ident->span(), Op::NOT};
				}

				return *keyword;
			}

			if (auto lit = TokenLit::tryFrom(*ident)) {
				return *lit;
			}

			return *ident;
		}

		throw std::runtime_error("unexpected character");
	}
};

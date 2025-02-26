#pragma once

#include <fmt/core.h>
#include <optional>
#include <string>
#include <variant>

#include "basic_tokenizer.h"
#include "span.h"

class TokenEof;
class TokenIdent;
class TokenLit;
class TokenOp;
class TokenDelim;
class TokenKeyword;
class TokenComment;

using Token = std::variant<TokenEof, TokenIdent, TokenLit, TokenOp, TokenDelim,
													 TokenKeyword, TokenComment>;

namespace token {
bool isDelimiter(char c);
}

class BaseToken {
public:
	BaseToken(Span span) : span_(span) {}
	BaseToken() = delete;

	Span span() { return span_; }

protected:
	std::string type();

	Span span_;
};

class TokenEof : public BaseToken {
public:
	TokenEof(Span span);

	std::string type() { return "eof"; }
};

class TokenIdent : public BaseToken {
public:
	TokenIdent(Span span, std::string value);

	std::string &value() { return value_; }
	std::string type() { return "ident"; }

	static std::optional<TokenIdent> parse(BasicTokenizer &t) {
		std::string value;

		while (auto c = t.peekChar()) {
			if (token::isDelimiter(*c)) {
				break;
			}

			value += t.nextChar();
		}

		if (value.empty()) {
			return std::nullopt;
		}

		return TokenIdent(t.endSpan(), value);
	}

	std::string value_;
};

using TokenLitType = std::variant<int, double, std::string>;

class TokenLit : public BaseToken {
public:
	TokenLit(Span span, TokenLitType value);

	std::string type() { return "lit"; }

	static std::optional<TokenLit> parse(BasicTokenizer &t) {
		if (auto lit = parseStringLit(t)) {
			return *lit;
		}

		if (auto lit = parseNumberLit(t)) {
			return *lit;
		}

		return std::nullopt;
	}

private:
	static std::optional<TokenLit> parseStringLit(BasicTokenizer &t) {
		if (!t.tryConsume("\"")) {
			return std::nullopt;
		}

		std::string value;

		while (!t.isEmpty()) {
			if (t.tryConsume("\\")) {
				if (t.tryConsume("\"")) {
					value += "\"";
				} else if (t.tryConsume("\\")) {
					value += "\\";
				} else if (t.tryConsume("n")) {
					value += "\n";
				} else if (t.tryConsume("r")) {
					value += "\r";
				} else if (t.tryConsume("t")) {
					value += "\t";
				} else if (t.tryConsume("\"")) {
					value += "\"";
				} else {
					throw std::runtime_error("invalid escape sequence");
				}
			} else if (t.startsWith("\"")) {
				break;
			} else {
				value += t.nextChar();
			}
		}

		if (!t.tryConsume("\"")) {
			throw std::runtime_error("unterminated string literal");
		}

		return TokenLit(t.endSpan(), value);
	}

	static std::optional<TokenLit> parseNumberLit(BasicTokenizer &t) {
		std::string value;

		if (auto d = t.tryNextDigit()) {
			value += *d;
		} else {
			return std::nullopt;
		}

		while (auto c = t.tryNextDigit()) {
			value += *c;
		}

		if (t.tryConsume(".")) {
			value += ".";

			while (auto c = t.tryNextDigit()) {
				value += *c;
			}

			return TokenLit(t.endSpan(), std::stod(value));
		} else {
			return TokenLit(t.endSpan(), std::stoi(value));
		}
	}

	TokenLitType value;
};

enum Op {
	ADD,
	SUB,
	MUL,
	DIV,
	MOD,
	POW,
	AND,
	OR,
	NOT,
	EQ,
	NEQ,
	LT,
	GT,
	LTE,
	GTE
};

class TokenOp : public BaseToken {
public:
	TokenOp(Span span, Op op);

	std::string type() { return "op"; }

	static std::optional<TokenOp> parse(BasicTokenizer &t) {
		Op op;

		if (t.tryConsume("+")) {
			op = ADD;
		} else if (t.tryConsume("-")) {
			op = SUB;
		} else if (t.tryConsume("*")) {
			op = MUL;
		} else if (t.tryConsume("/")) {
			op = DIV;
		} else if (t.tryConsume("%")) {
			op = MOD;
		} else if (t.tryConsume("^")) {
			op = POW;
		} else if (t.tryConsume("&&")) {
			op = AND;
		} else if (t.tryConsume("||")) {
			op = OR;
		} else if (t.tryConsume("!")) {
			op = NOT;
		} else if (t.tryConsume("==")) {
			op = EQ;
		} else if (t.tryConsume("!=")) {
			op = NEQ;
		} else if (t.tryConsume("<")) {
			op = LT;
		} else if (t.tryConsume(">")) {
			op = GT;
		} else if (t.tryConsume("<=")) {
			op = LTE;
		} else if (t.tryConsume(">=")) {
			op = GTE;
		} else {
			return std::nullopt;
		}

		return TokenOp(t.endSpan(), op);
	}

	static bool isOpStart(char c) {
		return c == '+' || c == '-' || c == '*' || c == '/' || c == '%' ||
					 c == '^' || c == '&' || c == '|' || c == '!' || c == '=' ||
					 c == '<' || c == '>';
	}

private:
	Op op;
};

enum Delim {
	LPAREN,
	RPAREN,
	LBRACE,
	RBRACE,
	LBRACKET,
	RBRACKET,
	COMMA,
	SEMICOLON,
	LANGLE,
	RANGLE
};

class TokenDelim : public BaseToken {
public:
	TokenDelim(Span span, Delim delim);

	std::string type() { return "delim"; }

	static std::optional<TokenDelim> parse(BasicTokenizer &t) {
		Delim delim;

		if (auto c = t.peekChar()) {
			if (auto d = tryDelimFrom(*c)) {
				t.nextChar();
				delim = *d;
			} else {
				return std::nullopt;
			}
		} else {
			return std::nullopt;
		}

		return TokenDelim(t.endSpan(), delim);
	}

	static std::optional<Delim> tryDelimFrom(char c) {
		if (c == '(') {
			return LPAREN;
		} else if (c == ')') {
			return RPAREN;
		} else if (c == '{') {
			return LBRACE;
		} else if (c == '}') {
			return RBRACE;
		} else if (c == '[') {
			return LBRACKET;
		} else if (c == ']') {
			return RBRACKET;
		} else if (c == ',') {
			return COMMA;
		} else if (c == ';') {
			return SEMICOLON;
		} else if (c == '<') {
			return LANGLE;
		} else if (c == '>') {
			return RANGLE;
		} else {
			return std::nullopt;
		}
	}

private:
	Delim delim;
};

enum Keyword { FN, LET, IF, ELSE, WHILE, FOR, RETURN };

class TokenKeyword : public BaseToken {
public:
	TokenKeyword(Span span, Keyword keyword);

	std::string type() { return "keyword"; }

	static std::optional<TokenKeyword> tryFrom(TokenIdent &ident) {
		std::string v = ident.value();
		Keyword keyword;

		if (v == "fn") {
			keyword = FN;
		} else if (v == "let") {
			keyword = LET;
		} else if (v == "if") {
			keyword = IF;
		} else if (v == "else") {
			keyword = ELSE;
		} else if (v == "while") {
			keyword = WHILE;
		} else if (v == "for") {
			keyword = FOR;
		} else if (v == "return") {
			keyword = RETURN;
		} else {
			return std::nullopt;
		}

		return TokenKeyword(ident.span(), keyword);
	}

private:
	Keyword keyword;
};

class TokenComment : public BaseToken {
public:
	TokenComment(Span span, std::string value);

	std::string type() { return "comment"; }

	static std::optional<TokenComment> parse(BasicTokenizer &t) {
		if (!t.tryConsume("//")) {
			return std::nullopt;
		}

		// parse until end of line
		std::string value;

		do {
			t.skipWhitespace();

			while (!t.startsWith("\n")) {
				value += t.nextChar();
			}

			t.skipWhitespace();
		} while (t.tryConsume("//"));

		return TokenComment(t.endSpan(), value);
	}

private:
	std::string value;
};

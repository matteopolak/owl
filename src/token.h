#pragma once

#include <fmt/core.h>
#include <optional>
#include <string>
#include <variant>

#include "basic_tokenizer.h"
#include "span.h"

namespace token {
bool isDelimiter(char c);
}

class BaseToken {
public:
	BaseToken(Span span) : span_(span) {}
	BaseToken() = delete;

	Span span() const { return span_; }
	std::string type() const;

protected:
	Span span_;
};

class TokenEof : public BaseToken {
public:
	TokenEof(Span span);

	static std::string type() { return "eof"; }
};

class TokenIdent : public BaseToken {
public:
	TokenIdent(Span span, std::string value);

	std::string &value() { return value_; }
	std::string type() const { return fmt::format("ident({})", value_); }

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

	// ==
	bool operator==(const TokenIdent &other) const {
		return value_ == other.value_;
	}

	// hash
	std::size_t hash() const { return std::hash<std::string>{}(value_); }
};

template <> struct std::hash<TokenIdent> {
	std::size_t operator()(const TokenIdent &token) const { return token.hash(); }
};

using TokenLitType = std::variant<int, double, std::string, bool>;

class TokenLit : public BaseToken {
public:
	TokenLit(Span span, TokenLitType value);

	TokenLitType value;
	std::string type() const { return "lit"; }

	static std::optional<TokenLit> parse(BasicTokenizer &t) {
		if (auto lit = parseStringLit(t)) {
			return *lit;
		}

		if (auto lit = parseNumberLit(t)) {
			return *lit;
		}

		return std::nullopt;
	}

	static std::optional<TokenLit> tryFrom(TokenIdent ident) {
		if (ident.value() == "true") {
			return TokenLit(ident.span(), true);
		} else if (ident.value() == "false") {
			return TokenLit(ident.span(), false);
		} else {
			return std::nullopt;
		}
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
};

enum class Op {
	ADD,
	SUB,
	MUL,
	DIV,
	MOD,
	POW,
	AND,
	OR,
	NOT,
	EQEQ,
	EQ,
	NEQ,
	LT,
	GT,
	LTE,
	GTE,

	BIT_AND,
	BIT_OR,
	BIT_XOR,
	BIT_NOT,
	BIT_LSHIFT,
	BIT_RSHIFT,

	// not parsed, used by the parser
	LPAREN,
	RPAREN,

	// used in `extern` functions for variadics
	ELLIPSIS,
};

class TokenOp : public BaseToken {
public:
	Op variant;

	TokenOp(Span span, Op op);

	std::string type() const { return "op"; }

	int precedence() {
		switch (variant) {
		case Op::OR:
			return 1;
		case Op::AND:
			return 2;
		case Op::EQEQ:
		case Op::NEQ:
		case Op::EQ:
			return 3;
		case Op::LT:
		case Op::GT:
		case Op::LTE:
		case Op::GTE:
			return 4;
		case Op::ADD:
		case Op::SUB:
			return 5;
		case Op::MUL:
		case Op::DIV:
		case Op::MOD:
			return 6;
		case Op::POW:
			return 7;
		case Op::NOT:
		case Op::BIT_NOT:
			return 8;
		case Op::BIT_AND:
			return 9;
		case Op::BIT_OR:
			return 10;
		case Op::BIT_XOR:
			return 11;
		case Op::BIT_LSHIFT:
		case Op::BIT_RSHIFT:
			return 12;
		default:
			return 0;
		}
	}

	static std::optional<TokenOp> parse(BasicTokenizer &t) {
		Op op;

		if (t.tryConsume("...")) {
			op = Op::ELLIPSIS;
		} else if (t.tryConsume("+")) {
			op = Op::ADD;
		} else if (t.tryConsume("-")) {
			op = Op::SUB;
		} else if (t.tryConsume("**")) {
			op = Op::POW;
		} else if (t.tryConsume("*")) {
			op = Op::MUL;
		} else if (t.tryConsume("/")) {
			op = Op::DIV;
		} else if (t.tryConsume("%")) {
			op = Op::MOD;
		} else if (t.tryConsume("&")) {
			op = Op::BIT_AND;
		} else if (t.tryConsume("|")) {
			op = Op::BIT_OR;
		} else if (t.tryConsume("^")) {
			op = Op::BIT_XOR;
		} else if (t.tryConsume("~")) {
			op = Op::BIT_NOT;
		} else if (t.tryConsume("<<")) {
			op = Op::BIT_LSHIFT;
		} else if (t.tryConsume(">>")) {
			op = Op::BIT_RSHIFT;
		} else if (t.tryConsume("&&")) {
			op = Op::AND;
		} else if (t.tryConsume("||")) {
			op = Op::OR;
		} else if (t.tryConsume("!")) {
			op = Op::NOT;
		} else if (t.tryConsume("==")) {
			op = Op::EQEQ;
		} else if (t.tryConsume("=")) {
			op = Op::EQ;
		} else if (t.tryConsume("!=")) {
			op = Op::NEQ;
		} else if (t.tryConsume("<")) {
			op = Op::LT;
		} else if (t.tryConsume(">")) {
			op = Op::GT;
		} else if (t.tryConsume("<=")) {
			op = Op::LTE;
		} else if (t.tryConsume(">=")) {
			op = Op::GTE;
		} else {
			return std::nullopt;
		}

		return TokenOp(t.endSpan(), op);
	}

	static bool isOpStart(char c) {
		return c == '+' || c == '-' || c == '*' || c == '/' || c == '%' ||
					 c == '^' || c == '&' || c == '|' || c == '!' || c == '=' ||
					 c == '<' || c == '>' || c == '~';
	}

	static std::string str(Op op) {
		switch (op) {
		case Op::ADD:
			return "+";
		case Op::SUB:
			return "-";
		case Op::MUL:
			return "*";
		case Op::DIV:
			return "/";
		case Op::MOD:
			return "%";
		case Op::POW:
			return "^";
		case Op::AND:
			return "&&";
		case Op::OR:
			return "||";
		case Op::NOT:
			return "!";
		case Op::EQEQ:
			return "==";
		case Op::EQ:
			return "=";
		case Op::NEQ:
			return "!=";
		case Op::LT:
			return "<";
		case Op::GT:
			return ">";
		case Op::LTE:
			return "<=";
		case Op::GTE:
			return ">=";
		case Op::LPAREN:
			return "(";
		case Op::RPAREN:
			return ")";
		case Op::ELLIPSIS:
			return "...";
		default:
			throw std::runtime_error("op str not implemented");
		}
	}
};

template <> class fmt::formatter<Op> {
public:
	constexpr auto parse(format_parse_context &ctx) { return ctx.begin(); }

	template <typename Context> auto format(const Op &op, Context &ctx) const {
		return format_to(ctx.out(), "{}", TokenOp::str(op));
	}
};

enum class Delim {
	LPAREN,
	RPAREN,
	LBRACE,
	RBRACE,
	LBRACKET,
	RBRACKET,
	COMMA,
	SEMICOLON,
	LANGLE,
	RANGLE,
	COLON,
	// not parsed, is created by the tokenizer
	COLON_COLON
};

class TokenDelim : public BaseToken {
public:
	Delim variant;

	TokenDelim(Span span, Delim delim);

	std::string type() const { return fmt::format("delim({})", str(variant)); }

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
			return Delim::LPAREN;
		} else if (c == ')') {
			return Delim::RPAREN;
		} else if (c == '{') {
			return Delim::LBRACE;
		} else if (c == '}') {
			return Delim::RBRACE;
		} else if (c == '[') {
			return Delim::LBRACKET;
		} else if (c == ']') {
			return Delim::RBRACKET;
		} else if (c == ',') {
			return Delim::COMMA;
		} else if (c == ';') {
			return Delim::SEMICOLON;
		} else if (c == '<') {
			return Delim::LANGLE;
		} else if (c == '>') {
			return Delim::RANGLE;
		} else if (c == ':') {
			return Delim::COLON;
		} else {
			return std::nullopt;
		}
	}

	static std::string str(Delim d) {
		switch (d) {
		case Delim::LPAREN:
			return "(";
		case Delim::RPAREN:
			return ")";
		case Delim::LBRACE:
			return "{";
		case Delim::RBRACE:
			return "}";
		case Delim::LBRACKET:
			return "[";
		case Delim::RBRACKET:
			return "]";
		case Delim::COMMA:
			return ",";
		case Delim::SEMICOLON:
			return ";";
		case Delim::LANGLE:
			return "<";
		case Delim::RANGLE:
			return ">";
		case Delim::COLON:
			return ":";
		case Delim::COLON_COLON:
			return "::";
		default:
			throw std::runtime_error("delim str not implemented");
		}
	}
};

template <> class fmt::formatter<Delim> {
public:
	constexpr auto parse(format_parse_context &ctx) { return ctx.begin(); }

	template <typename Context> auto format(const Delim &d, Context &ctx) const {
		return format_to(ctx.out(), "{}", TokenDelim::str(d));
	}
};

enum class Keyword {
	FN,
	LET,
	IF,
	ELSE,
	WHILE,
	FOR,
	LOOP,
	BREAK,
	CONTINUE,
	RETURN,
	STRUCT,
	INTERFACE,
	IMPLEMENT,
	IMPORT,
	EXPORT,
	EXTERN,
	CONST,
	SUPER
};

class TokenKeyword : public BaseToken {
public:
	Keyword variant;

	TokenKeyword(Span span, Keyword keyword);

	std::string type() const { return fmt::format("keyword({})", str(variant)); }

	static std::optional<TokenKeyword> tryFrom(TokenIdent &ident) {
		std::string v = ident.value();
		Keyword keyword;

		if (v == "fn") {
			keyword = Keyword::FN;
		} else if (v == "let") {
			keyword = Keyword::LET;
		} else if (v == "if") {
			keyword = Keyword::IF;
		} else if (v == "else") {
			keyword = Keyword::ELSE;
		} else if (v == "while") {
			keyword = Keyword::WHILE;
		} else if (v == "for") {
			keyword = Keyword::FOR;
		} else if (v == "return") {
			keyword = Keyword::RETURN;
		} else if (v == "struct") {
			keyword = Keyword::STRUCT;
		} else if (v == "interface") {
			keyword = Keyword::INTERFACE;
		} else if (v == "implement") {
			keyword = Keyword::IMPLEMENT;
		} else if (v == "import") {
			keyword = Keyword::IMPORT;
		} else if (v == "export") {
			keyword = Keyword::EXPORT;
		} else if (v == "extern") {
			keyword = Keyword::EXTERN;
		} else if (v == "const") {
			keyword = Keyword::CONST;
		} else if (v == "break") {
			keyword = Keyword::BREAK;
		} else if (v == "continue") {
			keyword = Keyword::CONTINUE;
		} else if (v == "loop") {
			keyword = Keyword::LOOP;
		} else if (v == "super") {
			keyword = Keyword::SUPER;
		} else {
			return std::nullopt;
		}

		return TokenKeyword(ident.span(), keyword);
	}

	static std::string str(Keyword k) {
		switch (k) {
		case Keyword::FN:
			return "fn";
		case Keyword::LET:
			return "let";
		case Keyword::IF:
			return "if";
		case Keyword::ELSE:
			return "else";
		case Keyword::WHILE:
			return "while";
		case Keyword::FOR:
			return "for";
		case Keyword::LOOP:
			return "loop";
		case Keyword::BREAK:
			return "break";
		case Keyword::CONTINUE:
			return "continue";
		case Keyword::RETURN:
			return "return";
		case Keyword::STRUCT:
			return "struct";
		case Keyword::INTERFACE:
			return "interface";
		case Keyword::IMPLEMENT:
			return "implement";
		case Keyword::IMPORT:
			return "import";
		case Keyword::EXPORT:
			return "export";
		case Keyword::EXTERN:
			return "extern";
		case Keyword::CONST:
			return "const";
		case Keyword::SUPER:
			return "super";
		default:
			throw std::runtime_error("keyword str not implemented");
		}
	}
};

template <> class fmt::formatter<Keyword> {
public:
	constexpr auto parse(format_parse_context &ctx) { return ctx.begin(); }

	template <typename Context>
	auto format(const Keyword &k, Context &ctx) const {
		return format_to(ctx.out(), "{}", TokenKeyword::str(k));
	}
};

class TokenComment : public BaseToken {
public:
	TokenComment(Span span, std::string value);

	std::string type() const { return "comment"; }

	static std::optional<TokenComment> parse(BasicTokenizer &t) {
		if (!t.tryConsume("//")) {
			return std::nullopt;
		}

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

using Token = std::variant<TokenEof, TokenIdent, TokenLit, TokenOp, TokenDelim,
													 TokenKeyword, TokenComment>;

template <> class fmt::formatter<Token> {
public:
	constexpr auto parse(format_parse_context &ctx) { return ctx.begin(); }

	template <typename Context> auto format(const Token &t, Context &ctx) {
		std::string &type = std::visit([](auto &&arg) { return arg.type(); }, t);

		return format_to(ctx.out(), "{}", type);
	}
};

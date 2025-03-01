#include <string>

#include "span.h"
#include "token.h"

TokenEof::TokenEof(Span span) : BaseToken(span) {}

TokenIdent::TokenIdent(Span span, std::string value)
		: BaseToken(span), value_(value) {}

TokenLit::TokenLit(Span span, TokenLitType value)
		: BaseToken(span), value(value) {}

TokenOp::TokenOp(Span span, Op op) : BaseToken(span), variant(op) {}

TokenDelim::TokenDelim(Span span, Delim delim)
		: BaseToken(span), variant(delim) {}

TokenKeyword::TokenKeyword(Span span, Keyword keyword)
		: BaseToken(span), variant(keyword) {}

TokenComment::TokenComment(Span span, std::string value)
		: BaseToken(span), value(value) {}

namespace token {
bool isDelimiter(char c) {
	if (TokenDelim::tryDelimFrom(c) || TokenOp::isOpStart(c)) {
		return true;
	}

	return std::isspace(c) || c == '/';
}
} // namespace token

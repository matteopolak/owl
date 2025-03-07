#pragma once

#include <optional>
#include <stdexcept>
#include <string>

#include "span.hpp"

class BasicTokenizer {
public:
	BasicTokenizer(std::string source) : source_(source) {}

	std::string_view source() { return source_; }

	inline void startSpan() { start_ = pos; }
	inline Span endSpan() { return Span{start_, pos}; }

	bool canBacktrack = true;

	// consumes the prefix. returns true if the prefix was consumed
	bool tryConsume(std::string prefix) {
		if (startsWith(prefix)) {
			pos.index += prefix.size();
			pos.column += prefix.size();
			return true;
		}
		return false;
	}

	void consume(std::string prefix) {
		if (!tryConsume(prefix)) {
			throw std::runtime_error("expected " + prefix);
		}
	}

	std::optional<char> tryNextDigit(int base = 10) {
		try {
			return nextDigit(base);
		} catch (std::runtime_error &e) {
			return std::nullopt;
		}
	}

	char nextDigit(int base = 10) {
		if (base != 10 && base != 16) {
			throw std::runtime_error("invalid base");
		}

		if (isEmpty()) {
			throw std::runtime_error("unexpected end of input");
		}

		char c = source_[pos.index];

		if (base == 10 && !std::isdigit(c)) {
			throw std::runtime_error("expected digit");
		}

		if (base == 16 && !std::isxdigit(c)) {
			throw std::runtime_error("expected hex digit");
		}

		return nextChar();
	}

	std::optional<char> peekChar() {
		if (isEmpty()) {
			return std::nullopt;
		}

		return source_[pos.index];
	}

	char nextChar() {
		if (isEmpty()) {
			throw std::runtime_error("unexpected end of input");
		}

		char c = source_[pos.index];
		pos.index++;

		if (c == '\n') {
			pos.line++;
			pos.column = 0;
		} else {
			pos.column++;
		}

		return c;
	}

	bool startsWith(std::string prefix) {
		return std::string_view(source_).substr(pos.index).starts_with(prefix);
	}

	bool isEmpty() { return pos.index >= source_.size(); }

	void skipWhitespace() {
		while (!isEmpty() && std::isspace(source_[pos.index])) {
			if (source_[pos.index] == '\n') {
				pos.line++;
				pos.column = 0;
			} else {
				pos.column++;
			}
			pos.index++;
		}
	}

private:
	std::string source_;
	Position pos = {.line = 1, .column = 0, .index = 0};
	Position start_;
};

#pragma once

#include <cstddef>
#include <string_view>

#include <fmt/core.h>

class Span;

class Position {
public:
	std::size_t line;
	std::size_t column;
	std::size_t index;

	Span close(Position end);
};

class Span {
public:
	Span(Position start, Position end) : start(start), end(end) {}
	Span() = default;

	Span merge(Span other) { return {start, other.end}; }

	std::string_view of(std::string_view source) const {
		return source.substr(start.index, end.index - start.index);
	}

	Position start;
	Position end;
};

inline Span Position::close(Position end) { return {*this, end}; }

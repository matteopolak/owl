#pragma once

#include <algorithm>
#include <cstdlib>
#include <exception>
#include <sstream>
#include <string>
#include <string_view>
#include <vector>

#include "rang.hpp"
#include <fmt/core.h>

#include "span.hpp"

class Error : public std::exception {
public:
	Error(std::string message,
				std::initializer_list<std::pair<Span, std::string>> notes)
			: message(message), notes(notes) {}

	const char *what() const noexcept override { return message.c_str(); }

	std::string format(const std::string_view &source) const {
		static std::size_t LINE_COUNT = 7;

		// TODO: move this or fix my terminal
		rang::setControlMode(rang::control::Force);

		std::ostringstream output;

		output << rang::fg::red << rang::style::bold
					 << "error: " << rang::style::reset << rang::fg::reset << message
					 << "\n\n";

		std::size_t maxLine = 0;

		for (const auto &[span, _] : notes) {
			maxLine = std::max(maxLine, span.end.line);
		}

		std::size_t chars = std::to_string(maxLine).size();
		std::string pad = std::string(chars + 3, ' ');

		for (const auto &[span, note] : notes) {
			std::size_t currentLine =
					span.start.line >= LINE_COUNT ? span.start.line + 1 - LINE_COUNT : 1;

			std::vector<std::string_view> lines =
					getLines(source, span.start.index - span.start.column, LINE_COUNT);

			for (std::size_t i = lines.size(); i > 0; i--) {
				output << rang::fg::gray << fmt::format("{:>{}}", currentLine, chars)
							 << " | " << rang::fg::reset << lines[i - 1] << '\n';
				currentLine++;
			}

			std::size_t length = std::abs(int(span.end.column - span.start.column));

			output << pad << std::string(span.start.column, ' ') << rang::fg::green
						 << rang::style::bold << std::string(length, '^') << '\n'
						 << rang::style::reset << rang::fg::reset << pad
						 << rang::style::bold << "note: " << rang::style::reset << note
						 << "\n\n";
		}

		return output.str();
	}

private:
	std::string message;
	std::vector<std::pair<Span, std::string>> notes;

	std::vector<std::string_view> getLines(const std::string_view &source,
																				 std::size_t startOfLast,
																				 std::size_t lineCount) const {
		std::vector<std::string_view> lineStarts;

		std::size_t lineStartIndex = startOfLast;
		std::size_t endOfLast = source.find('\n', startOfLast);

		if (endOfLast == std::string::npos) {
			endOfLast = source.size();
		}

		lineStarts.push_back(
				source.substr(lineStartIndex, endOfLast - lineStartIndex));

		lineStartIndex = startOfLast - 1;

		for (std::size_t i = 0; i < lineCount - 1; i++) {
			std::size_t startOfLine = source.rfind('\n', lineStartIndex - 1);

			if (startOfLine == std::string::npos) {
				lineStarts.push_back(source.substr(0, lineStartIndex - 1));
				break;
			}

			lineStarts.push_back(
					source.substr(startOfLine + 1, lineStartIndex - startOfLine - 1));
			lineStartIndex = startOfLine;
		}

		return lineStarts;
	}
};

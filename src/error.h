#pragma once

#include <algorithm>
#include <exception>
#include <sstream>
#include <string>
#include <string_view>
#include <vector>

#include <fmt/core.h>

#include "span.h"

class Error : public std::exception {
public:
	Error(std::string message,
				std::initializer_list<std::pair<Span, std::string>> notes)
			: message(message), notes(notes) {}

	const char *what() const noexcept override { return message.c_str(); }

	std::string format(const std::string_view &source) const {
		std::ostringstream output;

		output << "error: " << message << "\n";

		for (const auto &[span, note] : notes) {
			std::size_t currentLine = std::max<std::size_t>(span.start.line - 2, 1);
			std::size_t index = span.start.index - span.start.column;

			// go back span.start.line - currentLine lines in the index
			// use rfind?
			for (std::size_t i = 0; i < span.start.line - currentLine; i++) {
				index = source.rfind('\n', index - 1);
			}

			while (currentLine <= span.end.line && index < source.size()) {
				std::size_t lineEndIndex = source.find('\n', index);
				if (lineEndIndex == std::string::npos) {
					lineEndIndex = source.size();
				}

				std::string_view line = source.substr(index, lineEndIndex - index);

				output << currentLine << " | " << line << "\n";

				if (currentLine == span.end.line) {
					std::size_t length = span.end.column - span.start.column;

					output << "    " << std::string(span.start.column, ' ')
								 << std::string(length, '^') << "\n"
								 << "    note: " << note << "\n";
				}

				index = lineEndIndex + 1;
				currentLine++;
			}
		}

		return output.str();
	}

private:
	std::string message;
	std::vector<std::pair<Span, std::string>> notes;
};

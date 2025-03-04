#pragma once

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
			std::size_t startLine = span.start.line;
			std::size_t endLine = span.end.line;

			fmt::print("startLine: {}\n", startLine);

			std::size_t minLine = startLine > 2 ? startLine - 2 : 1;

			std::size_t lineStartIndex = span.start.index - span.start.column;

			std::size_t currentLine = minLine;
			std::size_t currentIndex = lineStartIndex;

			while (currentLine <= endLine && currentIndex < source.size()) {
				std::size_t lineEndIndex = source.find('\n', currentIndex);
				if (lineEndIndex == std::string::npos) {
					lineEndIndex = source.size();
				}

				std::string_view line =
						source.substr(currentIndex, lineEndIndex - currentIndex);

				output << currentLine << " | " << line << "\n";

				if (currentLine >= startLine && currentLine <= endLine) {
					std::size_t highlightStart =
							(currentLine == startLine) ? span.start.column : 0;
					std::size_t highlightEnd =
							(currentLine == endLine) ? span.end.column : line.size();

					output << std::string(std::to_string(currentLine).size() + 3 +
																		highlightStart,
																' ')
								 << std::string(highlightEnd - highlightStart, '^') << "\n";
				}

				if (currentLine == startLine) {
					output << "    note: " << note << "\n";
				}

				currentIndex = lineEndIndex + 1;
				currentLine++;
			}
		}

		return output.str();
	}

private:
	std::string message;
	std::vector<std::pair<Span, std::string>> notes;
};

#include <filesystem>
#include <fmt/core.h>
#include <fstream>
#include <sstream>
#include <string_view>

#include "mir.h"
#include "parser.h"
#include "tokenizer.h"

int main() {
	// path
	std::filesystem::path p = "example.owl";
	std::ifstream in(p, std::ifstream::in);

	std::stringstream ss;
	ss << in.rdbuf();

	if (!in) {
		fmt::print("error: failed to open file\n");
		return 1;
	}

	std::string source = ss.str();

	Tokenizer t(source);

	try {
		auto tokens = t.collect();

		for (Token &token : tokens) {
			std::visit(
					[&](auto &&arg) {
						Span span = arg.span();
						std::string_view part = span.of(source);

						fmt::print("{} ({}:{} to {}:{}) CONTENT: '{}'\n", arg.type(),
											 span.start.line, span.start.column, span.end.line,
											 span.end.column, part);
					},
					token);
		}

		Parser p(tokens);

		auto hir = p.collect();

		for (Hir &node : hir) {
			std::visit(
					[&](auto &&arg) {
						Span span = arg.span();
						std::string_view part = span.of(source);

						fmt::print("({}:{} to {}:{}) CONTENT: '{}'\n", span.start.line,
											 span.start.column, span.end.line, span.end.column, part);
					},
					node);
		}

		HirLowerer l(hir);

		auto mir = l.lower();
	} catch (std::runtime_error &e) {
		fmt::print("error: {}\n", e.what());
		return 1;
	}
}

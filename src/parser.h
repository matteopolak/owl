#pragma once

#include <fmt/core.h>
#include <optional>
#include <string>
#include <variant>
#include <vector>

#include "ast.h"

class Parser : public BasicParser {
public:
	std::vector<Node> collect() {
		std::vector<Node> nodes;

		while (!isEmpty()) {
			if (auto stmt = next()) {
				nodes.push_back(std::move(*stmt));
			}
		}

		return nodes;
	}

private:
	std::optional<Node> next() {
		if (auto eof = tryConsume<TokenEof>()) {
			return std::nullopt;
		}

		if (auto stmt = NodeStruct::parse(*this)) {
			return std::move(*stmt);
		}

		if (auto stmt = NodeFn::parse(*this)) {
			return std::move(*stmt);
		}

		if (auto stmt = NodeFor::parse(*this)) {
			return std::move(*stmt);
		}

		auto token = consume();
		std::string type = std::visit([](auto &&arg) { return arg.type(); }, token);

		throw std::runtime_error(fmt::format("unexpected token: {}", type));
	}
};

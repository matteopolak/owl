#pragma once

#include <fmt/core.h>
#include <optional>
#include <string>
#include <variant>
#include <vector>

#include "hir.h"

class Parser : public BasicParser {
public:
	std::vector<Hir> collect() {
		std::vector<Hir> nodes;

		while (auto hir = next()) {
			nodes.push_back(std::move(*hir));
		}

		return nodes;
	}

private:
	std::optional<Hir> next() {
		if (isEmpty() || peek<TokenEof>()) {
			return std::nullopt;
		}

		if (auto hir = tryParse<HirImport>()) {
			return std::move(*hir);
		}

		if (auto hir = tryParse<HirExtern>()) {
			return std::move(*hir);
		}

		auto export_ = tryConsume<TokenKeyword>(Keyword::EXPORT);

		if (auto hir = tryParse<HirFn>()) {
			hir->export_ = export_.has_value();
			return std::move(*hir);
		}

		if (auto hir = tryParse<HirStruct>()) {
			hir->export_ = export_.has_value();
			return std::move(*hir);
		}

		if (auto hir = tryParse<HirConst>()) {
			hir->export_ = export_.has_value();
			return std::move(*hir);
		}

		auto token = consume();
		std::string type = std::visit([](auto &&arg) { return arg.type(); }, token);

		throw std::runtime_error(fmt::format("unexpected token (main): {}", type));
	}

	// generic function that takes another function and wraps it in a try/catch.
	// if it throws, return nullopt. should use variadics
	template <typename T> std::optional<T> tryParse() {
		try {
			return T::parse(*this);
		} catch (std::runtime_error &e) {
			fmt::println("tryParse main: {}", e.what());
			return std::nullopt;
		}
	}
};

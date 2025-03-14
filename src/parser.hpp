#pragma once

#include <optional>
#include <string>
#include <variant>
#include <vector>

#include <fmt/core.h>

#include "hir.hpp"

class Parser : public BasicParser {
public:
	std::vector<Hir> collect() {
		tokens = tokenizer.collect();
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
		std::string type = std::visit([](auto &&arg) { return arg.str(); }, token);
		Span span = std::visit([](auto &&arg) { return arg.span(); }, token);

		throw Error(fmt::format("unexpected {}", type),
								{{span, "unexpected token here"}});
	}

	// generic function that takes another function and wraps it in a try/catch.
	// if it throws, return nullopt. should use variadics
	template <typename T> std::optional<T> tryParse() {
		try {
			return T::parse(*this);
		} catch (std::runtime_error &e) {
			return std::nullopt;
		}
	}
};

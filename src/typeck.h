#pragma once

#include <filesystem>
#include <unordered_map>
#include <unordered_set>
#include <variant>
#include <vector>

#include "hir.h"
#include "module.h"

class TypeChecker {
public:
	TypeChecker(std::filesystem::path root, std::vector<Hir> hir) : root(root) {
		modules.emplace(root, Module{HirPath::empty(), hir});
	}

	void check() {
		auto &rootModule = modules.at(HirPath::empty());

		processToplevel(rootModule);
	}

	void processToplevel(Module &module) {
		for (auto &hir : module.hir) {
			if (auto fn = std::get_if<HirFn>(&hir)) {
				module.add(fn->ident, *fn);
			}

			if (auto str = std::get_if<HirStruct>(&hir)) {
				module.add(str->ident, *str);
			}

			if (auto cnst = std::get_if<HirConst>(&hir)) {
				module.add(cnst->ident.ident, *cnst);
			}

			/*if (std::holds_alternative<HirImport>(hir)) {
				auto import = std::get<HirImport>(hir);

				std::filesystem::path path = import.path.toPath(modulePath);

				if (visited.contains(path)) {
					continue;
				}

				// if it's not within the root
				if (!path.native().starts_with(root.native())) {
					throw std::runtime_error("import outside of root");
				}

				Parser parser{Tokenizer{path}.collect()};

				auto moduleHir = parser.collect();

				modules.emplace(import.path, Module{import.path, moduleHir});
				visited.insert(path);

				processToplevel(path, moduleHir);
			}*/
		}
	}

private:
	std::filesystem::path root;
	std::unordered_set<std::filesystem::path> visited;
	std::unordered_map<HirPath, Module> modules;

	TypeCtx ctx;
};

/*
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
*/

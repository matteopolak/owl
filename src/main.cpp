#include <filesystem>
#include <fstream>
#include <sstream>

#include <fmt/core.h>
#include <llvm/Pass.h>
#include <llvm/Passes/OptimizationLevel.h>

#include "llir.h"
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
		Parser p(tokens);

		auto hir = p.collect();

		HirLowerer mir(hir);
		MirLowerer llir(mir);

		llir.lower();
		llir.module.print(llvm::errs(), nullptr);
		llir.compile(llvm::OptimizationLevel::O3, llvm::ThinOrFullLTOPhase::None);
	} catch (std::runtime_error &e) {
		fmt::print("error: {}\n", e.what());
		return 1;
	}
}

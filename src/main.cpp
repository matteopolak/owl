#include <filesystem>
#include <fstream>
#include <sstream>

#include <fmt/core.h>
#include <llvm/Pass.h>
#include <llvm/Passes/OptimizationLevel.h>
#include <llvm/Support/ManagedStatic.h>

#include "error.h"
#include "llir.h"
#include "mir.h"
#include "parser.h"
#include "tokenizer.h"

int main() {
	std::filesystem::path p = "example.owl";
	std::ifstream in(p, std::ifstream::in);

	std::stringstream ss;
	ss << in.rdbuf();

	if (!in) {
		fmt::print("error: failed to open file\n");
		return 1;
	}

	std::string source = ss.str();

	Tokenizer tok(source);

	llvm::llvm_shutdown_obj lso;

	try {
		Parser hir(tok);
		HirLowerer mir(hir);
		MirLowerer llir(mir);

		llir.lower();
		llir.compileObjectFile(llvm::OptimizationLevel::O3,
													 llvm::ThinOrFullLTOPhase::None, false);
		llir.module.print(llvm::errs(), nullptr);
		llir.link();
	} catch (Error &error) {
		fmt::print("{}\n", error.format(source));
		return 1;
	} catch (std::runtime_error &e) {
		fmt::print("error: {}\n", e.what());
		return 1;
	}
}

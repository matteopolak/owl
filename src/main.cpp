#include <filesystem>
#include <fstream>
#include <sstream>

#include <argparse/argparse.hpp>
#include <fmt/core.h>
#include <llvm/Pass.h>
#include <llvm/Passes/OptimizationLevel.h>
#include <llvm/Support/ManagedStatic.h>

#include "error.hpp"
#include "llir.hpp"
#include "mir.hpp"
#include "parser.hpp"
#include "tokenizer.hpp"

int main(int argc, char **argv) {
	argparse::ArgumentParser program("owl", "1.0.0",
																	 argparse::default_arguments::all);

	program.add_argument("file").help("source file to compile").required();
	program.add_argument("-O", "--optimize")
			.help("optimization level")
			.scan<'i', int>()
			.default_value(3);
	program.add_argument("--emit-llvm")
			.help("emit llvm ir")
			.default_value(false)
			.implicit_value(true);

	try {
		program.parse_args(argc, argv);
	} catch (const std::runtime_error &err) {
		fmt::print("{}\n", err.what());
		return 1;
	}

	int o = program.get<int>("-O");

	if (o < 0 || o > 3) {
		fmt::print("error: invalid optimization level '{}'\n", o);
		return 1;
	}

	std::filesystem::path p = program.get<std::string>("file");
	std::ifstream in(p, std::ifstream::in);

	std::stringstream ss;
	ss << in.rdbuf();

	if (!in) {
		fmt::print("error: could not read file '{}'\n", p.string());
		return 1;
	}

	// get file name without extension
	std::filesystem::path target("target");
	std::string name = p.stem().string();
	std::string object = name + ".o";

	std::string source = ss.str();

	Tokenizer tok(source);

	llvm::llvm_shutdown_obj lso;
	llvm::OptimizationLevel opt = o == 0	 ? llvm::OptimizationLevel::O0
																: o == 1 ? llvm::OptimizationLevel::O1
																: o == 2 ? llvm::OptimizationLevel::O2
																				 : llvm::OptimizationLevel::O3;

	try {
		Parser hir(tok);
		HirLowerer mir(hir);
		MirLowerer llir(mir);

		llir.lower();
		llir.compileObjectFile(target / object, opt,
													 llvm::ThinOrFullLTOPhase::None);

		if (program["--emit-llvm"] == true) {
			llir.module.print(llvm::errs(), nullptr);
		}

		llir.link(target / object, target / name);
	} catch (Error &error) {
		fmt::print("{}\n", error.format(source));
		return 1;
	} catch (std::runtime_error &e) {
		fmt::print("error: {}\n", e.what());
		return 1;
	}
}

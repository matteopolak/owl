#pragma once

#include "hir.h"
#include "module.h"
#include <llvm/IR/DerivedTypes.h>

llvm::FunctionType *llvmType(HirFn &fn, Module &module,
														 llvm::LLVMContext &context) {
	std::vector<llvm::Type *> args;

	for (auto &arg : fn.params) {
		if (auto type = arg.type) {
			args.push_back(llvmType(*type, module));
		} else {
			throw std::runtime_error("function argument has no type");
		}
	}

	return llvm::FunctionType::get(llvmType(fn.ret, module), args, false);
}

llvm::Type *llvmType(std::optional<HirPath> &path, Module &module) {
	if (path) {
		return llvmType(*path, module);
	}

	throw std::runtime_error("type required");
}

llvm::Type *llvmType(HirPath &path, Module &module) {
	if (auto type = module.scope.get(path)) {
		return *type;
	}

	throw std::runtime_error("no type found");
}

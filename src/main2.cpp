#include <llvm/Bitcode/BitcodeWriter.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Verifier.h>
#include <llvm/Pass.h>
#include <llvm/Passes/PassBuilder.h>
#include <llvm/Support/ErrorHandling.h>
#include <llvm/Support/FileSystem.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/Transforms/Scalar.h>

int main() {
	// Create an LLVM context, module, and IR builder
	llvm::LLVMContext context;
	llvm::Module module("example", context);
	llvm::IRBuilder<> builder(context);

	// Declare the `printf` function
	llvm::FunctionType *printfType = llvm::FunctionType::get(
			builder.getInt32Ty(),									 // Return type (int)
			{builder.getInt8Ty()->getPointerTo()}, // Argument type (char*)
			true																	 // Variadic function
	);
	llvm::FunctionCallee printfFunc =
			module.getOrInsertFunction("printf", printfType);

	// Create the `main` function
	llvm::FunctionType *mainType =
			llvm::FunctionType::get(builder.getInt32Ty(), // Return type (int)
															{},										// No arguments
															false									// Not variadic
			);
	llvm::Function *mainFunc = llvm::Function::Create(
			mainType, llvm::Function::ExternalLinkage, "main", module);

	// Create a basic block for the `main` function
	llvm::BasicBlock *entryBlock =
			llvm::BasicBlock::Create(context, "entry", mainFunc);
	builder.SetInsertPoint(entryBlock);

	// Generate the sum of 10 + 5
	llvm::Value *sum =
			builder.CreateAdd(builder.getInt32(10), builder.getInt32(5), "sum");

	// Create a format string for `printf`
	llvm::Value *formatStr =
			builder.CreateGlobalStringPtr("hello world %d\n", "formatStr");

	// Call `printf` to print the sum
	builder.CreateCall(printfFunc, {formatStr, sum});

	// Return 0 from `main`
	builder.CreateRet(builder.getInt32(0));

	// Verify the module
	if (llvm::verifyModule(module, &llvm::errs())) {
		llvm::errs() << "Error: Module verification failed!\n";
		return 1;
	}

	std::error_code error;
	llvm::raw_fd_ostream file("example.bc", error, llvm::sys::fs::OF_None);

	// Print the optimized IR
	llvm::WriteBitcodeToFile(module, file);
	file.close();

	module.print(llvm::errs(), nullptr);
}

#pragma once

#include <unordered_map>

#include <llvm/Analysis/CGSCCPassManager.h>
#include <llvm/Analysis/LoopAnalysisManager.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Verifier.h>
#include <llvm/MC/TargetRegistry.h>
#include <llvm/Passes/PassBuilder.h>
#include <llvm/Support/CodeGen.h>
#include <llvm/Support/FileSystem.h>
#include <llvm/Support/Process.h>
#include <llvm/Support/Program.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/Target/TargetMachine.h>
#include <llvm/TargetParser/Host.h>
#include <llvm/Transforms/InstCombine/InstCombine.h>
#include <llvm/Transforms/Scalar/Reassociate.h>
#include <llvm/Transforms/Utils/Mem2Reg.h>

#include "mir.h"
#include "token.h"

class LlScope {
public:
	LlScope(llvm::IRBuilder<> &builder, std::shared_ptr<LlScope> parent = nullptr,
					llvm::Function *fn = nullptr)
			: fn(fn), builder(builder), parent(parent) {}

	llvm::Function *fn;

	llvm::Value *get(MirIdent ident) {
		auto alloc = getAlloca(ident);
		auto ty = alloc->getAllocatedType();

		return builder.CreateLoad(ty, alloc);
	}

	llvm::Function *getFn(MirPath path) {
		if (auto it = functions.find(path); it != functions.end()) {
			return it->second;
		}

		if (parent) {
			return parent->getFn(path);
		}

		throw std::runtime_error("function not found");
	}

	void set(MirIdent ident, llvm::Value *value) {
		llvm::IRBuilder<> b(&fn->getEntryBlock(), fn->getEntryBlock().begin());
		auto ptr = b.CreateAlloca(value->getType(), nullptr, ident.value());

		variables[ident] = ptr;
		builder.CreateStore(value, ptr);
	}

	void setAlloca(MirIdent ident, llvm::AllocaInst *alloca) {
		variables[ident] = alloca;
	}

	void setFn(MirPath path, llvm::Function *fn) { functions[path] = fn; }

	llvm::AllocaInst *getAlloca(MirIdent ident) {
		if (auto it = variables.find(ident); it != variables.end()) {
			return it->second;
		}

		if (parent) {
			return parent->getAlloca(ident);
		}

		throw std::runtime_error("variable not found");
	}

private:
	llvm::IRBuilder<> &builder;
	std::shared_ptr<LlScope> parent;

	std::unordered_map<MirIdent, llvm::AllocaInst *> variables;
	std::unordered_map<MirPath, llvm::Function *> functions;
};

class LlTypeCtx {
public:
	LlTypeCtx(TypeCtx &ty, llvm::LLVMContext &ctx, llvm::Module &module)
			: ty(ty), ctx(ctx), dataLayout(module.getDataLayout()) {}

	TypeCtx &ty;

	template <typename T> T *get(TypeHandle handle) {
		if (auto it = types.find(handle); it != types.end()) {
			return static_cast<T *>(it->second);
		}

		auto type = get(ty.get(handle));

		types[handle] = type;

		return static_cast<T *>(type);
	}

	llvm::Type *get(TypeHandle handle) { return get<llvm::Type>(handle); }

private:
	llvm::LLVMContext &ctx;
	llvm::DataLayout dataLayout;

	std::unordered_map<TypeHandle, llvm::Type *> types;

	llvm::Type *get(const MirType &type) {
		return std::visit([&](auto &&arg) { return get(arg); }, type.kind);
	}

	llvm::Type *get(const MirTypeBuiltin &builtin) {
		switch (builtin) {
		case MirTypeBuiltin::Bool:
			return llvm::Type::getInt1Ty(ctx);
		case MirTypeBuiltin::Char:
			return llvm::Type::getInt8Ty(ctx);
		case MirTypeBuiltin::Uint8:
			return llvm::Type::getInt8Ty(ctx);
		case MirTypeBuiltin::Uint16:
			return llvm::Type::getInt16Ty(ctx);
		case MirTypeBuiltin::Uint32:
			return llvm::Type::getInt32Ty(ctx);
		case MirTypeBuiltin::Uint64:
			return llvm::Type::getInt64Ty(ctx);
		case MirTypeBuiltin::Usize:
			return llvm::IntegerType::get(ctx, dataLayout.getPointerSizeInBits());
		case MirTypeBuiltin::Int8:
			return llvm::Type::getInt8Ty(ctx);
		case MirTypeBuiltin::Int16:
			return llvm::Type::getInt16Ty(ctx);
		case MirTypeBuiltin::Int32:
			return llvm::Type::getInt32Ty(ctx);
		case MirTypeBuiltin::Int64:
			return llvm::Type::getInt64Ty(ctx);
		case MirTypeBuiltin::Isize:
			return llvm::IntegerType::get(ctx, dataLayout.getPointerSizeInBits());
		case MirTypeBuiltin::Float16:
			return llvm::Type::getHalfTy(ctx);
		case MirTypeBuiltin::Float32:
			return llvm::Type::getFloatTy(ctx);
		case MirTypeBuiltin::Float64:
			return llvm::Type::getDoubleTy(ctx);
		case MirTypeBuiltin::Void:
			return llvm::Type::getVoidTy(ctx);
		}

		throw std::runtime_error("builtin type not implemented");
	}

	llvm::Type *get(const MirPointer &ptr) { return get(ptr, ptr.refCount); }

	llvm::Type *get(const MirPointer &ptr, std::size_t refCount) {
		if (refCount == 0) {
			return get(ptr.type);
		}

		refCount -= 1;
		llvm::Type *type = get(ptr, refCount);

		return type->getPointerTo();
	}

	llvm::Type *get(const MirStruct &str) {
		std::vector<llvm::Type *> fields;

		for (const MirStructField &field : str.fields) {
			llvm::Type *type = get(field.type);
			fields.push_back(type);
		}

		return llvm::StructType::create(ctx, fields, str.ident.value());
	}

	llvm::Type *get(const MirArray &array) {
		llvm::Type *type = get(array.type);

		return llvm::ArrayType::get(type, array.size);
	}

	llvm::Type *get(const std::shared_ptr<MirFnSignature> &fn) {
		std::vector<llvm::Type *> params;

		for (auto &param : fn->params) {
			llvm::Type *type = get(param.type);
			params.push_back(type);
		}

		llvm::Type *ret = get(fn->ret);

		return llvm::FunctionType::get(ret, params, fn->variadic);
	}
};

class MirLowerer {
public:
	MirLowerer(HirLowerer mir_)
			: context(), module("example", context), mir(std::move(mir_)),
				ctx(mir.ctx, context, module),
				scope(std::make_shared<LlScope>(builder)) {}

	llvm::LLVMContext context;
	llvm::Module module;

	void lower() {
		auto nodes = mir.lower();

		for (const Mir &node : nodes) {
			std::visit([&](auto &&arg) { lower(scope, arg); }, node);
		}

		llvm::Function *mainFunction = module.getFunction("main");
		if (!mainFunction) {
			llvm::errs() << "Error: main() function not found in the module!\n";
			return;
		}
	}

	void compileObjectFile() {
		compileObjectFile(llvm::OptimizationLevel::O3,
											llvm::ThinOrFullLTOPhase::None, false);
	}

	void compileObjectFile(llvm::OptimizationLevel level,
												 llvm::ThinOrFullLTOPhase phase, bool optimize = true) {
		if (llvm::verifyModule(module, &llvm::errs())) {
			llvm::errs() << "Error: Module verification failed!\n";
			return;
		}

		llvm::InitializeNativeTarget();
		llvm::InitializeNativeTargetAsmPrinter();

		std::string targetTriple = llvm::sys::getDefaultTargetTriple();
		module.setTargetTriple(targetTriple);

		std::string error;
		const llvm::Target *target =
				llvm::TargetRegistry::lookupTarget(targetTriple, error);

		if (!target) {
			llvm::errs() << "Error: " << error;
			return;
		}

		llvm::TargetOptions opt;
		llvm::Reloc::Model relocModel = llvm::Reloc::Model::PIC_;
		std::unique_ptr<llvm::TargetMachine> targetMachine =
				std::unique_ptr<llvm::TargetMachine>(target->createTargetMachine(
						targetTriple, "generic", "", opt, relocModel));

		module.setDataLayout(targetMachine->createDataLayout());

		if (optimize) {
			llvm::PassBuilder passBuilder;

			llvm::LoopAnalysisManager loopAnalysisManager;
			llvm::FunctionAnalysisManager functionAnalysisManager;
			llvm::CGSCCAnalysisManager cgsccAnalysisManager;
			llvm::ModuleAnalysisManager moduleAnalysisManager;

			passBuilder.registerModuleAnalyses(moduleAnalysisManager);
			passBuilder.registerCGSCCAnalyses(cgsccAnalysisManager);
			passBuilder.registerFunctionAnalyses(functionAnalysisManager);
			passBuilder.registerLoopAnalyses(loopAnalysisManager);
			passBuilder.crossRegisterProxies(
					loopAnalysisManager, functionAnalysisManager, cgsccAnalysisManager,
					moduleAnalysisManager);

			llvm::ModulePassManager modulePassManager;

			modulePassManager.addPass(
					passBuilder.buildModuleSimplificationPipeline(level, phase));

			llvm::FunctionPassManager functionPassManager;

			functionPassManager.addPass(llvm::PromotePass());
			functionPassManager.addPass(llvm::InstCombinePass());
			functionPassManager.addPass(llvm::ReassociatePass());

			modulePassManager.addPass(llvm::createModuleToFunctionPassAdaptor(
					std::move(functionPassManager)));

			modulePassManager.run(module, moduleAnalysisManager);
		}

		std::error_code ec;

		ec = llvm::sys::fs::create_directory("target");

		if (ec) {
			llvm::errs() << "Error creating directory: " << ec.message();
			return;
		}

		llvm::raw_fd_ostream dest("target/output.o", ec, llvm::sys::fs::OF_None);
		if (ec) {
			llvm::errs() << "Error opening file: " << ec.message();
			return;
		}

		llvm::legacy::PassManager codeGenPassManager;
		if (targetMachine->addPassesToEmitFile(codeGenPassManager, dest, nullptr,
																					 llvm::CodeGenFileType::ObjectFile)) {
			llvm::errs() << "Error: TargetMachine can't emit object file";
			return;
		}

		codeGenPassManager.run(module);
		dest.flush();
	}

	void link() {
		std::string linker;
		std::vector<std::string> args;
		std::string outputBinary = "target/output";

		if (llvm::Triple(llvm::sys::getProcessTriple()).isOSWindows()) {
			linker = "link.exe";
			args = {linker,				"/entry:_start",				"/subsystem:console",
							"msvcrt.lib", "/out:" + outputBinary, "target/output.o"};
		} else if (llvm::Triple(llvm::sys::getProcessTriple()).isMacOSX()) {
			linker = "ld";
			args = {linker,
							"-o",
							outputBinary,
							"target/output.o",
							"-L/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/lib",
							"-lc",
							"-lSystem"};
		} else {
			// Linux
			linker = "/usr/bin/ld";
			args = {linker,
							"-o",
							outputBinary,
							"target/output.o",
							"-L/lib",
							"-L/usr/lib",
							"-lc",
							"/usr/lib/x86_64-linux-gnu/crt1.o",
							"/usr/lib/x86_64-linux-gnu/crti.o",
							"-dynamic-linker",
							"/lib64/ld-linux-x86-64.so.2",
							"/usr/lib/x86_64-linux-gnu/crtn.o"};
		}

		std::vector<llvm::StringRef> execArgs;
		for (const auto &arg : args) {
			execArgs.push_back(arg);
		}

		std::string errMsg;
		int result = llvm::sys::ExecuteAndWait(linker, execArgs, std::nullopt, {},
																					 0, 0, &errMsg);

		if (result != 0) {
			llvm::errs() << "Linking failed: " << errMsg << "\n";
		} else {
			llvm::outs() << "Successfully linked " << outputBinary << "\n";
		}
	}

private:
	llvm::IRBuilder<> builder{context};

	HirLowerer mir;
	LlTypeCtx ctx;
	std::shared_ptr<LlScope> scope;

	void lower(std::shared_ptr<LlScope> &scope, const MirFn &mir) {
		llvm::FunctionType *ty = ctx.get<llvm::FunctionType>(mir.type);

		llvm::Function *fn = llvm::Function::Create(
				ty, llvm::Function::ExternalLinkage, mir.ident.value(), module);

		llvm::BasicBlock *entry = llvm::BasicBlock::Create(context, "entry", fn);
		std::shared_ptr<LlScope> entryScope =
				std::make_shared<LlScope>(builder, scope, fn);

		for (std::size_t i = 0; i < mir.params.size(); i++) {
			llvm::Argument *arg = fn->arg_begin() + i;
			arg->setName(mir.params[i].ident.value());
			entryScope->set(mir.params[i].ident, arg);
		}

		scope->setFn(MirPath{mir.ident}, fn);

		builder.SetInsertPoint(entry);

		for (const MirBlockItem &node : mir.block.items) {
			std::visit([&](auto &&arg) { lower(entryScope, arg); }, node);
		}
	}

	void lower(std::shared_ptr<LlScope> &scope, const MirFnSignature &mir) {
		llvm::FunctionType *ty = ctx.get<llvm::FunctionType>(mir.type);

		llvm::Function *fn = llvm::Function::Create(
				ty, llvm::Function::ExternalLinkage, mir.ident.value(), module);

		for (std::size_t i = 0; i < mir.params.size(); i++) {
			llvm::Argument *arg = fn->arg_begin() + i;
			arg->setName(mir.params[i].ident.value());
		}

		scope->setFn(MirPath{mir.ident}, fn);
	}

	void lower(std::shared_ptr<LlScope> &scope, const MirStruct &mir) {
		ctx.get<llvm::StructType>(mir.type);
	}

	llvm::Value *lower(std::shared_ptr<LlScope> &scope, const MirLit &mir) {
		if (auto it = std::get_if<int>(&mir.value)) {
			return builder.getInt32(*it);
		} else if (auto it = std::get_if<double>(&mir.value)) {
			auto ty = ctx.ty.get(MirPath{"f64"});
			llvm::Type *doubleTy = ctx.get(ty);

			return llvm::ConstantFP::get(doubleTy, *it);
		} else if (auto it = std::get_if<std::string>(&mir.value)) {
			return builder.CreateGlobalStringPtr(*it);
		} else if (auto it = std::get_if<bool>(&mir.value)) {
			return builder.getInt1(*it);
		}

		throw std::runtime_error("lowering given literal not implemented");
	}

	llvm::Value *lower(std::shared_ptr<LlScope> &scope, const MirIdent &mir) {
		return scope->get(mir);
	}

	llvm::Value *lower(std::shared_ptr<LlScope> &scope, const MirFnCall &mir) {
		llvm::Function *fn = scope->getFn(mir.path);

		std::vector<llvm::Value *> args;

		for (const MirExpr &arg : mir.args) {
			args.push_back(lower(scope, arg));
		}

		return builder.CreateCall(fn, args);
	}

	llvm::Value *lower(std::shared_ptr<LlScope> &scope,
										 const std::shared_ptr<MirFnCall> &mir) {
		return lower(scope, *mir);
	}

	llvm::Value *lowerBuiltinBinOp(const MirTypeBuiltin &type, Op op,
																 llvm::Value *lhs, llvm::Value *rhs) {
		switch (type) {
		case MirTypeBuiltin::Float16:
		case MirTypeBuiltin::Float32:
		case MirTypeBuiltin::Float64:
			switch (op) {
			case Op::ADD:
				return builder.CreateFAdd(lhs, rhs);
			case Op::SUB:
				return builder.CreateFSub(lhs, rhs);
			case Op::MUL:
				return builder.CreateFMul(lhs, rhs);
			case Op::DIV:
				return builder.CreateFDiv(lhs, rhs);
			case Op::MOD:
				return builder.CreateFRem(lhs, rhs);
			case Op::POW: {
				llvm::Function *pow = llvm::Intrinsic::getDeclaration(
						&module, llvm::Intrinsic::pow, {lhs->getType()});

				return builder.CreateCall(pow, {lhs, rhs});
			}
			case Op::EQEQ:
				return builder.CreateFCmpOEQ(lhs, rhs);
			case Op::NEQ:
				return builder.CreateFCmpONE(lhs, rhs);
			case Op::LT:
				return builder.CreateFCmpOLT(lhs, rhs);
			case Op::GT:
				return builder.CreateFCmpOGT(lhs, rhs);
			default:
				throw std::runtime_error(
						fmt::format("unimplemented binop for float: {}", op));
			}
		case MirTypeBuiltin::Int8:
		case MirTypeBuiltin::Int16:
		case MirTypeBuiltin::Int32:
		case MirTypeBuiltin::Int64:
			switch (op) {
			case Op::ADD:
				return builder.CreateAdd(lhs, rhs);
			case Op::SUB:
				return builder.CreateSub(lhs, rhs);
			case Op::MUL:
				return builder.CreateMul(lhs, rhs);
			case Op::DIV:
				return builder.CreateSDiv(lhs, rhs);
			case Op::MOD:
				return builder.CreateSRem(lhs, rhs);
			case Op::POW: {
				llvm::Function *pow = llvm::Intrinsic::getDeclaration(
						&module, llvm::Intrinsic::pow, {lhs->getType()});

				return builder.CreateCall(pow, {lhs, rhs});
			}
			case Op::EQEQ:
				return builder.CreateICmpEQ(lhs, rhs);
			case Op::NEQ:
				return builder.CreateICmpNE(lhs, rhs);
			case Op::LT:
				return builder.CreateICmpSLT(lhs, rhs);
			case Op::GT:
				return builder.CreateICmpSGT(lhs, rhs);
			case Op::LTE:
				return builder.CreateICmpSLE(lhs, rhs);
			case Op::GTE:
				return builder.CreateICmpSGE(lhs, rhs);
			case Op::BIT_AND:
				return builder.CreateAnd(lhs, rhs);
			case Op::BIT_OR:
				return builder.CreateOr(lhs, rhs);
			case Op::BIT_XOR:
				return builder.CreateXor(lhs, rhs);
			case Op::BIT_LSHIFT:
				return builder.CreateShl(lhs, rhs);
			case Op::BIT_RSHIFT:
				return builder.CreateLShr(lhs, rhs);
			default:
				throw std::runtime_error(
						fmt::format("unimplemented binop for int: {}", op));
			}
		case MirTypeBuiltin::Uint8:
		case MirTypeBuiltin::Uint16:
		case MirTypeBuiltin::Uint32:
		case MirTypeBuiltin::Uint64:
			switch (op) {
			case Op::ADD:
				return builder.CreateAdd(lhs, rhs);
			case Op::SUB:
				return builder.CreateSub(lhs, rhs);
			case Op::MUL:
				return builder.CreateMul(lhs, rhs);
			case Op::DIV:
				return builder.CreateUDiv(lhs, rhs);
			case Op::MOD:
				return builder.CreateURem(lhs, rhs);
			case Op::POW: {
				llvm::Function *pow = llvm::Intrinsic::getDeclaration(
						&module, llvm::Intrinsic::pow, {lhs->getType()});

				return builder.CreateCall(pow, {lhs, rhs});
			}
			case Op::EQEQ:
				return builder.CreateICmpEQ(lhs, rhs);
			case Op::NEQ:
				return builder.CreateICmpNE(lhs, rhs);
			case Op::LT:
				return builder.CreateICmpULT(lhs, rhs);
			case Op::GT:
				return builder.CreateICmpUGT(lhs, rhs);
			case Op::LTE:
				return builder.CreateICmpULE(lhs, rhs);
			case Op::GTE:
				return builder.CreateICmpUGE(lhs, rhs);
			case Op::BIT_AND:
				return builder.CreateAnd(lhs, rhs);
			case Op::BIT_OR:
				return builder.CreateOr(lhs, rhs);
			case Op::BIT_XOR:
				return builder.CreateXor(lhs, rhs);
			case Op::BIT_LSHIFT:
				return builder.CreateShl(lhs, rhs);
			case Op::BIT_RSHIFT:
				return builder.CreateLShr(lhs, rhs);
			default:
				throw std::runtime_error(
						fmt::format("unimplemented binop for uint: {}", op));
			}
		case MirTypeBuiltin::Bool:
			switch (op) {
			case Op::AND:
				return builder.CreateLogicalAnd(lhs, rhs);
			case Op::OR:
				return builder.CreateLogicalOr(lhs, rhs);
			case Op::EQEQ:
				return builder.CreateICmpEQ(lhs, rhs);
			case Op::NEQ:
				return builder.CreateICmpNE(lhs, rhs);
			default:
				throw std::runtime_error(
						fmt::format("unimplemented binop for bool: {}", op));
			}
		default:
			throw std::runtime_error("not implemented");
		}

		throw std::runtime_error("not implemented");
	}

	llvm::Value *lowerBinOp(const MirTypeKind &kind, Op op, llvm::Value *lhs,
													llvm::Value *rhs) {

		if (auto it = std::get_if<MirTypeBuiltin>(&kind); it) {
			return lowerBuiltinBinOp(*it, op, lhs, rhs);
		}

		if (auto it = std::get_if<MirPointer>(&kind); it) {
			switch (op) {
			case Op::ADD:
				return builder.CreateAdd(lhs, rhs);
			case Op::SUB:
				return builder.CreateSub(lhs, rhs);
			case Op::EQEQ:
				return builder.CreateICmpEQ(lhs, rhs);
			case Op::NEQ:
				return builder.CreateICmpNE(lhs, rhs);
			default:
				throw std::runtime_error(
						fmt::format("unimplemented binop for pointer: {}", op));
			}
		}

		throw std::runtime_error("not implemented");
	}

	llvm::Value *lowerBuiltinUnOp(const MirTypeBuiltin &type, Op op,
																llvm::Value *value) {
		switch (type) {
		case MirTypeBuiltin::Float16:
		case MirTypeBuiltin::Float32:
		case MirTypeBuiltin::Float64:
			switch (op) {
			case Op::SUB:
				return builder.CreateFNeg(value);
			default:
				throw std::runtime_error(
						fmt::format("unimplemented unop for float: {}", op));
			}
		case MirTypeBuiltin::Int8:
		case MirTypeBuiltin::Int16:
		case MirTypeBuiltin::Int32:
		case MirTypeBuiltin::Int64:
			switch (op) {
			case Op::SUB:
				return builder.CreateNeg(value);
			case Op::BIT_NOT:
				return builder.CreateNot(value);
			default:
				throw std::runtime_error(
						fmt::format("unimplemented unop for int: {}", op));
			}
		case MirTypeBuiltin::Uint8:
		case MirTypeBuiltin::Uint16:
		case MirTypeBuiltin::Uint32:
		case MirTypeBuiltin::Uint64:
			switch (op) {
			case Op::SUB:
				return builder.CreateNeg(value);
			case Op::BIT_NOT:
				return builder.CreateNot(value);
			default:
				throw std::runtime_error(
						fmt::format("unimplemented unop for uint: {}", op));
			}
		case MirTypeBuiltin::Bool:
			switch (op) {
			case Op::NOT:
				return builder.CreateNot(value);
			default:
				throw std::runtime_error(
						fmt::format("unimplemented unop for bool: {}", op));
			}
		default:
			throw std::runtime_error("not implemented");
		}

		return nullptr;
	}

	llvm::Value *lowerUnOp(const MirTypeKind &type, Op op, llvm::Value *value) {
		// check for pointer
		if (op == Op::BIT_AND /* ref */) {
			// TODO: array
			llvm::Value *out = builder.CreateAlloca(value->getType());
			builder.CreateStore(value, out);

			return out;
		} else if (op == Op::MUL /* deref */) {
			auto ptrType = std::get<MirPointer>(type);
			ptrType.refCount -= 1;
			auto path = ctx.ty.getPath(ptrType.type);
			path.refCount = ptrType.refCount;
			auto ty = ctx.ty.get(path);
			auto inner = ctx.get(ty);

			return builder.CreateLoad(inner, value);
		}

		if (auto it = std::get_if<MirTypeBuiltin>(&type); it) {
			return lowerBuiltinUnOp(*it, op, value);
		}

		throw std::runtime_error("not implemented");
	}

	llvm::Value *lower(std::shared_ptr<LlScope> &scope,
										 const MirStructInstance &mir) {
		llvm::Type *ty = ctx.get(mir.type);
		llvm::AllocaInst *str = builder.CreateAlloca(ty);

		for (std::size_t i = 0; i < mir.fields.size(); i++) {
			llvm::Value *value = lower(scope, mir.fields[i].expr);
			llvm::Value *ptr = builder.CreateStructGEP(ty, str, i);
			builder.CreateStore(value, ptr);
		}

		return str;
	}

	llvm::Value *lower(std::shared_ptr<LlScope> &scope,
										 const std::shared_ptr<MirStructInstance> &mir) {
		return lower(scope, *mir);
	}

	llvm::Value *lower(std::shared_ptr<LlScope> &scope, const MirBinOp &mir) {
		llvm::Value *lhs = lower(scope, mir.lhs);
		llvm::Value *rhs = lower(scope, mir.rhs);

		if (mir.lhs.type != mir.rhs.type) {
			throw std::runtime_error("type mismatch");
		}

		// both the same type
		const MirTypeKind &kind = ctx.ty.get(mir.lhs.type).kind;

		return lowerBinOp(kind, mir.op.variant, lhs, rhs);
	}

	llvm::Value *lower(std::shared_ptr<LlScope> &scope,
										 const std::shared_ptr<MirBinOp> &mir) {
		return lower(scope, *mir);
	}

	llvm::Value *lower(std::shared_ptr<LlScope> &scope, const MirUnOp &mir) {
		llvm::Value *value = lower(scope, mir.expr);

		const MirTypeKind &kind = ctx.ty.get(mir.expr.type).kind;

		return lowerUnOp(kind, mir.op.variant, value);
	}

	llvm::Value *lower(std::shared_ptr<LlScope> &scope,
										 const std::shared_ptr<MirUnOp> &mir) {
		return lower(scope, *mir);
	}

	llvm::Value *lowerToAlloc(std::shared_ptr<LlScope> &scope,
														const MirStructPath &mir) {
		llvm::Value *alloc = scope->getAlloca(mir.ident);
		auto ty = mir.structType;

		for (std::size_t idx : mir.indices) {
			auto structTy = std::get<MirStruct>(ctx.ty.get(ty).kind);
			auto llvmTy = ctx.get(ty);

			alloc = builder.CreateStructGEP(llvmTy, alloc, idx);
			ty = structTy.fields[idx].type;
		}

		return alloc;
	}

	llvm::Value *lower(std::shared_ptr<LlScope> &scope,
										 const MirStructPath &mir) {
		llvm::Value *alloc = scope->getAlloca(mir.ident);
		auto ty = mir.structType;

		for (std::size_t idx : mir.indices) {
			auto structTy = std::get<MirStruct>(ctx.ty.get(ty).kind);
			auto llvmTy = ctx.get(ty);

			alloc = builder.CreateStructGEP(llvmTy, alloc, idx);
			ty = structTy.fields[idx].type;

			fmt::print("getting value: {}\n", idx);
		}

		return builder.CreateLoad(ctx.get(ty), alloc, mir.ident.value());
	}

	llvm::Value *lower(std::shared_ptr<LlScope> &scope, const MirExpr &mir) {
		return std::visit([&](auto &&arg) { return lower(scope, arg); }, mir.expr);
	}

	void lower(std::shared_ptr<LlScope> &scope, const MirLoop &mir) {
		if (scope->fn == nullptr) {
			throw std::runtime_error("no block");
		}

		std::shared_ptr<LlScope> loopScope =
				std::make_shared<LlScope>(builder, scope, scope->fn);

		llvm::BasicBlock *setupBlock =
				llvm::BasicBlock::Create(context, "loop_setup", scope->fn);
		llvm::BasicBlock *condBlock =
				llvm::BasicBlock::Create(context, "loop_cond", scope->fn);
		llvm::BasicBlock *bodyBlock =
				llvm::BasicBlock::Create(context, "loop_body", scope->fn);
		llvm::BasicBlock *stepBlock =
				llvm::BasicBlock::Create(context, "loop_step", scope->fn);
		llvm::BasicBlock *mergeBlock =
				llvm::BasicBlock::Create(context, "loop_merge", scope->fn);

		builder.CreateBr(setupBlock);
		builder.SetInsertPoint(setupBlock);

		if (auto &s = mir.setup) {
			lower(loopScope, *s);
		}

		builder.CreateBr(condBlock);
		builder.SetInsertPoint(condBlock);

		if (std::optional<MirExpr> c = mir.cond) {
			llvm::Value *cond = lower(loopScope, *c);
			builder.CreateCondBr(cond, bodyBlock, mergeBlock);
		} else {
			builder.CreateBr(mergeBlock);
		}

		builder.SetInsertPoint(stepBlock);

		if (auto &u = mir.step) {
			lower(loopScope, *u);
		}

		builder.CreateBr(condBlock);

		std::shared_ptr<LlScope> loopBodyScope =
				std::make_shared<LlScope>(builder, loopScope, scope->fn);

		builder.SetInsertPoint(bodyBlock);

		for (auto &node : mir.block.items) {
			lower(loopScope, node);
		}

		builder.CreateBr(stepBlock);
		builder.SetInsertPoint(mergeBlock);
	}

	void lower(std::shared_ptr<LlScope> &scope, const MirIf &mir) {
		if (scope->fn == nullptr) {
			throw std::runtime_error("no block");
		}

		llvm::BasicBlock *bodyBlock =
				llvm::BasicBlock::Create(context, "if_body", scope->fn);

		llvm::BasicBlock *mergeBlock =
				llvm::BasicBlock::Create(context, "if_merge", scope->fn);

		llvm::BasicBlock *elseBlock =
				llvm::BasicBlock::Create(context, "if_else", scope->fn);

		std::shared_ptr<LlScope> ifScope =
				std::make_shared<LlScope>(builder, scope, scope->fn);

		llvm::Value *cond = lower(ifScope, mir.cond);

		builder.CreateCondBr(cond, bodyBlock, elseBlock);
		builder.SetInsertPoint(bodyBlock);

		for (auto &node : mir.block.items) {
			lower(ifScope, node);
		}

		builder.CreateBr(mergeBlock);
		builder.SetInsertPoint(elseBlock);

		for (auto &node : mir.else_) {
			lower(ifScope, mergeBlock, node);
		}

		if (mir.else_.empty()) {
			builder.CreateBr(mergeBlock);
		}

		builder.SetInsertPoint(mergeBlock);
	}

	void lower(std::shared_ptr<LlScope> &scope, llvm::BasicBlock *mergeBlock,
						 const MirElse &mir) {
		llvm::BasicBlock *bodyBlock =
				llvm::BasicBlock::Create(context, "else_body", scope->fn);

		if (auto c = mir.cond) {
			llvm::Value *cond = lower(scope, *c);

			builder.CreateCondBr(cond, bodyBlock, mergeBlock);
		} else {
			builder.CreateBr(bodyBlock);
		}

		builder.SetInsertPoint(bodyBlock);

		for (auto &node : mir.block.items) {
			lower(scope, node);
		}

		builder.CreateBr(mergeBlock);
	}

	void lower(std::shared_ptr<LlScope> &scope, const MirAssign &mir) {
		llvm::Value *value = lower(scope, mir.expr);

		if (std::holds_alternative<std::shared_ptr<MirStructInstance>>(
						mir.expr.expr)) {
			scope->setAlloca(mir.ident, static_cast<llvm::AllocaInst *>(value));
		} else {
			scope->set(mir.ident, value);
		}
	}

	void lower(std::shared_ptr<LlScope> &scope, const MirReassign &mir) {
		llvm::Value *value = lower(scope, mir.expr);
		llvm::Value *alloc = lowerToAlloc(scope, mir.path);

		// TODO: fix for structs?
		builder.CreateStore(value, alloc);
	}

	void lower(std::shared_ptr<LlScope> &scope, const MirReturn &mir) {
		if (mir.expr) {
			llvm::Value *value = lower(scope, *mir.expr);
			builder.CreateRet(value);
		} else {
			builder.CreateRetVoid();
		}
	}

	void lower(std::shared_ptr<LlScope> &scope,
						 const std::shared_ptr<MirLoop> &mir) {
		lower(scope, *mir);
	}

	void lower(std::shared_ptr<LlScope> &scope,
						 const std::shared_ptr<MirIf> &mir) {
		lower(scope, *mir);
	}

	void lower(std::shared_ptr<LlScope> &scope, const MirStmt &mir) {
		std::visit([&](auto &&arg) { lower(scope, arg); }, mir);
	}

	void lower(std::shared_ptr<LlScope> &scope, const MirBlockItem &mir) {
		std::visit([&](auto &&arg) { lower(scope, arg); }, mir);
	}
};

#pragma once

#include <filesystem>
#include <memory>
#include <optional>
#include <unordered_map>
#include <vector>

#include <llvm/IR/Type.h>

class Scope {
public:
	void add(MirIdent ident, TypeHandle type) { variables.emplace(ident, type); }

private:
	std::unordered_map<MirIdent, TypeHandle> variables;
};

class Module {
public:
	Module(TypeCtx &ctx, MirPath path, std::vector<Mir> mir)
			: path(path), mir(mir), ctx(ctx) {}

	void addVariable(MirIdent ident) {}

	MirPath path;
	std::vector<Mir> mir;

	std::unordered_map<MirIdent, Mir> functions;
	Scope scope;

	TypeCtx &ctx;
};

// index.owl
// hello.owl
//
//
// index.owl:
// import hello; -> "hello.owl"
// import hello::world; -> "hello/world.owl"
//
// each import is a module, and each module contains its own types.
// every symbol must be defined within the module (or through more imports)

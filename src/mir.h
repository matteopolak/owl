#include <string>
#include <unordered_map>
#include <variant>
#include <vector>

#include "hir.h"
#include "span.h"
#include "token.h"

class TypeHandle {
public:
	bool operator==(const TypeHandle &other) const {
		return index == other.index;
	}

private:
	std::size_t index;

	TypeHandle(std::size_t index) : index(index) {}

	friend class TypeCtx;
};

enum class MirTypeBuiltin {
	Bool,
	Char,

	Uint8,
	Uint16,
	Uint32,
	Uint64,
	Usize,

	Int8,
	Int16,
	Int32,
	Int64,
	Isize,

	Float16,
	Float32,
	Float64,

	Void,
};

class MirIdent {
public:
	MirIdent(std::string ident) : ident(ident) {}

	bool operator==(const MirIdent &other) const { return ident == other.ident; }

	static MirIdent from_hir(TokenIdent ident) { return MirIdent{ident.value()}; }

	std::string value() const { return ident; }

private:
	std::string ident;
};

class Scope {
public:
	Scope(std::shared_ptr<Scope> parent) : parent(parent) {}

	void add(MirIdent ident, TypeHandle type) { variables.emplace(ident, type); }

	TypeHandle get(MirIdent ident) const {
		if (auto it = variables.find(ident); it != variables.end()) {
			return it->second;
		}

		if (parent) {
			return parent->get()->get(ident);
		}

		throw std::runtime_error("variable not found");
	}

private:
	std::optional<std::shared_ptr<Scope>> parent;
	std::unordered_map<MirIdent, TypeHandle> variables;
};

class MirPath {
public:
	MirPath(MirIdent ident) : parts{ident} {}
	MirPath(std::string ident) : parts{MirIdent{ident}} {}
	MirPath() = default;

	bool operator==(const MirPath &other) const { return parts == other.parts; }

	static MirPath empty() { return MirPath{}; }

	static MirPath from_hir(HirPath hir) {
		MirPath path{MirIdent{hir.parts[0].value()}};

		for (std::size_t i = 1; i < hir.parts.size(); i++) {
			path.parts.push_back(MirIdent{hir.parts[i].value()});
		}

		return path;
	}

	MirPath operator+(const MirPath &other) {
		MirPath path = *this;

		for (auto &part : other.parts) {
			if (part.value() == "super") {
				path.parts.pop_back();
			} else {
				path.parts.push_back(part);
			}
		}

		return path;
	}

private:
	std::vector<MirIdent> parts;
};

class TypeCtx;

class MirStructField {
public:
	MirStructField(MirIdent ident, TypeHandle type) : ident(ident), type(type) {}

	static MirStructField from_hir(TypeCtx &ctx, HirTypedIdent hir);

	MirIdent ident;
	TypeHandle type;
};

class MirStruct {
public:
	MirStruct(MirIdent ident, std::vector<MirStructField> fields)
			: ident(ident), fields(fields) {}

	MirIdent ident;
	std::vector<MirStructField> fields;

	static MirStruct from_hir(TypeCtx &ctx, HirStruct hir);
};

class MirArray {
public:
	MirArray(TypeHandle type, std::size_t size) : type(type), size(size) {}

	TypeHandle type;
	std::size_t size;
};

using MirTypeKind = std::variant<MirTypeBuiltin, MirStruct, MirArray>;

class MirType {
public:
	MirType(MirTypeKind kind) : kind(kind) {}

	MirTypeKind kind;
};

class TypeCtx {
public:
	std::shared_ptr<Scope> scope;

	TypeCtx() {
		add(MirPath{"bool"}, MirType{MirTypeBuiltin::Bool});
		add(MirPath{"char"}, MirType{MirTypeBuiltin::Char});

		add(MirPath{"u8"}, MirType{MirTypeBuiltin::Uint8});
		add(MirPath{"u16"}, MirType{MirTypeBuiltin::Uint16});
		add(MirPath{"u32"}, MirType{MirTypeBuiltin::Uint32});
		add(MirPath{"u64"}, MirType{MirTypeBuiltin::Uint64});
		add(MirPath{"usize"}, MirType{MirTypeBuiltin::Usize});

		add(MirPath{"i8"}, MirType{MirTypeBuiltin::Int8});
		add(MirPath{"i16"}, MirType{MirTypeBuiltin::Int16});
		add(MirPath{"i32"}, MirType{MirTypeBuiltin::Int32});
		add(MirPath{"i64"}, MirType{MirTypeBuiltin::Int64});
		add(MirPath{"isize"}, MirType{MirTypeBuiltin::Isize});

		add(MirPath{"f16"}, MirType{MirTypeBuiltin::Float16});
		add(MirPath{"f32"}, MirType{MirTypeBuiltin::Float32});
		add(MirPath{"f64"}, MirType{MirTypeBuiltin::Float64});

		add(MirPath{"void"}, MirType{MirTypeBuiltin::Void});
	}

	// takes an absolute path from the root of the program
	TypeHandle add(MirPath path, MirType type) {
		if (auto it = handles.find(path); it != handles.end()) {
			return it->second;
		}

		auto handle = TypeHandle{handles.size()};

		symbols.emplace(handle, type);
		handles.emplace(path, handle);

		return handle;
	}

	TypeHandle get(MirPath path) {
		path = anchor + path;

		if (auto it = handles.find(path); it != handles.end()) {
			return it->second;
		}

		throw std::runtime_error("type not found");
	}

	// sets the current MirPath root (all `add` operations will be relative to it)
	void setAnchor(MirPath path) { anchor = path; }
	void resetAnchor() { anchor = MirPath::empty(); }

private:
	std::unordered_map<MirPath, TypeHandle> handles;
	std::unordered_map<TypeHandle, MirType> symbols;
	MirPath anchor;
};

MirStructField MirStructField::from_hir(TypeCtx &ctx, HirTypedIdent hir) {
	auto path = MirIdent::from_hir(hir.ident);
	auto type = ctx.get(path);

	return MirStructField{path, type};
}

MirStruct MirStruct::from_hir(TypeCtx &ctx, HirStruct hir) {
	std::vector<MirStructField> fields;

	for (auto &field : hir.fields) {
		auto path = MirPath::from_hir(*field.type);
		auto type = ctx.get(path);
		auto ident = MirIdent::from_hir(field.ident);

		fields.push_back(MirStructField{ident, type});
	}

	auto ident = MirIdent::from_hir(hir.ident);

	return MirStruct{ident, fields};
}

class MirFnParam {
public:
	MirFnParam(MirIdent ident, TypeHandle type) : ident(ident), type(type) {}

	MirIdent ident;
	TypeHandle type;

	static MirFnParam from_hir(TypeCtx &ctx, HirTypedIdent hir) {
		auto path = MirIdent::from_hir(hir.ident);
		auto type = ctx.get(path);

		return MirFnParam{path, type};
	}
};

class MirFn {
public:
	MirFn(MirIdent ident, std::vector<MirFnParam> params, TypeHandle ret)
			: ident(ident), params(params), ret(ret) {}

	MirIdent ident;
	std::vector<MirFnParam> params;
	TypeHandle ret;

	static MirFn from_hir(TypeCtx &ctx, HirFn hir) {
		Scope scope;
		std::vector<MirFnParam> params;

		for (auto &param : hir.params) {
			params.push_back(MirFnParam::from_hir(ctx, param));
		}

		auto ident = MirIdent::from_hir(hir.ident);
		auto ret = ctx.get(MirPath::from_hir(
				hir.ret.value_or(HirPath{Span{}, {TokenIdent{Span{}, "void"}}})));

		return MirFn{ident, params, ret};
	}
};

class MirLit {
public:
	TokenLitType value;

	MirLit(TokenLit value) : value(value.value) {}

	static MirLit from_hir(TokenLit hir) { return MirLit{hir}; }
};

class MirBinOp;
class MirUnOp;
class MirFnCall;

using MirExprItem =
		std::variant<MirLit, MirIdent, MirFnCall, std::shared_ptr<MirBinOp>,
								 std::shared_ptr<MirUnOp>>;

class MirExpr {
public:
	MirExprItem expr;
	TypeHandle type;

	static MirExpr from_hir(TypeCtx &ctx, HirExpr hir);
};

class MirBinOp {
public:
	MirBinOp(MirExpr lhs, MirExpr rhs, TokenOp op) : lhs(lhs), rhs(rhs), op(op) {}

	MirExpr lhs;
	MirExpr rhs;
	TokenOp op;
};

class MirUnOp {
public:
	MirUnOp(MirExpr expr, TokenOp op) : expr(expr), op(op) {}

	MirExpr expr;
	TokenOp op;
};

class MirFnCall {
public:
	MirFnCall(MirPath path, std::vector<MirExpr> args) : path(path), args(args) {}

	MirPath path;
	std::vector<MirExpr> args;
};

MirExpr MirExpr::from_hir(TypeCtx &ctx, HirExpr hir) {
	throw std::runtime_error("not implemented");
}

class MirAssign {
public:
	MirAssign(MirIdent ident, MirExpr expr) : ident(ident), expr(expr) {}

	MirIdent ident;
	MirExpr expr;
};

class MirReassign {
public:
	MirReassign(MirIdent ident, MirExpr expr) : ident(ident), expr(expr) {}

	MirIdent ident;
	MirExpr expr;
};

class MirReturn {
public:
	MirReturn(std::optional<MirExpr> expr) : expr(expr) {}

	std::optional<MirExpr> expr;
};

using MirStmt = std::variant<MirAssign, MirReassign, MirReturn, MirFnCall>;

class MirLoop;
class MirIf;

using MirBlockItem =
		std::variant<MirAssign, MirReassign, MirReturn, MirFnCall,
								 std::shared_ptr<MirLoop>, std::shared_ptr<MirIf>>;

class MirBlock {
public:
	MirBlock(std::vector<MirBlockItem> items) : items(items) {}

	std::vector<MirBlockItem> items;

	static MirBlock from_hir(TypeCtx &ctx, HirBlock hir) {
		std::vector<MirBlockItem> items;

		for (auto &hir : hir.stmts) {
			items.push_back(
					std::visit([ctx](auto &&hir) { return lower(ctx, hir); }, hir));
		}

		return MirBlock{items};
	}
};

class MirIf {
public:
	MirIf(MirExpr cond, MirBlock block) : cond(cond), block(block) {}

	MirExpr cond;
	MirBlock block;
	std::optional<std::unique_ptr<MirIf>> else_;
};

class MirLoop {
public:
	MirLoop(MirBlock block) : block(block) {}

	std::optional<MirStmt> setup;
	std::optional<MirExpr> cond;
	std::optional<MirStmt> step;
	MirBlock block;

	static MirLoop from_hir(TypeCtx &ctx, HirFor hir) {}

	static MirLoop from_hir(TypeCtx &ctx, HirWhile hir) {}

	static MirLoop from_hir(TypeCtx &ctx, HirLoop hir) {
		auto block = MirBlock::from_hir(ctx, hir.block);

		return MirLoop{block};
	}
};

using Mir = std::variant<MirFn, MirStruct, MirLit, MirExpr, MirLoop, MirIf,
												 MirAssign, MirReassign, MirReturn>;

class HirLowerer {
public:
	HirLowerer(std::vector<Hir> hir) : hir(hir) {}

	std::vector<Mir> lower() {
		auto mainScope = ctx.scope;
		std::vector<Mir> mir;

		for (auto &hir : hir) {
			ctx.resetAnchor();
			ctx.scope = mainScope;
			mir.push_back(std::visit([this](auto &&hir) { return lower(hir); }, hir));
		}
	}

	Mir lower(HirFn hir) { return MirFn::from_hir(ctx, hir); }
	Mir lower(HirStruct hir) { throw std::runtime_error("not implemented"); }
	Mir lower(HirExpr hir) { throw std::runtime_error("not implemented"); }
	Mir lower(HirLoop hir) { throw std::runtime_error("not implemented"); }
	Mir lower(HirIf hir) { throw std::runtime_error("not implemented"); }
	Mir lower(HirFor hir) { throw std::runtime_error("not implemented"); }
	Mir lower(HirWhile hir) { throw std::runtime_error("not implemented"); }
	Mir lower(HirConst hir) { throw std::runtime_error("not implemented"); }
	Mir lower(HirAssign hir) { throw std::runtime_error("not implemented"); }
	Mir lower(HirReassign hir) { throw std::runtime_error("not implemented"); }
	Mir lower(HirReturn hir) { throw std::runtime_error("not implemented"); }
	Mir lower(HirImport hir) { throw std::runtime_error("not implemented"); }
	Mir lower(HirExtern hir) { throw std::runtime_error("not implemented"); }

private:
	std::vector<Hir> hir;
	TypeCtx ctx;
};

/*
using Hir = std::variant<HirExpr, HirAssign, HirReassign, HirReturn, HirFn,
												 HirStruct, HirWhile, HirIf, HirFor, HirImport,
												 HirExtern, HirConst, HirLoop>;
*/

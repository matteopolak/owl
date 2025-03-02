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

	std::size_t hash() const { return index; }

private:
	std::size_t index;

	TypeHandle(std::size_t index) : index(index) {}

	friend class TypeCtx;
};

template <> struct std::hash<TypeHandle> {
	std::size_t operator()(const TypeHandle &handle) const {
		return handle.hash();
	}
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

	// hash
	std::size_t hash() const { return std::hash<std::string>{}(ident); }

private:
	std::string ident;
};

template <> struct std::hash<MirIdent> {
	std::size_t operator()(const MirIdent &ident) const { return ident.hash(); }
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
	std::size_t hash() const {
		std::size_t h = 0;

		for (auto &part : parts) {
			h ^= part.hash();
		}

		return h;
	}

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

template <> struct std::hash<MirPath> {
	std::size_t operator()(const MirPath &path) const { return path.hash(); }
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

class MirFn;

using MirTypeKind =
		std::variant<MirTypeBuiltin, MirStruct, MirArray, std::shared_ptr<MirFn>>;

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

	MirType &get(TypeHandle handle) {
		if (auto it = symbols.find(handle); it != symbols.end()) {
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

class MirLit {
public:
	TokenLitType value;

	MirLit(TokenLit value) : value(value.value) {}

	static MirLit from_hir(TokenLit hir) { return MirLit{hir}; }

	TypeHandle type(TypeCtx &ctx) const {
		if (std::holds_alternative<int>(value)) {
			return ctx.get(MirPath{"i32"});
		} else if (std::holds_alternative<double>(value)) {
			return ctx.get(MirPath{"f64"});
		} else if (std::holds_alternative<std::string>(value)) {
			return ctx.get(MirPath{"string"});
		}

		throw std::runtime_error("not implemented");
	}
};

class MirBinOp;
class MirUnOp;
class MirFnCall;

using MirExprItem =
		std::variant<MirLit, MirIdent, std::shared_ptr<MirFnCall>,
								 std::shared_ptr<MirBinOp>, std::shared_ptr<MirUnOp>>;

class MirExpr {
public:
	MirExprItem expr;
	TypeHandle type;

	static MirExpr from_hir(TypeCtx &ctx, HirExpr hir);

	static MirExpr from_hir(TypeCtx &ctx, TokenLit hir) {
		auto lit = MirLit::from_hir(hir);

		return MirExpr{lit, lit.type(ctx)};
	}

	static MirExpr from_hir(TypeCtx &ctx, TokenIdent hir) {
		auto ident = MirIdent::from_hir(hir);
		auto type = ctx.scope->get(ident);

		return MirExpr{ident, type};
	}

	static MirExpr from_hir(TypeCtx &ctx, std::shared_ptr<HirFnCall> hir);

	static MirExpr from_hir(TypeCtx &ctx, std::shared_ptr<HirBinOp> hir) {
		auto lhs = MirExpr::from_hir(ctx, hir->lhs);
		auto rhs = MirExpr::from_hir(ctx, hir->rhs);

		if (lhs.type != rhs.type) {
			throw std::runtime_error("type mismatch");
		}

		return MirExpr{std::make_shared<MirBinOp>(lhs, rhs, hir->op), lhs.type};
	}

	static MirExpr from_hir(TypeCtx &ctx, std::shared_ptr<HirUnOp> hir) {
		auto expr = MirExpr::from_hir(ctx, hir->expr);

		return MirExpr{std::make_shared<MirUnOp>(expr, hir->op), expr.type};
	}
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

	static MirFnCall from_hir(TypeCtx &ctx, HirFnCall hir) {
		auto path = MirPath::from_hir(hir.path);

		std::vector<MirExpr> args;

		for (auto &arg : hir.args) {
			args.push_back(MirExpr::from_hir(ctx, arg));
		}

		return MirFnCall{path, args};
	}
};

class MirAssign {
public:
	MirAssign(MirIdent ident, MirExpr expr) : ident(ident), expr(expr) {}

	MirIdent ident;
	MirExpr expr;

	static MirAssign from_hir(TypeCtx &ctx, HirAssign hir) {
		auto path = MirIdent::from_hir(hir.ident.ident);

		if (!hir.ident.type) {
			throw std::runtime_error("not implemented. please specify type");
		}

		auto typePath = MirPath::from_hir(*hir.ident.type);
		auto type = ctx.get(typePath);
		auto expr = MirExpr::from_hir(ctx, hir.expr);

		if (expr.type != type) {
			throw std::runtime_error("type mismatch");
		}

		return MirAssign{path, expr};
	}
};

class MirReassign {
public:
	MirReassign(MirIdent ident, MirExpr expr) : ident(ident), expr(expr) {}

	MirIdent ident;
	MirExpr expr;

	static MirReassign from_hir(TypeCtx &ctx, HirReassign hir) {
		auto path = MirIdent::from_hir(hir.ident);
		auto expr = MirExpr::from_hir(ctx, hir.expr);

		if (ctx.scope->get(path) != expr.type) {
			throw std::runtime_error("type mismatch");
		}

		return MirReassign{path, expr};
	}
};

class MirReturn {
public:
	MirReturn(std::optional<MirExpr> expr) : expr(expr) {}

	std::optional<MirExpr> expr;

	static MirReturn from_hir(TypeCtx &ctx, HirReturn hir) {
		std::optional<MirExpr> expr;

		if (hir.expr) {
			expr = MirExpr::from_hir(ctx, *hir.expr);
		}

		return MirReturn{expr};
	}
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
	MirBlock() = default;

	std::vector<MirBlockItem> items;

	static MirBlock from_hir(TypeCtx &ctx, HirBlock hir) {
		std::vector<MirBlockItem> items;

		for (auto &hir : hir.stmts) {
			items.push_back(std::visit(
					[&ctx](auto &&hir) { return MirBlock::lower(ctx, hir); }, hir.stmt));
		}

		return MirBlock{items};
	}

	static MirBlockItem lower(TypeCtx &ctx, HirAssign hir);
	static MirBlockItem lower(TypeCtx &ctx, HirReassign hir);
	static MirBlockItem lower(TypeCtx &ctx, HirReturn hir);
	static MirBlockItem lower(TypeCtx &ctx, HirFnCall hir);
	static MirBlockItem lower(TypeCtx &ctx, std::shared_ptr<HirFor> hir);
	static MirBlockItem lower(TypeCtx &ctx, std::shared_ptr<HirIf> hir);
	static MirBlockItem lower(TypeCtx &ctx, std::shared_ptr<HirWhile> hir);
};

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
	MirFn(MirIdent ident, std::vector<MirFnParam> params, MirBlock block,
				TypeHandle ret)
			: ident(ident), params(params), block(block), ret(ret) {}

	MirIdent ident;
	std::vector<MirFnParam> params;
	MirBlock block;
	TypeHandle ret;

	static MirFn from_hir(TypeCtx &ctx, HirFn hir) {
		ctx.scope = std::make_shared<Scope>(Scope{ctx.scope});

		std::vector<MirFnParam> params;

		for (auto &param : hir.params) {
			auto p = MirFnParam::from_hir(ctx, param);

			ctx.scope->add(p.ident, p.type);
			params.push_back(p);
		}

		auto ident = MirIdent::from_hir(hir.ident);
		auto ret = ctx.get(MirPath::from_hir(
				hir.ret.value_or(HirPath{Span{}, {TokenIdent{Span{}, "void"}}})));

		auto fn = MirFn{ident, params, MirBlock{}, ret};

		ctx.add(ident, MirType{std::make_shared<MirFn>(fn)});
		fn.block = MirBlock::from_hir(ctx, hir.block);

		return fn;
	}
};

MirExpr MirExpr::from_hir(TypeCtx &ctx, std::shared_ptr<HirFnCall> hir) {
	auto call = MirFnCall::from_hir(ctx, *hir);
	auto handle = ctx.get(call.path);
	auto type = ctx.get(handle);

	if (auto fn = std::get_if<std::shared_ptr<MirFn>>(&type.kind)) {
		auto ret = (*fn)->ret;

		return MirExpr{std::make_shared<MirFnCall>(call), ret};
	} else {
		throw std::runtime_error("not a function");
	}
}

MirExpr MirExpr::from_hir(TypeCtx &ctx, HirExpr hir) {
	return std::visit([&ctx](auto &&hir) { return MirExpr::from_hir(ctx, hir); },
										hir.expr);
}

class MirIf {
public:
	MirIf(MirExpr cond, MirBlock block) : cond(cond), block(block) {}

	MirExpr cond;
	MirBlock block;
	std::optional<std::unique_ptr<MirIf>> else_;

	static MirIf from_hir(TypeCtx &ctx, std::shared_ptr<HirIf> hir) {
		auto cond = MirExpr::from_hir(ctx, hir->cond);
		auto block = MirBlock::from_hir(ctx, hir->block);

		auto mir = MirIf{cond, block};

		if (auto block = hir->elseBlock) {
			throw std::runtime_error("not implemented");
		}

		return mir;
	}
};

class MirLoop {
public:
	MirLoop(std::optional<MirStmt> setup, std::optional<MirExpr> cond,
					std::optional<MirStmt> step, MirBlock block)
			: setup(setup), cond(cond), step(step), block(block) {}
	MirLoop(MirLoop &&) = default;

	std::optional<MirStmt> setup;
	std::optional<MirExpr> cond;
	std::optional<MirStmt> step;
	MirBlock block;

	static MirStmt lower(TypeCtx &ctx, HirAssign hir) {
		return MirAssign::from_hir(ctx, hir);
	}

	static MirStmt lower(TypeCtx &ctx, HirReassign hir) {
		return MirReassign::from_hir(ctx, hir);
	}

	static MirStmt lower(TypeCtx &ctx, HirReturn hir) {
		return MirReturn::from_hir(ctx, hir);
	}

	static MirStmt lower(TypeCtx &ctx, HirFnCall hir) {
		return MirFnCall::from_hir(ctx, hir);
	}

	static MirLoop from_hir(TypeCtx &ctx, HirFor hir) {
		std::optional<MirStmt> setup;
		std::optional<MirExpr> cond;
		std::optional<MirStmt> step;

		if (hir.init) {
			if (!hir.init->needsSemi()) {
				throw std::runtime_error("not implemented");
			}

			setup =
					std::visit([&ctx](auto &&hir) { return MirLoop::lower(ctx, hir); },
										 hir.init->stmt);
		}

		if (hir.cond) {
			cond = MirExpr::from_hir(ctx, *hir.cond);
		}

		if (hir.update) {
			step = std::visit([&ctx](auto &&hir) { return MirLoop::lower(ctx, hir); },
												hir.update->stmt);
		}

		auto block = MirBlock::from_hir(ctx, hir.block);

		return MirLoop{setup, cond, step, block};
	}

	static MirLoop from_hir(TypeCtx &ctx, HirWhile hir) {
		auto cond = MirExpr::from_hir(ctx, hir.cond);
		auto block = MirBlock::from_hir(ctx, hir.block);

		return MirLoop{std::nullopt, cond, std::nullopt, block};
	}

	static MirLoop from_hir(TypeCtx &ctx, HirLoop hir) {
		auto block = MirBlock::from_hir(ctx, hir.block);

		return MirLoop{std::nullopt, std::nullopt, std::nullopt, block};
	}

private:
	static MirStmt lower(TypeCtx &ctx, HirStmtItem hir) {
		throw std::runtime_error("not implemented");
	}
};

using Mir = std::variant<MirFn, MirStruct, MirLit, MirExpr, MirLoop, MirIf,
												 MirAssign, MirReassign, MirReturn>;

MirBlockItem MirBlock::lower(TypeCtx &ctx, HirAssign hir) {
	return MirAssign::from_hir(ctx, hir);
}

MirBlockItem MirBlock::lower(TypeCtx &ctx, HirReassign hir) {
	return MirReassign::from_hir(ctx, hir);
}

MirBlockItem MirBlock::lower(TypeCtx &ctx, HirReturn hir) {
	return MirReturn::from_hir(ctx, hir);
}

MirBlockItem MirBlock::lower(TypeCtx &ctx, HirFnCall hir) {
	return MirFnCall::from_hir(ctx, hir);
}

MirBlockItem MirBlock::lower(TypeCtx &ctx, std::shared_ptr<HirFor> hir) {
	auto mir = MirLoop::from_hir(ctx, *hir);

	return std::make_shared<MirLoop>(std::move(mir));
}

MirBlockItem MirBlock::lower(TypeCtx &ctx, std::shared_ptr<HirIf> hir) {
	auto mir = MirIf::from_hir(ctx, hir);

	return std::make_shared<MirIf>(std::move(mir));
}

MirBlockItem MirBlock::lower(TypeCtx &ctx, std::shared_ptr<HirWhile> hir) {
	auto mir = MirLoop::from_hir(ctx, *hir);

	return std::make_shared<MirLoop>(std::move(mir));
}

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

		return mir;
	}

	Mir lower(HirFn hir) { return MirFn::from_hir(ctx, hir); }
	Mir lower(HirStruct hir) { return MirStruct::from_hir(ctx, hir); }
	Mir lower(HirExpr hir) { throw std::runtime_error("not allowed here"); }
	Mir lower(HirLoop hir) { throw std::runtime_error("not allowed here"); }
	Mir lower(HirIf hir) { throw std::runtime_error("not allowed here"); }
	Mir lower(HirFor hir) { throw std::runtime_error("not allowed here"); }
	Mir lower(HirWhile hir) { throw std::runtime_error("not allowed here"); }
	Mir lower(HirConst hir) { throw std::runtime_error("not implemented"); }
	Mir lower(HirAssign hir) { throw std::runtime_error("not allowed here"); }
	Mir lower(HirReassign hir) { throw std::runtime_error("not allowed here"); }
	Mir lower(HirReturn hir) { throw std::runtime_error("not allowed here"); }
	Mir lower(HirImport hir) { throw std::runtime_error("not implemented"); }
	Mir lower(HirExtern hir) { throw std::runtime_error("not implemented"); }

private:
	std::vector<Hir> hir;
	TypeCtx ctx;
};

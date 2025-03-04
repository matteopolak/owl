#pragma once

#include <cstddef>
#include <string>
#include <unordered_map>
#include <variant>
#include <vector>

#include "error.h"
#include "hir.h"
#include "parser.h"
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
	MirIdent(Span span, std::string ident) : span(span), ident(ident) {}
	MirIdent(std::string ident) : ident(ident) {}

	Span span;
	std::string ident;

	bool operator==(const MirIdent &other) const { return ident == other.ident; }

	static MirIdent from_hir(TokenIdent ident) {
		return MirIdent{ident.span(), ident.value()};
	}

	std::string value() const { return ident; }

	// hash
	std::size_t hash() const { return std::hash<std::string>{}(ident); }
};

template <> struct std::hash<MirIdent> {
	std::size_t operator()(const MirIdent &ident) const { return ident.hash(); }
};

class Scope {
public:
	Scope(std::shared_ptr<Scope> parent) : parent(parent) {}
	Scope() = default;

	void add(MirIdent ident, TypeHandle type) { variables.emplace(ident, type); }

	TypeHandle get(MirIdent ident) const {
		if (auto it = variables.find(ident); it != variables.end()) {
			return it->second;
		}

		if (parent) {
			return parent->get(ident);
		}

		throw std::runtime_error(
				fmt::format("variable '{}' not found", ident.value()));
	}

private:
	std::shared_ptr<Scope> parent;
	std::unordered_map<MirIdent, TypeHandle> variables;
};

class MirPath {
public:
	MirPath(MirIdent ident, std::size_t refCount = 0)
			: parts{ident}, refCount(refCount) {}
	MirPath() = default;
	MirPath(std::string ident) : parts{MirIdent{ident}} {}
	template <typename... Args> MirPath(std::string ident, Args... args) {
		parts.push_back(MirIdent{ident});
		(parts.push_back(MirIdent{args}), ...);
	}

	std::vector<MirIdent> parts;
	std::size_t refCount = 0;

	bool operator==(const MirPath &other) const {
		return refCount == other.refCount && parts == other.parts;
	}

	std::size_t hash() const {
		std::size_t h = 0;

		for (auto &part : parts) {
			h ^= part.hash();
		}

		return h ^ refCount;
	}

	static MirPath empty() { return MirPath{}; }

	static MirPath from_hir(HirPath hir) {
		MirPath path{MirIdent{hir.parts[0].value()}, hir.refCount};

		for (std::size_t i = 1; i < hir.parts.size(); i++) {
			path.parts.push_back(MirIdent{hir.parts[i].value()});
		}

		return path;
	}

	MirPath operator+(const MirPath &other) const {
		MirPath path = *this;

		for (auto &part : other.parts) {
			if (part.value() == "super") {
				path.parts.pop_back();
			} else {
				path.parts.push_back(part);
			}
		}

		path.refCount = other.refCount;

		return path;
	}
};

template <> struct std::hash<MirPath> {
	std::size_t operator()(const MirPath &path) const { return path.hash(); }
};

template <> class fmt::formatter<MirPath> {
public:
	constexpr auto parse(format_parse_context &ctx) { return ctx.begin(); }

	template <typename Context>
	auto format(const MirPath &path, Context &ctx) const {
		std::string str;

		for (std::size_t i = 0; i < path.parts.size(); i++) {
			str += path.parts[i].value();

			if (i < path.parts.size() - 1) {
				str += "::";
			}
		}

		return format_to(ctx.out(), "{}", str);
	}
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
	MirStruct(TypeHandle type, MirIdent ident, std::vector<MirStructField> fields)
			: type(type), ident(ident), fields(fields) {}

	TypeHandle type;
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

class MirFnSignature;

class MirPointer {
public:
	MirPointer(TypeHandle type, std::size_t refCount = 0)
			: type(type), refCount(refCount) {}

	TypeHandle type;
	std::size_t refCount;
};

using MirTypeKind = std::variant<MirTypeBuiltin, MirStruct, MirArray,
																 MirPointer, std::shared_ptr<MirFnSignature>>;

class MirType {
public:
	MirType(MirTypeKind kind) : kind(kind) {}

	MirTypeKind kind;

	std::string str(const TypeCtx &ctx) const {
		return std::visit([&](auto &&arg) -> std::string { return str(ctx, arg); },
											kind);
	}

	// get fn if it's a function type
	std::shared_ptr<MirFnSignature> fn() const {
		if (auto fn = std::get_if<std::shared_ptr<MirFnSignature>>(&kind)) {
			return *fn;
		}

		return nullptr;
	}

private:
	std::string str(const TypeCtx &ctx,
									const std::shared_ptr<MirFnSignature> &fn) const;
	std::string str(const TypeCtx &ctx, const MirArray &array) const;

	std::string str(const TypeCtx &ctx, const MirStruct &type) const {
		return fmt::format("struct {}", type.ident.value());
	}

	std::string str(const TypeCtx &ctx, const MirPointer &ptr) const;

	std::string str(const TypeCtx &ctx, const MirTypeBuiltin &type) const {
		switch (type) {
		case MirTypeBuiltin::Bool:
			return "bool";
		case MirTypeBuiltin::Char:
			return "char";
		case MirTypeBuiltin::Uint8:
			return "u8";
		case MirTypeBuiltin::Uint16:
			return "u16";
		case MirTypeBuiltin::Uint32:
			return "u32";
		case MirTypeBuiltin::Uint64:
			return "u64";
		case MirTypeBuiltin::Usize:
			return "usize";
		case MirTypeBuiltin::Int8:
			return "i8";
		case MirTypeBuiltin::Int16:
			return "i16";
		case MirTypeBuiltin::Int32:
			return "i32";
		case MirTypeBuiltin::Int64:
			return "i64";
		case MirTypeBuiltin::Isize:
			return "isize";
		case MirTypeBuiltin::Float16:
			return "f16";
		case MirTypeBuiltin::Float32:
			return "f32";
		case MirTypeBuiltin::Float64:
			return "f64";
		case MirTypeBuiltin::Void:
			return "void";
		}
	}
};

class TypeCtx {
public:
	std::shared_ptr<Scope> scope;

	TypeCtx() {
		scope = std::make_shared<Scope>(Scope{});

		add(MirPath{"bool"}, MirType{MirTypeBuiltin::Bool});
		auto ch = add(MirPath{"char"}, MirType{MirTypeBuiltin::Char});

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
		inverseHandles.emplace(handle, path);

		return handle;
	}

	TypeHandle get(MirPath path) {
		path = anchor + path;

		if (auto it = handles.find(path); it != handles.end()) {
			return it->second;
		}

		if (path.refCount > 0) {
			std::size_t refCount = path.refCount;
			path.refCount = 0;
			auto inner = get(path);
			path.refCount = refCount;

			return add(path, MirType{MirPointer{inner, path.refCount}});
		}

		throw std::runtime_error(fmt::format("type not found: {}", path));
	}

	MirPath getPath(TypeHandle handle) const {
		if (auto it = inverseHandles.find(handle); it != inverseHandles.end()) {
			return it->second;
		}

		throw std::runtime_error("type not found");
	}

	const MirType &get(TypeHandle handle) const {
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
	std::unordered_map<TypeHandle, MirPath> inverseHandles;
	std::unordered_map<TypeHandle, MirType> symbols;
	MirPath anchor;
};

std::string MirType::str(const TypeCtx &ctx, const MirArray &array) const {
	auto inner = ctx.get(array.type).str(ctx);

	return fmt::format("[{}; {}]", inner, array.size);
}

std::string MirType::str(const TypeCtx &ctx, const MirPointer &ptr) const {
	auto inner = ctx.get(ptr.type).str(ctx);

	return fmt::format("{:*>{}}{}", "", ptr.refCount, inner);
}

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
	auto struct_ = MirStruct{ctx.get(MirPath{"void"}), ident, fields};

	struct_.type = ctx.add(MirPath{ident}, MirType{struct_});

	return struct_;
}

class MirLit {
public:
	Span span;
	TokenLitType value;

	MirLit(TokenLit value) : span(value.span()), value(value.value) {}

	static MirLit from_hir(TokenLit hir) { return MirLit{hir}; }

	TypeHandle type(TypeCtx &ctx) const {
		if (std::holds_alternative<int>(value)) {
			return ctx.get(MirPath{"i32"});
		} else if (std::holds_alternative<double>(value)) {
			return ctx.get(MirPath{"f64"});
		} else if (std::holds_alternative<std::string>(value)) {
			return ctx.get(MirPath{MirIdent{"char"}, 1});
		} else if (std::holds_alternative<bool>(value)) {
			return ctx.get(MirPath{"bool"});
		}

		throw std::runtime_error("unknown literal type");
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
			auto expected = ctx.get(lhs.type);
			auto given = ctx.get(rhs.type);

			throw std::runtime_error(fmt::format(
					"type mismatch for operator `{}`. expected {}, found {}",
					TokenOp::str(hir->op.variant), expected.str(ctx), given.str(ctx)));
		}

		return MirExpr{std::make_shared<MirBinOp>(lhs, rhs, hir->op), lhs.type};
	}

	static MirExpr from_hir(TypeCtx &ctx, std::shared_ptr<HirUnOp> hir) {
		auto expr = MirExpr::from_hir(ctx, hir->expr);
		auto type = expr.type;

		if (hir->op.variant == Op::BIT_AND /* ref */) {
			auto path = ctx.getPath(expr.type);
			path.refCount++;
			type = ctx.get(path);
		} else if (hir->op.variant == Op::MUL /* deref */) {
			auto inner = ctx.get(expr.type);
			auto ptr = std::get_if<MirPointer>(&inner.kind);

			if (!ptr) {
				throw std::runtime_error(fmt::format(
						"cannot dereference non-pointer type `{}`", inner.str(ctx)));
			}

			auto path = ctx.getPath(expr.type);
			path.refCount--;

			if (path.refCount == 0) {
				type = ptr->type;
			} else {
				type = ctx.get(path);
			}
		}

		return MirExpr{std::make_shared<MirUnOp>(expr, hir->op), type};
	}

	Span span() const {
		return std::visit([](auto &&arg) { return span(arg); }, expr);
	}

private:
	static Span span(const MirLit &lit) { return lit.span; }
	static Span span(const MirIdent &ident) { return ident.span; }

	static Span span(const std::shared_ptr<MirFnCall> &fn);
	static Span span(const std::shared_ptr<MirBinOp> &bin);
	static Span span(const std::shared_ptr<MirUnOp> &un);
};

class MirBinOp {
public:
	MirBinOp(MirExpr lhs, MirExpr rhs, TokenOp op) : lhs(lhs), rhs(rhs), op(op) {}

	MirExpr lhs;
	MirExpr rhs;
	TokenOp op;

	Span span() const { return lhs.span().merge(rhs.span()); }
};

class MirUnOp {
public:
	MirUnOp(MirExpr expr, TokenOp op) : expr(expr), op(op) {}

	MirExpr expr;
	TokenOp op;

	Span span() const { return op.span().merge(expr.span()); }
};

class MirFnCall {
public:
	MirFnCall(Span span, MirPath path, std::vector<MirExpr> args, TypeHandle type)
			: span(span), path(path), args(args), type(type) {}

	Span span;
	MirPath path;
	std::vector<MirExpr> args;
	TypeHandle type;

	static MirFnCall from_hir(TypeCtx &ctx, HirFnCall hir);
};

Span MirExpr::span(const std::shared_ptr<MirFnCall> &fn) { return fn->span; }
Span MirExpr::span(const std::shared_ptr<MirBinOp> &bin) { return bin->span(); }
Span MirExpr::span(const std::shared_ptr<MirUnOp> &un) { return un->span(); }

class MirAssign {
public:
	MirAssign(MirIdent ident, MirExpr expr) : ident(ident), expr(expr) {}

	MirIdent ident;
	MirExpr expr;

	static MirAssign from_hir(TypeCtx &ctx, HirAssign hir) {
		auto path = MirIdent::from_hir(hir.ident.ident);

		auto expr = MirExpr::from_hir(ctx, hir.expr);

		if (hir.ident.type) {
			auto typePath = MirPath::from_hir(*hir.ident.type);
			auto type = ctx.get(typePath);

			if (expr.type != type) {
				auto lhs = ctx.get(type);
				auto rhs = ctx.get(expr.type);

				throw Error(
						fmt::format(
								"type mismatch for assignment of `{}`. expected {}, found {}",
								path.value(), lhs.str(ctx), rhs.str(ctx)),
						{{expr.span(), "incorrect value here"}});
			}
		}

		ctx.scope->add(path, expr.type);

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
			auto lhs = ctx.get(ctx.scope->get(path));
			auto rhs = ctx.get(expr.type);

			throw std::runtime_error(
					fmt::format("type mismatch for re-assignment of variable `{}`. "
											"expected {}, found {}",
											path.value(), lhs.str(ctx), rhs.str(ctx)));
		}

		return MirReassign{path, expr};
	}
};

class MirReturn {
public:
	explicit MirReturn(std::optional<MirExpr> expr) : expr(expr) {}

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

		ctx.scope = std::make_shared<Scope>(Scope{ctx.scope});

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
		auto ident = MirIdent::from_hir(hir.ident);

		if (auto type = hir.type) {
			auto path = MirPath::from_hir(*type);
			auto ty = ctx.add(path, MirType{ctx.get(path)});

			return MirFnParam{ident, ty};
		} else {
			throw std::runtime_error("not implemented. please specify type");
		}
	}
};

class MirFnSignature {
public:
	MirFnSignature(TypeHandle type, MirIdent ident,
								 std::vector<MirFnParam> params, TypeHandle ret)
			: type(type), ident(ident), params(params), ret(ret) {}

	TypeHandle type;

	MirIdent ident;
	std::vector<MirFnParam> params;
	TypeHandle ret;
	bool variadic = false;

	static MirFnSignature from_hir(TypeCtx &ctx, HirExtern hir) {
		auto scope = ctx.scope;
		ctx.scope = std::make_shared<Scope>(Scope{ctx.scope});

		std::vector<MirFnParam> params;

		for (auto &param : hir.params) {
			auto p = MirFnParam::from_hir(ctx, param);

			ctx.scope->add(p.ident, p.type);
			params.push_back(p);
		}

		auto ident = MirIdent::from_hir(hir.ident);
		auto ret = ctx.get(MirPath::from_hir(
				hir.ret.value_or(HirPath{Span{}, {TokenIdent{Span{}, "void"}}, 0})));

		auto sig = MirFnSignature{ret, ident, params, ret};
		sig.variadic = hir.variadic;
		auto ty = ctx.add(ident, MirType{std::make_shared<MirFnSignature>(sig)});

		sig.type = ty;
		scope->add(ident, ty);

		return sig;
	}

	static MirFnSignature from_hir(TypeCtx &ctx, HirFn hir) {
		auto scope = ctx.scope;
		ctx.scope = std::make_shared<Scope>(Scope{ctx.scope});

		std::vector<MirFnParam> params;

		for (auto &param : hir.params) {
			auto p = MirFnParam::from_hir(ctx, param);

			ctx.scope->add(p.ident, p.type);
			params.push_back(p);
		}

		auto ident = MirIdent::from_hir(hir.ident);
		auto ret = ctx.get(MirPath::from_hir(
				hir.ret.value_or(HirPath{Span{}, {TokenIdent{Span{}, "void"}}, 0})));

		auto sig = MirFnSignature{ret, ident, params, ret};
		auto ty = ctx.add(ident, MirType{std::make_shared<MirFnSignature>(sig)});

		sig.type = ty;

		scope->add(ident, ty);

		return sig;
	}
};

class MirFn : public MirFnSignature {
public:
	MirFn(TypeHandle type, MirIdent ident, std::vector<MirFnParam> params,
				TypeHandle ret, MirBlock block)
			: MirFnSignature(type, ident, params, ret), block(block) {}
	MirFn(MirFnSignature sig, MirBlock block)
			: MirFnSignature(sig), block(block) {}

	MirBlock block;

	static MirFn from_hir(TypeCtx &ctx, HirFn hir) {
		auto sig = MirFnSignature::from_hir(ctx, hir);

		return MirFn{sig, MirBlock::from_hir(ctx, hir.block)};
	}
};

MirFnCall MirFnCall::from_hir(TypeCtx &ctx, HirFnCall hir) {
	auto path = MirPath::from_hir(hir.path);

	if (path.parts.size() != 1) {
		throw std::runtime_error("modules not implemented");
	}

	auto fnType = ctx.scope->get(path.parts[0]);
	auto maybeFn = ctx.get(fnType);
	std::shared_ptr<MirFnSignature> fn = maybeFn.fn();

	if (!fn) {
		throw std::runtime_error(
				fmt::format("expected function, found {}", maybeFn.str(ctx)));
	}

	if (hir.args.size() != fn->params.size()) {
		if (!fn->variadic || hir.args.size() < fn->params.size()) {
			throw std::runtime_error(fmt::format("expected {} arguments, found {}",
																					 fn->params.size(), hir.args.size()));
		}
	}

	std::vector<MirExpr> args;

	for (std::size_t i = 0; i < hir.args.size(); i++) {
		auto arg = MirExpr::from_hir(ctx, hir.args[i]);

		if (i < fn->params.size() && arg.type != fn->params[i].type) {
			auto expected = ctx.get(fn->params[i].type);
			auto given = ctx.get(arg.type);

			throw std::runtime_error(
					fmt::format("type mismatch for argument {}. expected {}, found {}", i,
											expected.str(ctx), given.str(ctx)));
		}

		args.push_back(arg);
	}

	return MirFnCall{hir.span(), path, args, fn->ret};
}

std::string MirType::str(const TypeCtx &ctx,
												 const std::shared_ptr<MirFnSignature> &fn) const {
	std::string formatted = "fn(";

	for (std::size_t i = 0; i < fn->params.size(); i++) {
		formatted += ctx.get(fn->params[i].type).str(ctx);

		if (i + 1 < fn->params.size()) {
			formatted += ", ";
		}
	}

	formatted += "): ";
	formatted += ctx.get(fn->ret).str(ctx);

	return formatted;
}

MirExpr MirExpr::from_hir(TypeCtx &ctx, std::shared_ptr<HirFnCall> hir) {
	auto call = MirFnCall::from_hir(ctx, *hir);
	auto handle = ctx.get(call.path);
	auto type = ctx.get(handle);

	if (auto fn = std::get_if<std::shared_ptr<MirFnSignature>>(&type.kind)) {
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
			throw std::runtime_error("else block not implemented");
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

		ctx.scope = std::make_shared<Scope>(Scope{ctx.scope});

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
		ctx.scope = std::make_shared<Scope>(Scope{ctx.scope});

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

using Mir = std::variant<MirFn, MirFnSignature, MirStruct, MirLit, MirExpr,
												 MirLoop, MirIf, MirAssign, MirReassign, MirReturn>;

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
	HirLowerer(Parser parser) : parser(parser) {}

	TypeCtx ctx;

	std::vector<Mir> lower() {
		std::vector<Hir> hir = parser.collect();

		auto mainScope = ctx.scope;
		std::vector<Mir> mir;

		for (auto &hir : hir) {
			ctx.resetAnchor();
			ctx.scope = mainScope;
			mir.push_back(std::visit([this](auto &&hir) { return lower(hir); }, hir));
		}

		// make sure there's a main function with no args and that returns i32
		auto mainHandle = ctx.get(MirPath{"main"});
		auto main = ctx.get(mainHandle);

		if (auto fn = std::get_if<std::shared_ptr<MirFnSignature>>(&main.kind)) {
			if ((*fn)->params.size() != 0) {
				throw std::runtime_error("main function must have no arguments");
			}

			if ((*fn)->ret != ctx.get(MirPath{"i32"})) {
				throw std::runtime_error("main function must return i32");
			}
		} else {
			throw std::runtime_error("main function not found");
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
	Mir lower(HirExtern hir) { return MirFnSignature::from_hir(ctx, hir); }

private:
	Parser parser;
};

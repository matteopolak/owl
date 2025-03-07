#pragma once

#include <cstddef>
#include <string>
#include <unordered_map>
#include <variant>
#include <vector>

#include "error.hpp"
#include "hir.hpp"
#include "parser.hpp"
#include "span.hpp"
#include "token.hpp"

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

class TypeCtx;

class Scope {
public:
	Scope(std::shared_ptr<Scope> parent) : parent(parent) {
		if (parent) {
			fnReturnType = parent->fnReturnType;
			canBreak = parent->canBreak;
		}
	}
	Scope(std::shared_ptr<Scope> parent, std::optional<TypeHandle> fnReturnType)
			: fnReturnType(fnReturnType), parent(parent) {
		if (parent) {
			canBreak = parent->canBreak;
		}
	}
	Scope(std::shared_ptr<Scope> parent, bool canBreak)
			: canBreak(canBreak), parent(parent) {
		if (parent) {
			fnReturnType = parent->fnReturnType;
		}
	}
	Scope() = default;

	std::optional<TypeHandle> fnReturnType;
	bool canBreak = false;

	void add(MirIdent ident, TypeHandle type) { variables.emplace(ident, type); }

	TypeHandle get(MirIdent ident) const {
		if (auto it = variables.find(ident); it != variables.end()) {
			return it->second;
		}

		if (parent) {
			return parent->get(ident);
		}

		throw Error(fmt::format("cannot access variable `{}`", ident.ident),
								{{ident.span, "variable not found"}});
	}

private:
	std::shared_ptr<Scope> parent;
	std::unordered_map<MirIdent, TypeHandle> variables;
};

class MirPath {
public:
	explicit MirPath(MirIdent ident) : parts{ident} {}
	MirPath() = default;
	MirPath(std::string ident) : parts{MirIdent{ident}} {}
	template <typename... Args> MirPath(std::string ident, Args... args) {
		parts.push_back(MirIdent{ident});
		(parts.push_back(MirIdent{args}), ...);
	}

	std::vector<MirIdent> parts;

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

	MirPath operator+(const MirPath &other) const {
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
};

class MirArray;

using MirTypeLitItem = std::variant<MirPath, std::shared_ptr<MirArray>>;

class MirTypeLit {
public:
	MirTypeLit(MirTypeLitItem item, std::size_t refCount)
			: item(item), refCount(refCount) {}

	MirTypeLitItem item;
	std::size_t refCount;

	static MirTypeLit from_hir(TypeCtx &ctx, HirType hir);

	bool operator==(const MirTypeLit &other) const {
		return refCount == other.refCount && item == other.item;
	}

private:
	static MirTypeLit from_hir(TypeCtx &ctx, std::shared_ptr<HirArray> hir,
														 std::size_t refCount);
	static MirTypeLit from_hir(TypeCtx &ctx, HirPath hir, std::size_t refCount) {
		return MirTypeLit{MirPath::from_hir(hir), refCount};
	}
};

template <> struct std::hash<MirPath> {
	std::size_t operator()(const MirPath &path) const { return path.hash(); }
};

template <> struct std::hash<MirTypeLit> {
	std::size_t operator()(const MirTypeLit &type) const {
		return std::hash<MirTypeLitItem>{}(type.item);
	}
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

	static MirArray from_hir(TypeCtx &ctx, std::shared_ptr<HirArray> hir) {
		return from_hir(ctx, *hir);
	}

	static MirArray from_hir(TypeCtx &ctx, const HirArray &hir);
};

// hash
template <> struct std::hash<MirArray> {
	std::size_t operator()(const MirArray &array) const {
		return std::hash<TypeHandle>{}(array.type) ^
					 std::hash<std::size_t>{}(array.size);
	}
};

MirTypeLit MirTypeLit::from_hir(TypeCtx &ctx, HirType hir) {
	auto refCount = hir.refCount;

	return std::visit(
			[&](auto &&arg) -> MirTypeLit {
				return MirTypeLit::from_hir(ctx, arg, refCount);
			},
			hir.item);
}

MirTypeLit MirTypeLit::from_hir(TypeCtx &ctx, std::shared_ptr<HirArray> hir,
																std::size_t refCount) {
	return MirTypeLit{std::make_shared<MirArray>(MirArray::from_hir(ctx, hir)),
										refCount};
}

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

class MirExpr;

class MirArrayIndex {
public:
	MirArrayIndex(Span span, TypeHandle type, TypeHandle arrayType,
								std::shared_ptr<MirExpr> expr)
			: span(span), type(type), arrayType(arrayType), expr(expr) {}

	Span span;
	TypeHandle type;
	TypeHandle arrayType;
	std::shared_ptr<MirExpr> expr;

	static MirArrayIndex from_hir(TypeCtx &ctx, TypeHandle parent,
																HirArrayIndex hir);
};

class MirFieldAccess {
public:
	MirFieldAccess(Span span, TypeHandle type, TypeHandle structType,
								 MirIdent ident, std::size_t index)
			: span(span), type(type), structType(structType), ident(ident),
				index(index) {}

	Span span;
	TypeHandle type;
	TypeHandle structType;
	MirIdent ident;
	std::size_t index;

	static MirFieldAccess from_hir(TypeCtx &ctx, TypeHandle parent,
																 HirFieldAccess hir);
};

class MirPointerDeref;
class MirAssignable;

using MirAssignableItem = std::variant<MirArrayIndex, MirFieldAccess>;

using MirAssignableRootItem =
		std::variant<MirIdent, std::shared_ptr<MirPointerDeref>,
								 std::shared_ptr<MirAssignable>>;

class MirAssignable {
public:
	MirAssignable(Span span, TypeHandle type, MirAssignableRootItem root,
								std::vector<MirAssignableItem> parts)
			: span(span), type(type), root(std::move(root)), parts(std::move(parts)) {
	}

	Span span;
	TypeHandle type;
	MirAssignableRootItem root;
	std::vector<MirAssignableItem> parts;

	static MirAssignable from_hir(TypeCtx &ctx, HirAssignable hir);

private:
	static MirAssignableRootItem from_hir(TypeCtx &ctx,
																				std::shared_ptr<HirAssignable> hir) {
		return std::make_shared<MirAssignable>(MirAssignable::from_hir(ctx, *hir));
	}

	static MirAssignableRootItem from_hir(TypeCtx &ctx, TokenIdent hir) {
		return MirIdent::from_hir(hir);
	}

	static MirAssignableRootItem from_hir(TypeCtx &ctx,
																				std::shared_ptr<HirPointerDeref> hir);

	static MirAssignableRootItem from_hir(TypeCtx &ctx,
																				HirAssignableRootItem hir) {
		return std::visit(
				[&](auto &&arg) -> MirAssignableRootItem { return from_hir(ctx, arg); },
				hir);
	}

	static MirAssignableItem from_hir(TypeCtx &ctx, TypeHandle parent,
																		HirArrayIndex hir);

	static MirAssignableItem from_hir(TypeCtx &ctx, TypeHandle parent,
																		HirFieldAccess hir) {
		return MirFieldAccess::from_hir(ctx, parent, hir);
	}

	static MirAssignableItem from_hir(TypeCtx &ctx, TypeHandle parent,
																		HirAssignableItem hir) {
		return std::visit(
				[&](auto &&arg) -> MirAssignableItem {
					return from_hir(ctx, parent, arg);
				},
				hir);
	}
};

class MirPointerDeref {
public:
	MirPointerDeref(Span span, TypeHandle type, MirAssignable expr)
			: span(span), type(type), expr(std::move(expr)) {}

	Span span;
	TypeHandle type;
	MirAssignable expr;

	static MirPointerDeref from_hir(TypeCtx &ctx, HirPointerDeref hir);
};

MirAssignableRootItem
MirAssignable::from_hir(TypeCtx &ctx, std::shared_ptr<HirPointerDeref> hir) {
	return std::make_shared<MirPointerDeref>(
			MirPointerDeref::from_hir(ctx, *hir));
}

class TypeCtx {
public:
	std::shared_ptr<Scope> scope;

	// constructs a new shared_ptr for a scope then returns the old one
	template <typename... Args> std::shared_ptr<Scope> switchScope(Args... args) {
		auto old = scope;
		scope = std::make_shared<Scope>(Scope{args...});

		return old;
	}

	TypeCtx() {
		scope = std::make_shared<Scope>(Scope{});

		add(MirPath{"bool"}, 0, MirType{MirTypeBuiltin::Bool});
		add(MirPath{"char"}, 0, MirType{MirTypeBuiltin::Char});

		add(MirPath{"u8"}, 0, MirType{MirTypeBuiltin::Uint8});
		add(MirPath{"u16"}, 0, MirType{MirTypeBuiltin::Uint16});
		add(MirPath{"u32"}, 0, MirType{MirTypeBuiltin::Uint32});
		add(MirPath{"u64"}, 0, MirType{MirTypeBuiltin::Uint64});
		add(MirPath{"usize"}, 0, MirType{MirTypeBuiltin::Usize});

		add(MirPath{"i8"}, 0, MirType{MirTypeBuiltin::Int8});
		add(MirPath{"i16"}, 0, MirType{MirTypeBuiltin::Int16});
		add(MirPath{"i32"}, 0, MirType{MirTypeBuiltin::Int32});
		add(MirPath{"i64"}, 0, MirType{MirTypeBuiltin::Int64});
		add(MirPath{"isize"}, 0, MirType{MirTypeBuiltin::Isize});

		add(MirPath{"f16"}, 0, MirType{MirTypeBuiltin::Float16});
		add(MirPath{"f32"}, 0, MirType{MirTypeBuiltin::Float32});
		add(MirPath{"f64"}, 0, MirType{MirTypeBuiltin::Float64});

		add(MirPath{"void"}, 0, MirType{MirTypeBuiltin::Void});
	}

	TypeHandle add(MirPath path, std::size_t refCount, MirType type) {
		return add(MirTypeLit{path, refCount}, type);
	}

	// takes an absolute path from the root of the program
	TypeHandle add(MirTypeLit path, MirType type) {
		if (auto it = handles.find(path); it != handles.end()) {
			return it->second;
		}

		auto handle = TypeHandle{nextIndex++};

		symbols.emplace(handle, type);
		handles.emplace(path, handle);
		inverseHandles.emplace(handle, path);

		return handle;
	}

	TypeHandle get(MirTypeLit lit) {
		return std::visit([&](auto &&arg) { return get(arg, lit.refCount); },
											lit.item);
	}

	TypeHandle get(std::shared_ptr<MirArray> array, std::size_t refCount) {
		auto path = MirTypeLit{array, refCount};

		if (auto it = handles.find(path); it != handles.end()) {
			return it->second;
		}

		return add(path, MirType{*array});
	}

	TypeHandle get(MirPath path, std::size_t refCount) {
		path = anchor + path;

		auto wrap = MirTypeLit{path, refCount};

		if (auto it = handles.find(wrap); it != handles.end()) {
			return it->second;
		}

		if (refCount > 0) {
			auto inner = get(path, 0);

			return add(path, refCount, MirType{MirPointer{inner, refCount}});
		}

		throw std::runtime_error(fmt::format("type not found: {}", path));
	}

	MirTypeLit getTypeLit(TypeHandle handle) const {
		if (auto it = inverseHandles.find(handle); it != inverseHandles.end()) {
			return MirTypeLit{it->second};
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
	std::size_t nextIndex = 0;

	std::unordered_map<MirTypeLit, TypeHandle> handles;
	std::unordered_map<TypeHandle, MirTypeLit> inverseHandles;
	std::unordered_map<TypeHandle, MirType> symbols;
	MirPath anchor;
};

MirPointerDeref MirPointerDeref::from_hir(TypeCtx &ctx, HirPointerDeref hir) {
	auto expr = MirAssignable::from_hir(ctx, hir.expr);
	auto type = ctx.get(expr.type);
	auto ptr = std::get_if<MirPointer>(&type.kind);

	// ensure it can be dereferenced
	if (!ptr) {
		throw Error(
				fmt::format("cannot dereference non-pointer type `{}`", type.str(ctx)),
				{{hir.expr.span(), "expected pointer type"}});
	}

	auto ty = ctx.getTypeLit(ptr->type);
	ty.refCount = ptr->refCount - 1;
	auto handle = ctx.get(ty);

	return MirPointerDeref{hir.span(), handle, expr};
}

MirAssignable MirAssignable::from_hir(TypeCtx &ctx, HirAssignable hir) {
	MirAssignableRootItem root = MirAssignable::from_hir(ctx, hir.root);

	auto type = std::visit(
			[&](auto &&arg) -> TypeHandle {
				if constexpr (std::is_same_v<std::decay_t<decltype(arg)>, MirIdent>) {
					return ctx.scope->get(arg);
				} else if constexpr (std::is_same_v<std::decay_t<decltype(arg)>,
																						std::shared_ptr<MirPointerDeref>>) {
					return arg->type;
				} else if constexpr (std::is_same_v<std::decay_t<decltype(arg)>,
																						std::shared_ptr<MirAssignable>>) {
					return arg->type;
				}
			},
			root);

	std::vector<MirAssignableItem> parts;

	for (auto &part : hir.parts) {
		auto item = from_hir(ctx, type, part);

		type = std::visit([&](auto &&arg) -> TypeHandle { return arg.type; }, item);
		parts.push_back(item);
	}

	return MirAssignable{hir.span(), type, root, parts};
}

MirArray MirArray::from_hir(TypeCtx &ctx, const HirArray &hir) {
	auto path = MirTypeLit::from_hir(ctx, *hir.type);
	auto ty = ctx.get(path);

	auto size = std::get_if<int>(&hir.size.value);

	if (!size) {
		throw Error("array size must be a constant integer",
								{{hir.size.span(), "expected constant integer"}});
	}

	return MirArray{ty, std::size_t(*size)};
}

MirFieldAccess MirFieldAccess::from_hir(TypeCtx &ctx, TypeHandle parent,
																				HirFieldAccess hir) {
	auto ident = MirIdent::from_hir(hir.ident);
	auto type = ctx.get(parent);
	auto struct_ = std::get_if<MirStruct>(&type.kind);

	if (!struct_) {
		throw Error(fmt::format("cannot access field `{}` of non-struct type `{}`",
														ident.ident, type.str(ctx)),
								{{hir.ident.span(), "while accessing this field"}});
	}

	auto field = std::find_if(struct_->fields.begin(), struct_->fields.end(),
														[&](auto &field) { return field.ident == ident; });
	std::size_t idx = std::distance(struct_->fields.begin(), field);

	if (field == struct_->fields.end()) {
		throw Error(fmt::format("field `{}` not found in struct `{}`", ident.ident,
														struct_->ident.ident),
								{{hir.ident.span(), "field not found"},
								 {struct_->ident.span, fmt::format("struct `{}` defined here",
																									 struct_->ident.ident)}});
	}

	return MirFieldAccess{hir.span(), field->type, parent, ident, idx};
}

std::string MirType::str(const TypeCtx &ctx, const MirArray &array) const {
	auto inner = ctx.get(array.type).str(ctx);

	return fmt::format("[{}; {}]", inner, array.size);
}

std::string MirType::str(const TypeCtx &ctx, const MirPointer &ptr) const {
	auto inner = ctx.get(ptr.type).str(ctx);

	return fmt::format("{:*>{}}{}", "", ptr.refCount, inner);
}

MirStruct MirStruct::from_hir(TypeCtx &ctx, HirStruct hir) {
	std::vector<MirStructField> fields;

	for (auto &field : hir.fields) {
		auto path = MirTypeLit::from_hir(ctx, *field.type);
		auto type = ctx.get(path);
		auto ident = MirIdent::from_hir(field.ident);

		fields.push_back(MirStructField{ident, type});
	}

	auto ident = MirIdent::from_hir(hir.ident);
	auto struct_ = MirStruct{ctx.get(MirPath{"void"}, 0), ident, fields};

	struct_.type = ctx.add(MirPath{ident}, 0, MirType{struct_});

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
			return ctx.get(MirPath{"i32"}, 0);
		} else if (std::holds_alternative<double>(value)) {
			return ctx.get(MirPath{"f64"}, 0);
		} else if (std::holds_alternative<std::string>(value)) {
			return ctx.get(MirPath{"char"}, 1);
		} else if (std::holds_alternative<bool>(value)) {
			return ctx.get(MirPath{"bool"}, 0);
		}

		throw std::runtime_error("unknown literal type");
	}
};

class MirStructInstance;
class MirBinOp;
class MirUnOp;
class MirFnCall;
class MirArrayInstance;

using MirExprItem =
		std::variant<MirLit, MirIdent, MirAssignable,
								 std::shared_ptr<MirStructInstance>,
								 std::shared_ptr<MirArrayInstance>, std::shared_ptr<MirFnCall>,
								 std::shared_ptr<MirBinOp>, std::shared_ptr<MirUnOp>>;

class MirExpr {
public:
	MirExprItem expr;
	TypeHandle type;

	static MirExpr from_hir(TypeCtx &ctx, HirExpr hir);

	static MirExpr from_hir(TypeCtx &ctx, std::shared_ptr<HirAssignable> hir) {
		auto path = MirAssignable::from_hir(ctx, *hir);

		return MirExpr{path, path.type};
	}

	static MirExpr from_hir(TypeCtx &ctx, TokenLit hir) {
		auto lit = MirLit::from_hir(hir);

		return MirExpr{lit, lit.type(ctx)};
	}

	static MirExpr from_hir(TypeCtx &ctx, TokenIdent hir) {
		auto ident = MirIdent::from_hir(hir);
		auto type = ctx.scope->get(ident);

		return MirExpr{ident, type};
	}

	static MirExpr from_hir(TypeCtx &ctx, std::shared_ptr<HirArrayInstance> hir);
	static MirExpr from_hir(TypeCtx &ctx, std::shared_ptr<HirStructInstance> hir);

	static MirExpr from_hir(TypeCtx &ctx, std::shared_ptr<HirFnCall> hir);

	static MirExpr from_hir(TypeCtx &ctx, std::shared_ptr<HirBinOp> hir) {
		auto lhs = MirExpr::from_hir(ctx, hir->lhs);
		auto rhs = MirExpr::from_hir(ctx, hir->rhs);

		if (lhs.type != rhs.type) {
			auto expected = ctx.get(lhs.type);
			auto given = ctx.get(rhs.type);

			throw std::runtime_error(fmt::format(
					"unsupported operation `{}` between types `{}` and `{}`",
					TokenOp::str(hir->op.variant), expected.str(ctx), given.str(ctx)));
		}

		// validate operators.
		/*	ADD,
	SUB,
	MUL,
	DIV,
	MOD,
	EQEQ,
	EQ,
	NEQ,
	LT,
	GT,
	LTE,
	GTE,

	BIT_AND,
	BIT_OR,
	BIT_XOR,
	BIT_NOT,
	BIT_LSHIFT,
	BIT_RSHIFT,

	// not parsed, used by the parser
	LPAREN,
	RPAREN,
	AND,
	OR,
	NOT,

	// used in `extern` functions for variadics
	ELLIPSIS,*/

		auto type = lhs.type;

		// allowed operators are all of the first ones, all of the bit stuff, and
		// the three `AND`, `OR`, `NOT`
		// note that for the comparison ones, the return type is changed to a bool
		// and the bit stuff is only allowed for integers
		bool isInteger = lhs.type == ctx.get(MirPath{"i32"}, 0);
		bool isFloat = lhs.type == ctx.get(MirPath{"f64"}, 0);
		bool isBool = lhs.type == ctx.get(MirPath{"bool"}, 0);

		auto error = [&]() {
			return Error(
					fmt::format("unsupported operation `{}` between types `{}` and `{}`",
											TokenOp::str(hir->op.variant), ctx.get(lhs.type).str(ctx),
											ctx.get(rhs.type).str(ctx)),
					{{hir->lhs.span(), "incorrect type here"},
					 {hir->rhs.span(), "incorrect type here"}});
		};

		switch (hir->op.variant) {
		case Op::ADD:
		case Op::SUB:
		case Op::MUL:
		case Op::DIV:
		case Op::MOD:
			if (isInteger || isFloat) {
				break;
			}

			throw error();
		case Op::EQEQ:
		case Op::NEQ:
		case Op::LT:
		case Op::GT:
		case Op::LTE:
		case Op::GTE:
			if (isInteger || isFloat || isBool) {
				type = ctx.get(MirPath{"bool"}, 0);
				break;
			}

			throw error;
		case Op::BIT_AND:
		case Op::BIT_OR:
		case Op::BIT_XOR:
		case Op::BIT_NOT:
		case Op::BIT_LSHIFT:
		case Op::BIT_RSHIFT:
			if (isInteger) {
				break;
			}

			throw error();
		case Op::AND:
		case Op::OR:
			if (isBool) {
				break;
			}

			throw error();
		default:
			throw error();
		}

		return MirExpr{std::make_shared<MirBinOp>(lhs, rhs, hir->op), type};
	}

	static MirExpr from_hir(TypeCtx &ctx, std::shared_ptr<HirUnOp> hir) {
		auto expr = MirExpr::from_hir(ctx, hir->expr);
		auto type = expr.type;

		// check if the op is legal with the given type
		// allowed: !bool, -i32, -f64, ref T. deref is handled in a different
		// place
		//
		// bit stuff can only be applied to integers (only check i32)
		// TODO: check other types. will need to make it possible to use other
		// types as literals
		if (hir->op.variant == Op::BIT_AND /* ref */) {
			auto lit = ctx.getTypeLit(expr.type);
			lit.refCount++;
			type = ctx.get(lit);
		} else if (hir->op.variant == Op::NOT) {
			if (type != ctx.get(MirPath{"bool"}, 0)) {
				throw Error(fmt::format("cannot apply operator `not` to type `{}`",
																ctx.get(type).str(ctx)),
										{{hir->expr.span(), fmt::format("expected bool, found `{}`",
																										ctx.get(type).str(ctx))}});
			}
		} else if (hir->op.variant == Op::SUB) {
			if (type != ctx.get(MirPath{"i32"}, 0) &&
					type != ctx.get(MirPath{"f64"}, 0)) {
				throw Error(fmt::format("cannot apply operator `neg` to type `{}`",
																ctx.get(type).str(ctx)),
										{{hir->expr.span(),
											fmt::format("expected integer or float, found `{}`",
																	ctx.get(type).str(ctx))}});
			}
		} else if (hir->op.variant == Op::BIT_NOT) {
			if (type != ctx.get(MirPath{"i32"}, 0)) {
				throw Error(fmt::format("cannot apply operator `~` to type `{}`",
																ctx.get(type).str(ctx)),
										{{hir->expr.span(), fmt::format("expected i32, found `{}`",
																										ctx.get(type).str(ctx))}});
			}
		} else {
			throw Error(fmt::format("cannot apply operator `{}` to type `{}`",
															TokenOp::str(hir->op.variant),
															ctx.get(type).str(ctx)),
									{{hir->expr.span(), "incorrect type here"}});
		}

		return MirExpr{std::make_shared<MirUnOp>(expr, hir->op), type};
	}

	Span span() const {
		return std::visit([](auto &&arg) { return span(arg); }, expr);
	}

private:
	static Span span(const MirLit &lit) { return lit.span; }
	static Span span(const MirIdent &ident) { return ident.span; }
	static Span span(const MirAssignable &assign) { return assign.span; }

	static Span span(const std::shared_ptr<MirArrayInstance> &instance);
	static Span span(const std::shared_ptr<MirStructInstance> &instance);
	static Span span(const std::shared_ptr<MirFnCall> &fn);
	static Span span(const std::shared_ptr<MirBinOp> &bin);
	static Span span(const std::shared_ptr<MirUnOp> &un);
};

MirArrayIndex MirArrayIndex::from_hir(TypeCtx &ctx, TypeHandle parent,
																			HirArrayIndex hir) {
	auto expr = MirExpr::from_hir(ctx, hir.expr);
	auto type = ctx.get(parent);
	auto array = std::get_if<MirArray>(&type.kind);

	if (!array) {
		throw Error(fmt::format("cannot index non-array type `{}`", type.str(ctx)),
								{{hir.expr.span(), "incorrect index here"}});
	}

	// ensure type is i32
	if (expr.type != ctx.get(MirPath{"i32"}, 0)) {
		throw Error(fmt::format("array index must be an integer. found `{}`",
														ctx.get(expr.type).str(ctx)),
								{{hir.expr.span(), "incorrect index here"}});
	}

	return MirArrayIndex{hir.span(), array->type, parent,
											 std::make_shared<MirExpr>(expr)};
}

MirAssignableItem MirAssignable::from_hir(TypeCtx &ctx, TypeHandle parent,
																					HirArrayIndex hir) {
	return MirArrayIndex::from_hir(ctx, parent, hir);
}

class MirArrayInstance {
public:
	MirArrayInstance(Span span, TypeHandle type, std::vector<MirExpr> values,
									 std::size_t size)
			: span(span), type(type), values(std::move(values)), size(size) {}

	Span span;
	TypeHandle type;
	std::vector<MirExpr> values;
	std::size_t size;

	static MirArrayInstance from_hir(TypeCtx &ctx, HirArrayInstance hir) {
		std::optional<TypeHandle> type;
		std::vector<MirExpr> values;
		Span inferredTypeOrigin;

		for (auto &value : hir.values) {
			auto expr = MirExpr::from_hir(ctx, value);

			if (type && expr.type != type) {
				auto expected = ctx.get(*type);
				auto given = ctx.get(expr.type);

				throw Error(
						fmt::format("type mismatch in array. expected `{}`, found `{}`",
												expected.str(ctx), given.str(ctx)),
						{{value.span(), "incorrect value here"},
						 {inferredTypeOrigin, "inferred type from previous value here"}});
			}

			type = expr.type;
			inferredTypeOrigin = value.span();
			values.push_back(expr);
		}

		auto litSize = std::get_if<TokenLit>(&hir.size.expr);

		if (!litSize) {
			throw Error("array size must be a constant",
									{{hir.size.span(), "expected constant"}});
		}

		if (!std::holds_alternative<int>(litSize->value)) {
			throw Error("array size must be an integer constant",
									{{litSize->span(),
										fmt::format("expected integer constant, found `{}`",
																litSize->type())}});
		}

		auto size = std::size_t(std::get<int>(litSize->value));

		if (size < 1) {
			throw Error(
					"array size must be greater than 0",
					{{litSize->span(),
						fmt::format("size must be greater than 0, found {}", size)}});
		}

		if (size != values.size()) {
			throw Error(fmt::format("array size mismatch. expected `{}`, found `{}`",
															size, values.size()),
									{{hir.size.span(), "incorrect size here"}});
		}

		auto array = std::make_shared<MirArray>(MirArray{*type, size});
		auto ty = ctx.get(array, 0);

		return MirArrayInstance{hir.span(), ty, std::move(values), size};
	}
};

class MirStructFieldAssign {
public:
	MirStructFieldAssign(MirIdent ident, MirExpr expr)
			: ident(ident), expr(expr) {}

	MirIdent ident;
	MirExpr expr;

	static MirStructFieldAssign from_hir(TypeCtx &ctx, HirStructFieldAssign hir) {
		auto ident = MirIdent::from_hir(hir.ident);
		auto expr = MirExpr::from_hir(ctx, hir.expr);

		return MirStructFieldAssign{ident, expr};
	}
};

class MirStructInstance {
public:
	MirStructInstance(Span span, TypeHandle type, MirPath path,
										std::vector<MirStructFieldAssign> fields)
			: span(span), type(type), path(path), fields(fields) {}

	Span span;
	TypeHandle type;
	MirPath path;
	std::vector<MirStructFieldAssign> fields;

	static MirStructInstance from_hir(TypeCtx &ctx, HirStructInstance hir) {
		auto path = MirPath::from_hir(hir.path);
		auto type = ctx.get(path, 0);

		std::vector<MirStructFieldAssign> fields;

		auto struct_ = std::get_if<MirStruct>(&ctx.get(type).kind);

		if (!struct_) {
			throw Error(fmt::format("cannot instantiate non-struct type `{}`", path),
									{{hir.path.span(), "expected struct type"}});
		}

		for (std::size_t i = 0; i < hir.fields.size(); i++) {
			auto field = hir.fields[i];
			auto f = MirStructFieldAssign::from_hir(ctx, field);

			auto found =
					std::find_if(struct_->fields.begin(), struct_->fields.end(),
											 [&](auto &field) { return field.ident == f.ident; });

			if (found == struct_->fields.end()) {
				throw Error(
						fmt::format("field `{}` not found in struct `{}`", f.ident.ident,
												struct_->ident.ident),
						{{field.ident.span(), "field not found"},
						 {struct_->ident.span,
							fmt::format("struct `{}` defined here", struct_->ident.ident)}});
			}

			if (found->type != f.expr.type) {
				auto lhs = ctx.get(found->type);
				auto rhs = ctx.get(f.expr.type);

				throw Error(
						fmt::format(
								"type mismatch for field `{}`. expected `{}`, found `{}`",
								f.ident.ident, lhs.str(ctx), rhs.str(ctx)),
						{{f.expr.span(), "incorrect value here"}});
			}

			auto end = hir.fields.begin() + i;
			auto duplicate = std::find_if(hir.fields.begin(), end, [&](auto &f) {
				return f.ident == field.ident;
			});

			if (duplicate != end) {
				throw Error(fmt::format("duplicate field `{}` in struct `{}`",
																field.ident.value(), struct_->ident.ident),
										{
												{field.ident.span(), "duplicate field"},
												{duplicate->ident.span(), "previous field here"},
										});
			}

			fields.push_back(f);
		}

		// check for missing fields
		for (auto &field : struct_->fields) {
			auto found = std::find_if(fields.begin(), fields.end(), [&](auto &f) {
				return f.ident == field.ident;
			});

			if (found == fields.end()) {
				throw Error(
						fmt::format("missing field `{}` in instantiation of struct `{}`",
												field.ident.ident, struct_->ident.ident),
						{{hir.path.span(),
							fmt::format("missing field `{}` in this instantiation",
													field.ident.ident)}});
			}
		}

		std::sort(fields.begin(), fields.end(), [&](auto &a, auto &b) {
			auto aField =
					std::find_if(struct_->fields.begin(), struct_->fields.end(),
											 [&](auto &field) { return field.ident == a.ident; });
			auto bField =
					std::find_if(struct_->fields.begin(), struct_->fields.end(),
											 [&](auto &field) { return field.ident == b.ident; });

			return std::distance(struct_->fields.begin(), aField) <
						 std::distance(struct_->fields.begin(), bField);
		});

		return MirStructInstance{hir.span(), type, path, fields};
	}
};

MirExpr MirExpr::from_hir(TypeCtx &ctx,
													std::shared_ptr<HirStructInstance> hir) {
	auto instance = MirStructInstance::from_hir(ctx, *hir);

	return MirExpr{std::make_shared<MirStructInstance>(instance), instance.type};
}

MirExpr MirExpr::from_hir(TypeCtx &ctx, std::shared_ptr<HirArrayInstance> hir) {
	auto instance = MirArrayInstance::from_hir(ctx, *hir);

	return MirExpr{std::make_shared<MirArrayInstance>(instance), instance.type};
}

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
Span MirExpr::span(const std::shared_ptr<MirStructInstance> &instance) {
	return instance->span;
}
Span MirExpr::span(const std::shared_ptr<MirArrayInstance> &instance) {
	return instance->span;
}

class MirAssign {
public:
	MirAssign(MirIdent ident, MirExpr expr) : ident(ident), expr(expr) {}

	MirIdent ident;
	MirExpr expr;

	static MirAssign from_hir(TypeCtx &ctx, HirAssign hir) {
		auto path = MirIdent::from_hir(hir.ident.ident);
		auto expr = MirExpr::from_hir(ctx, hir.expr);

		if (hir.ident.type) {
			auto typePath = MirTypeLit::from_hir(ctx, *hir.ident.type);
			auto type = ctx.get(typePath);

			if (expr.type != type) {
				auto lhs = ctx.get(type);
				auto rhs = ctx.get(expr.type);

				throw Error(fmt::format("type mismatch for assignment of `{}`. "
																"expected `{}`, found `{}`",
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
	MirReassign(MirAssignable path, MirExpr expr) : path(path), expr(expr) {}

	MirAssignable path;
	MirExpr expr;

	static MirReassign from_hir(TypeCtx &ctx, HirReassign hir) {
		auto path = MirAssignable::from_hir(ctx, hir.path);
		auto expr = MirExpr::from_hir(ctx, hir.expr);

		if (path.type != expr.type) {
			auto lhs = ctx.get(path.type);
			auto rhs = ctx.get(expr.type);

			throw Error("type mismatch for re-assignment of variable.",
									{{path.span, "reassignment of variable here"},
									 {expr.span(), fmt::format("expected `{}`, found `{}`",
																						 lhs.str(ctx), rhs.str(ctx))}});
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

			if (auto ret = ctx.scope->fnReturnType) {
				if (expr->type != *ret) {
					auto lhs = ctx.get(*ret);
					auto rhs = ctx.get(expr->type);

					throw Error(
							fmt::format("type mismatch for return. expected `{}`, found `{}`",
													lhs.str(ctx), rhs.str(ctx)),
							{{hir.expr->span(), "incorrect value here"}});
				}
			}
		}

		if (!ctx.scope->fnReturnType) {
			throw Error("cannot return from outside a function",
									{{hir.span(), "return outside function here"}});
		}

		return MirReturn{expr};
	}
};

class MirBreak {
public:
	explicit MirBreak(Span span) : span(span) {}

	Span span;

	static MirBreak from_hir(TypeCtx &ctx, HirBreak hir) {
		if (!ctx.scope->canBreak) {
			throw Error("cannot break outside of a loop",
									{{hir.span(), "break outside loop here"}});
		}

		return MirBreak{hir.span()};
	}
};

class MirContinue {
public:
	explicit MirContinue(Span span) : span(span) {}

	Span span;

	static MirContinue from_hir(TypeCtx &ctx, HirContinue hir) {
		if (!ctx.scope->canBreak) {
			throw Error("cannot continue outside of a loop",
									{{hir.span(), "continue outside loop here"}});
		}

		return MirContinue{hir.span()};
	}
};

using MirStmt = std::variant<MirAssign, MirReassign, MirReturn, MirBreak,
														 MirContinue, MirFnCall>;

class MirLoop;
class MirIf;

using MirBlockItem =
		std::variant<MirAssign, MirReassign, MirReturn, MirFnCall, MirBreak,
								 MirContinue, std::shared_ptr<MirLoop>, std::shared_ptr<MirIf>>;

class MirBlock {
public:
	MirBlock(std::vector<MirBlockItem> items) : items(items) {}
	MirBlock() = default;

	std::vector<MirBlockItem> items;

	static MirBlock from_hir(TypeCtx &ctx, HirBlock hir) {
		std::vector<MirBlockItem> items;

		auto old = ctx.switchScope(ctx.scope);

		for (auto &hir : hir.stmts) {
			items.push_back(std::visit(
					[&ctx](auto &&hir) { return MirBlock::lower(ctx, hir); }, hir.stmt));
		}

		ctx.scope = old;

		return MirBlock{items};
	}

	static MirBlockItem lower(TypeCtx &ctx, HirBreak hir);
	static MirBlockItem lower(TypeCtx &ctx, HirContinue hir);
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
			auto path = MirTypeLit::from_hir(ctx, *type);
			auto ty = ctx.get(path);

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
		auto old = ctx.switchScope(ctx.scope);

		std::vector<MirFnParam> params;

		for (auto &param : hir.params) {
			auto p = MirFnParam::from_hir(ctx, param);

			ctx.scope->add(p.ident, p.type);
			params.push_back(p);
		}

		auto ident = MirIdent::from_hir(hir.ident);
		// `void`
		auto fallback =
				HirType{Span{}, HirPath{Span{}, {TokenIdent{Span{}, "void"}}}, 0};
		auto ret = ctx.get(MirTypeLit::from_hir(ctx, hir.ret.value_or(fallback)));

		auto sig = MirFnSignature{ret, ident, params, ret};
		sig.variadic = hir.variadic;
		auto ty = ctx.add(MirPath{ident}, 0,
											MirType{std::make_shared<MirFnSignature>(sig)});

		sig.type = ty;
		old->add(ident, ty);
		ctx.scope = old;

		return sig;
	}

	static MirFnSignature from_hir(TypeCtx &ctx, HirFn hir) {
		auto fallback =
				HirType{Span{}, HirPath{Span{}, {TokenIdent{Span{}, "void"}}}, 0};
		auto ret = ctx.get(MirTypeLit::from_hir(ctx, hir.ret.value_or(fallback)));

		std::vector<MirFnParam> params;

		for (auto &param : hir.params) {
			auto p = MirFnParam::from_hir(ctx, param);
			params.push_back(p);
		}

		auto ident = MirIdent::from_hir(hir.ident);

		auto sig = MirFnSignature{ret, ident, params, ret};
		auto ty = ctx.add(MirPath{ident}, 0,
											MirType{std::make_shared<MirFnSignature>(sig)});

		sig.type = ty;
		ctx.scope->add(ident, ty);

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
		auto old = ctx.switchScope(ctx.scope, sig.ret);

		for (auto &param : sig.params) {
			ctx.scope->add(param.ident, param.type);
		}

		auto block = MirBlock::from_hir(ctx, hir.block);

		// ensure last statement is a return
		if (block.items.empty() ||
				!std::holds_alternative<MirReturn>(block.items.back())) {
			if (sig.ret != ctx.get(MirPath{"void"}, 0)) {
				throw Error("non-void function body must end with a return statement",
										{{hir.ident.span(), "function defined here"}});
			} else {
				block.items.push_back(MirReturn{std::nullopt});
			}
		}

		ctx.scope = old;

		return MirFn{sig, block};
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
		throw Error(fmt::format("cannot find function `{}` in this scope", path),
								{{hir.path.span(), "not found in this scope"}});
	}

	if (hir.args.size() != fn->params.size()) {
		if (!fn->variadic || hir.args.size() < fn->params.size()) {
			throw Error(
					fmt::format(
							"expected {} argument{}, found {} in call to function `{}`",
							fn->params.size(), fn->params.size() == 1 ? "" : "s",
							hir.args.size(), path),
					{{hir.lparen.span().merge(hir.rparen.span()),
						"incorrect number of arguments here"}});
		}
	}

	std::vector<MirExpr> args;

	for (std::size_t i = 0; i < hir.args.size(); i++) {
		auto arg = MirExpr::from_hir(ctx, hir.args[i]);

		if (i < fn->params.size() && arg.type != fn->params[i].type) {
			auto expected = ctx.get(fn->params[i].type);
			auto given = ctx.get(arg.type);

			throw Error(
					fmt::format("mismatched types for argument in call to function `{}`",
											fn->params[i].ident.value(), path),
					{{arg.span(), fmt::format("expected `{}`, found `{}`",
																		expected.str(ctx), given.str(ctx))}});
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
	auto handle = ctx.get(call.path, 0);
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

class MirElse {
public:
	MirElse(std::optional<MirExpr> cond, MirBlock block)
			: cond(cond), block(block) {}

	std::optional<MirExpr> cond;
	MirBlock block;
};

class MirIf {
public:
	MirIf(MirExpr cond, MirBlock block, std::vector<MirElse> else_)
			: cond(cond), block(block), else_(else_) {}

	MirExpr cond;
	MirBlock block;
	std::vector<MirElse> else_;

	static MirIf from_hir(TypeCtx &ctx, std::shared_ptr<HirIf> hir) {
		auto cond = MirExpr::from_hir(ctx, hir->cond);
		auto block = MirBlock::from_hir(ctx, hir->block);

		std::vector<MirElse> else_;

		for (auto &elseIf : hir->else_) {
			std::optional<MirExpr> cond = std::nullopt;

			if (elseIf.cond) {
				cond = MirExpr::from_hir(ctx, elseIf.cond->second);
			}

			auto block = MirBlock::from_hir(ctx, elseIf.block);

			else_.push_back(MirElse{cond, block});
		}

		return MirIf{cond, block, else_};
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

		auto old = ctx.switchScope(ctx.scope, true);

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

		ctx.scope = old;

		return MirLoop{setup, cond, step, block};
	}

	static MirLoop from_hir(TypeCtx &ctx, HirWhile hir) {
		auto old = ctx.switchScope(ctx.scope, true);

		auto cond = MirExpr::from_hir(ctx, hir.cond);
		auto block = MirBlock::from_hir(ctx, hir.block);

		ctx.scope = old;

		return MirLoop{std::nullopt, cond, std::nullopt, block};
	}

	static MirLoop from_hir(TypeCtx &ctx, HirLoop hir) {
		auto old = ctx.switchScope(ctx.scope, true);
		auto block = MirBlock::from_hir(ctx, hir.block);
		ctx.scope = old;

		return MirLoop{std::nullopt, std::nullopt, std::nullopt, block};
	}

private:
	static MirStmt lower(TypeCtx &ctx, HirStmtItem hir) {
		throw std::runtime_error("not implemented");
	}
};

using Mir = std::variant<MirFn, MirFnSignature, MirStruct, MirLit, MirExpr,
												 MirLoop, MirIf, MirAssign, MirReassign, MirReturn>;

MirBlockItem MirBlock::lower(TypeCtx &ctx, HirBreak hir) {
	return MirBreak::from_hir(ctx, hir);
}

MirBlockItem MirBlock::lower(TypeCtx &ctx, HirContinue hir) {
	return MirContinue::from_hir(ctx, hir);
}

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

		std::vector<Mir> mir;

		for (auto &hir : hir) {
			ctx.resetAnchor();
			mir.push_back(lower(hir));
		}

		// make sure there's a main function with no args and that returns i32
		auto mainHandle = ctx.get(MirPath{"main"}, 0);
		auto main = ctx.get(mainHandle);

		if (auto fn = std::get_if<std::shared_ptr<MirFnSignature>>(&main.kind)) {
			if ((*fn)->params.size() != 0) {
				throw std::runtime_error("main function must have no arguments");
			}

			if ((*fn)->ret != ctx.get(MirPath{"i32"}, 0)) {
				throw std::runtime_error("main function must return i32");
			}
		} else {
			throw std::runtime_error("main function not found");
		}

		return mir;
	}

	Mir lower(Hir hir) {
		return std::visit([this](auto &&hir) { return lower(hir); }, hir);
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

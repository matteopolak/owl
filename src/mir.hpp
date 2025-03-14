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

	friend class Scope;
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
	std::size_t hash() const { return std::hash<std::string>{}(ident); }
};

template <> struct std::hash<MirIdent> {
	std::size_t operator()(const MirIdent &ident) const { return ident.hash(); }
};

class MirGenerics {
public:
	MirGenerics(Span span, std::vector<MirIdent> generics)
			: span(span), generics(std::move(generics)) {}

	Span span;
	std::vector<MirIdent> generics;

	static MirGenerics from_hir(HirGenerics hir) {
		std::vector<MirIdent> generics;

		for (auto &ident : hir.generics) {
			auto id = MirIdent::from_hir(ident);

			for (auto &gen : generics) {
				if (gen == id) {
					throw Error(fmt::format("duplicate generic `{}`", id.ident),
											{{id.span, "duplicate generic"},
											 {gen.span, "previous declaration here"}});
				}
			}

			generics.push_back(id);
		}

		return MirGenerics{hir.span(), std::move(generics)};
	}
};

class Scope;

class MirPath {
public:
	explicit MirPath(MirIdent ident, Span span = Span{})
			: parts{ident}, span(span) {}
	MirPath() = default;
	MirPath(std::string ident) : parts{MirIdent{ident}} {}
	template <typename... Args> MirPath(std::string ident, Args... args) {
		parts.push_back(MirIdent{ident});
		(parts.push_back(MirIdent{args}), ...);
	}

	std::vector<MirIdent> parts;
	Span span;

	bool operator==(const MirPath &other) const { return parts == other.parts; }

	std::size_t hash() const {
		std::size_t h = 0;

		for (auto &part : parts) {
			h ^= part.hash();
		}

		return h;
	}

	static MirPath from_hir(HirPath hir) {
		MirPath path{MirIdent{hir.parts[0].value()}, hir.span()};

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

	static MirTypeLit from_hir(std::shared_ptr<Scope> ctx, HirType hir);

	bool operator==(const MirTypeLit &other) const {
		return refCount == other.refCount && item == other.item;
	}

private:
	static MirTypeLit from_hir(std::shared_ptr<Scope> ctx,
														 std::shared_ptr<HirArray> hir,
														 std::size_t refCount);
	static MirTypeLit from_hir(std::shared_ptr<Scope> ctx, HirPath hir,
														 std::size_t refCount) {
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

	static MirStructField from_hir(std::shared_ptr<Scope> ctx, HirTypedIdent hir);

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

	static MirStruct from_hir(std::shared_ptr<Scope> ctx, HirStruct hir);
};

class MirArray {
public:
	MirArray(TypeHandle type, std::size_t size) : type(type), size(size) {}

	TypeHandle type;
	std::size_t size;

	static MirArray from_hir(std::shared_ptr<Scope> ctx,
													 std::shared_ptr<HirArray> hir) {
		return from_hir(ctx, *hir);
	}

	static MirArray from_hir(std::shared_ptr<Scope> ctx, const HirArray &hir);
};

template <> struct std::hash<MirArray> {
	std::size_t operator()(const MirArray &array) const {
		return std::hash<TypeHandle>{}(array.type) ^
					 std::hash<std::size_t>{}(array.size);
	}
};

MirTypeLit MirTypeLit::from_hir(std::shared_ptr<Scope> ctx, HirType hir) {
	auto refCount = hir.refCount;

	return std::visit(
			[&](auto &&arg) -> MirTypeLit {
				return MirTypeLit::from_hir(ctx, arg, refCount);
			},
			hir.item);
}

MirTypeLit MirTypeLit::from_hir(std::shared_ptr<Scope> ctx,
																std::shared_ptr<HirArray> hir,
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

	std::string str(const std::shared_ptr<Scope> ctx) const {
		return std::visit([&](auto &&arg) -> std::string { return str(ctx, arg); },
											kind);
	}

	std::shared_ptr<MirFnSignature> fn() const {
		if (auto fn = std::get_if<std::shared_ptr<MirFnSignature>>(&kind)) {
			return *fn;
		}

		return nullptr;
	}

private:
	std::string str(const std::shared_ptr<Scope> ctx,
									const std::shared_ptr<MirFnSignature> &fn) const;
	std::string str(const std::shared_ptr<Scope> ctx,
									const MirArray &array) const;

	std::string str(const std::shared_ptr<Scope> ctx,
									const MirStruct &type) const {
		return fmt::format("struct {}", type.ident.value());
	}

	std::string str(const std::shared_ptr<Scope> ctx,
									const MirPointer &ptr) const;

	std::string str(const std::shared_ptr<Scope> ctx,
									const MirTypeBuiltin &type) const {
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

	static MirArrayIndex from_hir(std::shared_ptr<Scope> ctx, TypeHandle parent,
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

	static MirFieldAccess from_hir(std::shared_ptr<Scope> ctx, TypeHandle parent,
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

	static MirAssignable from_hir(std::shared_ptr<Scope> ctx, HirAssignable hir);

private:
	static MirAssignableRootItem from_hir(std::shared_ptr<Scope> ctx,
																				std::shared_ptr<HirAssignable> hir) {
		return std::make_shared<MirAssignable>(MirAssignable::from_hir(ctx, *hir));
	}

	static MirAssignableRootItem from_hir(std::shared_ptr<Scope> ctx,
																				TokenIdent hir) {
		return MirIdent::from_hir(hir);
	}

	static MirAssignableRootItem from_hir(std::shared_ptr<Scope> ctx,
																				std::shared_ptr<HirPointerDeref> hir);

	static MirAssignableRootItem from_hir(std::shared_ptr<Scope> ctx,
																				HirAssignableRootItem hir) {
		return std::visit(
				[&](auto &&arg) -> MirAssignableRootItem { return from_hir(ctx, arg); },
				hir);
	}

	static MirAssignableItem from_hir(std::shared_ptr<Scope> ctx,
																		TypeHandle parent, HirArrayIndex hir);

	static MirAssignableItem from_hir(std::shared_ptr<Scope> ctx,
																		TypeHandle parent, HirFieldAccess hir) {
		return MirFieldAccess::from_hir(ctx, parent, hir);
	}

	static MirAssignableItem from_hir(std::shared_ptr<Scope> ctx,
																		TypeHandle parent, HirAssignableItem hir) {
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

	static MirPointerDeref from_hir(std::shared_ptr<Scope> ctx,
																	HirPointerDeref hir);
};

MirAssignableRootItem
MirAssignable::from_hir(std::shared_ptr<Scope> ctx,
												std::shared_ptr<HirPointerDeref> hir) {
	return std::make_shared<MirPointerDeref>(
			MirPointerDeref::from_hir(ctx, *hir));
}

class MirGenericsInstance {
public:
	MirGenericsInstance(Span span, std::vector<TypeHandle> generics)
			: generics(std::move(generics)) {}

	std::vector<TypeHandle> generics;
};

// hash
template <> struct std::hash<MirGenericsInstance> {
	std::size_t operator()(const MirGenericsInstance &gen) const {
		std::size_t h = 0;

		for (auto &gen : gen.generics) {
			h ^= gen.hash();
		}

		return h;
	}
};

class Scope {
public:
	static std::shared_ptr<Scope> root() {
		auto scope = std::make_shared<Scope>(nullptr, std::nullopt, false);

		scope->addType(MirPath{"bool"}, 0, MirType{MirTypeBuiltin::Bool});
		scope->addType(MirPath{"char"}, 0, MirType{MirTypeBuiltin::Char});

		scope->addType(MirPath{"u8"}, 0, MirType{MirTypeBuiltin::Uint8});
		scope->addType(MirPath{"u16"}, 0, MirType{MirTypeBuiltin::Uint16});
		scope->addType(MirPath{"u32"}, 0, MirType{MirTypeBuiltin::Uint32});
		scope->addType(MirPath{"u64"}, 0, MirType{MirTypeBuiltin::Uint64});
		scope->addType(MirPath{"usize"}, 0, MirType{MirTypeBuiltin::Usize});

		scope->addType(MirPath{"i8"}, 0, MirType{MirTypeBuiltin::Int8});
		scope->addType(MirPath{"i16"}, 0, MirType{MirTypeBuiltin::Int16});
		scope->addType(MirPath{"i32"}, 0, MirType{MirTypeBuiltin::Int32});
		scope->addType(MirPath{"i64"}, 0, MirType{MirTypeBuiltin::Int64});
		scope->addType(MirPath{"isize"}, 0, MirType{MirTypeBuiltin::Isize});

		scope->addType(MirPath{"f16"}, 0, MirType{MirTypeBuiltin::Float16});
		scope->addType(MirPath{"f32"}, 0, MirType{MirTypeBuiltin::Float32});
		scope->addType(MirPath{"f64"}, 0, MirType{MirTypeBuiltin::Float64});

		scope->addType(MirPath{"void"}, 0, MirType{MirTypeBuiltin::Void});

		return scope;
	}

	static std::shared_ptr<Scope> fn(std::shared_ptr<Scope> parent,
																	 TypeHandle returnType) {
		return std::make_shared<Scope>(parent, returnType, parent->insideLoop);
	}

	static std::shared_ptr<Scope> loop(std::shared_ptr<Scope> parent) {
		return std::make_shared<Scope>(parent, parent->fnReturnType, true);
	}

	static std::shared_ptr<Scope> block(std::shared_ptr<Scope> parent) {
		return std::make_shared<Scope>(parent, parent->fnReturnType,
																	 parent->insideLoop);
	}

	std::optional<TypeHandle> fnReturnType;
	bool insideLoop = false;

	TypeHandle getVariableType(MirIdent ident) const {
		if (auto it = variables.find(ident); it != variables.end()) {
			return it->second;
		}

		if (parent) {
			return parent->getVariableType(ident);
		}

		throw Error(fmt::format("cannot access variable `{}`", ident.ident),
								{{ident.span, "variable not found"}});
	}

	TypeHandle addVariableType(MirIdent ident, TypeHandle type) {
		variables.emplace(ident, type);

		return type;
	}

	TypeHandle addType(MirPath path, std::size_t refCount, MirType type) {
		return addType(MirTypeLit{path, refCount}, type);
	}

	// takes an absolute path from the root of the program
	TypeHandle addType(MirTypeLit path, MirType type) {
		if (auto it = types.find(path); it != types.end()) {
			return it->second;
		}

		auto handle = TypeHandle{nextIndex++};

		symbols.emplace(handle, type);
		types.emplace(path, handle);
		inverseTypes.emplace(handle, path);

		return handle;
	}

	TypeHandle getGenericType(TypeHandle type, MirGenericsInstance generics) {
		std::pair<TypeHandle, MirGenericsInstance> key{type, generics};

		if (auto it = generics.find(key); it != generics.end()) {
			return it->second;
		}

		auto path = getTypeLit(type);
		auto ty = getType(path, 0);

		auto handle = addType(path, generics, ty);

		return handle;
	}

	TypeHandle getType(MirTypeLit lit) {
		return std::visit([&](auto &&arg) { return getType(arg, lit.refCount); },
											lit.item);
	}

	TypeHandle getType(std::shared_ptr<MirArray> array, std::size_t refCount) {
		auto path = MirTypeLit{array, refCount};

		if (auto it = types.find(path); it != types.end()) {
			return it->second;
		}

		return addType(path, MirType{*array});
	}

	TypeHandle getType(MirPath path, std::size_t refCount) {
		// path = anchor + path;

		auto wrap = MirTypeLit{path, refCount};

		if (auto it = types.find(wrap); it != types.end()) {
			return it->second;
		}

		if (refCount > 0) {
			auto inner = getType(path, 0);

			return addType(path, refCount, MirType{MirPointer{inner, refCount}});
		}

		throw std::runtime_error(fmt::format("type not found: {}", path));
	}

	MirTypeLit getTypeLit(TypeHandle handle) const {
		if (auto it = inverseTypes.find(handle); it != inverseTypes.end()) {
			return MirTypeLit{it->second};
		}

		throw std::runtime_error("type not found");
	}

	const MirType &getType(TypeHandle handle) const {
		if (auto it = symbols.find(handle); it != symbols.end()) {
			return it->second;
		}

		throw std::runtime_error("type not found");
	}

private:
	Scope(std::shared_ptr<Scope> parent, std::optional<TypeHandle> fnReturnType,
				bool insideLoop)
			: fnReturnType(fnReturnType), insideLoop(insideLoop), parent(parent) {}

	static std::size_t nextIndex;

	std::shared_ptr<Scope> parent;
	std::unordered_map<MirIdent, TypeHandle> variables;
	std::unordered_map<MirTypeLit, TypeHandle> types;
	std::unordered_map<TypeHandle, MirTypeLit> inverseTypes;
	std::unordered_map<TypeHandle, MirType> symbols;

	// with generics
	std::unordered_map<std::pair<TypeHandle, MirGenericsInstance>, TypeHandle>
			generics;
};

std::size_t Scope::nextIndex = 0;

MirPointerDeref MirPointerDeref::from_hir(std::shared_ptr<Scope> ctx,
																					HirPointerDeref hir) {
	auto expr = MirAssignable::from_hir(ctx, hir.expr);
	auto type = ctx->getType(expr.type);
	auto ptr = std::get_if<MirPointer>(&type.kind);

	// ensure it can be dereferenced
	if (!ptr) {
		throw Error(
				fmt::format("cannot dereference non-pointer type `{}`", type.str(ctx)),
				{{hir.expr.span(), "expected pointer type"}});
	}

	auto ty = ctx->getTypeLit(ptr->type);
	ty.refCount = ptr->refCount - 1;
	auto handle = ctx->getType(ty);

	return MirPointerDeref{hir.span(), handle, expr};
}

MirAssignable MirAssignable::from_hir(std::shared_ptr<Scope> ctx,
																			HirAssignable hir) {
	MirAssignableRootItem root = MirAssignable::from_hir(ctx, hir.root);

	auto type = std::visit(
			[&](auto &&arg) -> TypeHandle {
				if constexpr (std::is_same_v<std::decay_t<decltype(arg)>, MirIdent>) {
					return ctx->getVariableType(arg);
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

MirArray MirArray::from_hir(std::shared_ptr<Scope> ctx, const HirArray &hir) {
	auto path = MirTypeLit::from_hir(ctx, *hir.type);
	auto ty = ctx->getType(path);

	auto size = std::get_if<int>(&hir.size.value);

	if (!size) {
		throw Error("array size must be a constant integer",
								{{hir.size.span(), "expected constant integer"}});
	}

	return MirArray{ty, std::size_t(*size)};
}

MirFieldAccess MirFieldAccess::from_hir(std::shared_ptr<Scope> ctx,
																				TypeHandle parent, HirFieldAccess hir) {
	auto ident = MirIdent::from_hir(hir.ident);
	auto type = ctx->getType(parent);
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

std::string MirType::str(const std::shared_ptr<Scope> ctx,
												 const MirArray &array) const {
	auto inner = ctx->getType(array.type).str(ctx);

	return fmt::format("[{}; {}]", inner, array.size);
}

std::string MirType::str(const std::shared_ptr<Scope> ctx,
												 const MirPointer &ptr) const {
	auto inner = ctx->getType(ptr.type).str(ctx);

	return fmt::format("{:*>{}}{}", "", ptr.refCount, inner);
}

MirStruct MirStruct::from_hir(std::shared_ptr<Scope> ctx, HirStruct hir) {
	std::vector<MirStructField> fields;

	for (auto &field : hir.fields) {
		auto path = MirTypeLit::from_hir(ctx, *field.type);
		auto type = ctx->getType(path);
		auto ident = MirIdent::from_hir(field.ident);

		fields.push_back(MirStructField{ident, type});
	}

	auto ident = MirIdent::from_hir(hir.ident);
	auto struct_ = MirStruct{ctx->getType(MirPath{"void"}, 0), ident, fields};

	struct_.type = ctx->addType(MirPath{ident}, 0, MirType{struct_});

	return struct_;
}

class MirLit {
public:
	Span span;
	TokenLitType value;

	MirLit(TokenLit value) : span(value.span()), value(value.value) {}

	static MirLit from_hir(TokenLit hir) { return MirLit{hir}; }

	TypeHandle type(std::shared_ptr<Scope> ctx) const {
		if (std::holds_alternative<int>(value)) {
			return ctx->getType(MirPath{"i32"}, 0);
		} else if (std::holds_alternative<double>(value)) {
			return ctx->getType(MirPath{"f64"}, 0);
		} else if (std::holds_alternative<std::string>(value)) {
			return ctx->getType(MirPath{"char"}, 1);
		} else if (std::holds_alternative<bool>(value)) {
			return ctx->getType(MirPath{"bool"}, 0);
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

	static MirExpr from_hir(std::shared_ptr<Scope> ctx, HirExpr hir);

	static MirExpr from_hir(std::shared_ptr<Scope> ctx,
													std::shared_ptr<HirAssignable> hir) {
		auto path = MirAssignable::from_hir(ctx, *hir);

		return MirExpr{path, path.type};
	}

	static MirExpr from_hir(std::shared_ptr<Scope> ctx, TokenLit hir) {
		auto lit = MirLit::from_hir(hir);

		return MirExpr{lit, lit.type(ctx)};
	}

	static MirExpr from_hir(std::shared_ptr<Scope> ctx, TokenIdent hir) {
		auto ident = MirIdent::from_hir(hir);
		auto type = ctx->getVariableType(ident);

		return MirExpr{ident, type};
	}

	static MirExpr from_hir(std::shared_ptr<Scope> ctx,
													std::shared_ptr<HirArrayInstance> hir);
	static MirExpr from_hir(std::shared_ptr<Scope> ctx,
													std::shared_ptr<HirStructInstance> hir);

	static MirExpr from_hir(std::shared_ptr<Scope> ctx,
													std::shared_ptr<HirFnCall> hir);

	static MirExpr from_hir(std::shared_ptr<Scope> ctx,
													std::shared_ptr<HirBinOp> hir) {
		auto lhs = MirExpr::from_hir(ctx, hir->lhs);
		auto rhs = MirExpr::from_hir(ctx, hir->rhs);

		if (lhs.type != rhs.type) {
			auto expected = ctx->getType(lhs.type);
			auto given = ctx->getType(rhs.type);

			throw std::runtime_error(fmt::format(
					"unsupported operation `{}` between types `{}` and `{}`",
					TokenOp::str(hir->op.variant), expected.str(ctx), given.str(ctx)));
		}

		auto type = lhs.type;

		bool isInteger = lhs.type == ctx->getType(MirPath{"i32"}, 0);
		bool isFloat = lhs.type == ctx->getType(MirPath{"f64"}, 0);
		bool isBool = lhs.type == ctx->getType(MirPath{"bool"}, 0);

		auto error = [&]() {
			return Error(
					fmt::format("unsupported operation `{}` between types `{}` and `{}`",
											TokenOp::str(hir->op.variant),
											ctx->getType(lhs.type).str(ctx),
											ctx->getType(rhs.type).str(ctx)),
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
				type = ctx->getType(MirPath{"bool"}, 0);
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

	static MirExpr from_hir(std::shared_ptr<Scope> ctx,
													std::shared_ptr<HirUnOp> hir) {
		auto expr = MirExpr::from_hir(ctx, hir->expr);
		auto type = expr.type;

		// TODO: check other types. will need to make it possible to use other
		// types as literals
		if (hir->op.variant == Op::BIT_AND /* ref */) {
			auto lit = ctx->getTypeLit(expr.type);
			lit.refCount++;
			type = ctx->getType(lit);
		} else if (hir->op.variant == Op::NOT) {
			if (type != ctx->getType(MirPath{"bool"}, 0)) {
				throw Error(
						fmt::format("cannot apply operator `not` to type `{}`",
												ctx->getType(type).str(ctx)),
						{{hir->expr.span(), fmt::format("expected bool, found `{}`",
																						ctx->getType(type).str(ctx))}});
			}
		} else if (hir->op.variant == Op::SUB) {
			if (type != ctx->getType(MirPath{"i32"}, 0) &&
					type != ctx->getType(MirPath{"f64"}, 0)) {
				throw Error(fmt::format("cannot apply operator `neg` to type `{}`",
																ctx->getType(type).str(ctx)),
										{{hir->expr.span(),
											fmt::format("expected integer or float, found `{}`",
																	ctx->getType(type).str(ctx))}});
			}
		} else if (hir->op.variant == Op::BIT_NOT) {
			if (type != ctx->getType(MirPath{"i32"}, 0)) {
				throw Error(
						fmt::format("cannot apply operator `~` to type `{}`",
												ctx->getType(type).str(ctx)),
						{{hir->expr.span(), fmt::format("expected i32, found `{}`",
																						ctx->getType(type).str(ctx))}});
			}
		} else {
			throw Error(fmt::format("cannot apply operator `{}` to type `{}`",
															TokenOp::str(hir->op.variant),
															ctx->getType(type).str(ctx)),
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

MirArrayIndex MirArrayIndex::from_hir(std::shared_ptr<Scope> ctx,
																			TypeHandle parent, HirArrayIndex hir) {
	auto expr = MirExpr::from_hir(ctx, hir.expr);
	auto type = ctx->getType(parent);
	auto array = std::get_if<MirArray>(&type.kind);

	if (!array) {
		throw Error(fmt::format("cannot index non-array type `{}`", type.str(ctx)),
								{{hir.expr.span(), "incorrect index here"}});
	}

	// ensure type is i32
	if (expr.type != ctx->getType(MirPath{"i32"}, 0)) {
		throw Error(fmt::format("array index must be an integer. found `{}`",
														ctx->getType(expr.type).str(ctx)),
								{{hir.expr.span(), "incorrect index here"}});
	}

	return MirArrayIndex{hir.span(), array->type, parent,
											 std::make_shared<MirExpr>(expr)};
}

MirAssignableItem MirAssignable::from_hir(std::shared_ptr<Scope> ctx,
																					TypeHandle parent,
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

	static MirArrayInstance from_hir(std::shared_ptr<Scope> ctx,
																	 HirArrayInstance hir) {
		std::optional<TypeHandle> type;
		std::vector<MirExpr> values;
		Span inferredTypeOrigin;

		for (auto &value : hir.values) {
			auto expr = MirExpr::from_hir(ctx, value);

			if (type && expr.type != type) {
				auto expected = ctx->getType(*type);
				auto given = ctx->getType(expr.type);

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
		auto ty = ctx->getType(array, 0);

		return MirArrayInstance{hir.span(), ty, std::move(values), size};
	}
};

class MirStructFieldAssign {
public:
	MirStructFieldAssign(MirIdent ident, MirExpr expr)
			: ident(ident), expr(expr) {}

	MirIdent ident;
	MirExpr expr;

	static MirStructFieldAssign from_hir(std::shared_ptr<Scope> ctx,
																			 HirStructFieldAssign hir) {
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

	static MirStructInstance from_hir(std::shared_ptr<Scope> ctx,
																		HirStructInstance hir) {
		auto path = MirPath::from_hir(hir.path);
		auto type = ctx->getType(path, 0);

		std::vector<MirStructFieldAssign> fields;

		auto struct_ = std::get_if<MirStruct>(&ctx->getType(type).kind);

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
				auto lhs = ctx->getType(found->type);
				auto rhs = ctx->getType(f.expr.type);

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

MirExpr MirExpr::from_hir(std::shared_ptr<Scope> ctx,
													std::shared_ptr<HirStructInstance> hir) {
	auto instance = MirStructInstance::from_hir(ctx, *hir);

	return MirExpr{std::make_shared<MirStructInstance>(instance), instance.type};
}

MirExpr MirExpr::from_hir(std::shared_ptr<Scope> ctx,
													std::shared_ptr<HirArrayInstance> hir) {
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

	static MirFnCall from_hir(std::shared_ptr<Scope> ctx, HirFnCall hir);
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

	static MirAssign from_hir(std::shared_ptr<Scope> ctx, HirAssign hir) {
		auto path = MirIdent::from_hir(hir.ident.ident);
		auto expr = MirExpr::from_hir(ctx, hir.expr);

		if (hir.ident.type) {
			auto typePath = MirTypeLit::from_hir(ctx, *hir.ident.type);
			auto type = ctx->getType(typePath);

			if (expr.type != type) {
				auto lhs = ctx->getType(type);
				auto rhs = ctx->getType(expr.type);

				throw Error(fmt::format("type mismatch for assignment of `{}`. "
																"expected `{}`, found `{}`",
																path.value(), lhs.str(ctx), rhs.str(ctx)),
										{{expr.span(), "incorrect value here"}});
			}
		}

		ctx->addVariableType(path, expr.type);

		return MirAssign{path, expr};
	}
};

class MirReassign {
public:
	MirReassign(MirAssignable path, MirExpr expr) : path(path), expr(expr) {}

	MirAssignable path;
	MirExpr expr;

	static MirReassign from_hir(std::shared_ptr<Scope> ctx, HirReassign hir) {
		auto path = MirAssignable::from_hir(ctx, hir.path);
		auto expr = MirExpr::from_hir(ctx, hir.expr);

		if (path.type != expr.type) {
			auto lhs = ctx->getType(path.type);
			auto rhs = ctx->getType(expr.type);

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

	static MirReturn from_hir(std::shared_ptr<Scope> ctx, HirReturn hir) {
		std::optional<MirExpr> expr;

		if (hir.expr) {
			expr = MirExpr::from_hir(ctx, *hir.expr);
		}

		if (auto ret = ctx->fnReturnType) {
			auto lhs = ctx->getType(*ret);
			auto vd = ctx->getType(MirPath{"void"}, 0);

			if (expr && expr->type != *ret) {
				auto rhs = ctx->getType(expr->type);

				throw Error(
						fmt::format("type mismatch for return. expected `{}`, found `{}`",
												lhs.str(ctx), rhs.str(ctx)),
						{{expr->span(), "incorrect value here"}});
			} else if (!expr && *ret != vd) {
				throw Error(
						fmt::format("type mismatch for return. expected `{}`, found `void`",
												lhs.str(ctx)),
						{{hir.ret.span(), "incorrect value returned here"}});
			}
		}

		if (!ctx->fnReturnType) {
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

	static MirBreak from_hir(std::shared_ptr<Scope> ctx, HirBreak hir) {
		if (!ctx->insideLoop) {
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

	static MirContinue from_hir(std::shared_ptr<Scope> ctx, HirContinue hir) {
		if (!ctx->insideLoop) {
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

	static MirBlock from_hir(std::shared_ptr<Scope> ctx, HirBlock hir) {
		std::vector<MirBlockItem> items;

		auto scope = Scope::block(ctx);

		for (auto &hir : hir.stmts) {
			items.push_back(std::visit(
					[scope](auto &&hir) { return MirBlock::lower(scope, hir); },
					hir.stmt));
		}

		return MirBlock{items};
	}

	static MirBlockItem lower(std::shared_ptr<Scope> ctx, HirBreak hir);
	static MirBlockItem lower(std::shared_ptr<Scope> ctx, HirContinue hir);
	static MirBlockItem lower(std::shared_ptr<Scope> ctx, HirAssign hir);
	static MirBlockItem lower(std::shared_ptr<Scope> ctx, HirReassign hir);
	static MirBlockItem lower(std::shared_ptr<Scope> ctx, HirReturn hir);
	static MirBlockItem lower(std::shared_ptr<Scope> ctx, HirFnCall hir);
	static MirBlockItem lower(std::shared_ptr<Scope> ctx,
														std::shared_ptr<HirFor> hir);
	static MirBlockItem lower(std::shared_ptr<Scope> ctx,
														std::shared_ptr<HirIf> hir);
	static MirBlockItem lower(std::shared_ptr<Scope> ctx,
														std::shared_ptr<HirWhile> hir);
};

class MirFnParam {
public:
	MirFnParam(MirIdent ident, TypeHandle type) : ident(ident), type(type) {}

	MirIdent ident;
	TypeHandle type;

	static MirFnParam from_hir(std::shared_ptr<Scope> ctx, HirTypedIdent hir) {
		auto ident = MirIdent::from_hir(hir.ident);

		if (auto type = hir.type) {
			auto path = MirTypeLit::from_hir(ctx, *type);
			auto ty = ctx->getType(path);

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

	static MirFnSignature from_hir(std::shared_ptr<Scope> ctx_, HirExtern hir) {
		auto ctx = Scope::block(ctx_);

		std::vector<MirFnParam> params;

		for (auto &param : hir.params) {
			auto p = MirFnParam::from_hir(ctx, param);

			ctx->addVariableType(p.ident, p.type);
			params.push_back(p);
		}

		auto ident = MirIdent::from_hir(hir.ident);
		// `void`
		auto fallback = HirType{
				Span{}, HirPath{Span{}, {TokenIdent{Span{}, "void"}}, std::nullopt}, 0};
		auto ret =
				ctx->getType(MirTypeLit::from_hir(ctx, hir.ret.value_or(fallback)));

		auto sig = MirFnSignature{ret, ident, params, ret};
		sig.variadic = hir.variadic;

		auto shared = std::make_shared<MirFnSignature>(sig);
		auto ty = ctx_->addType(MirPath{ident}, 0, MirType{shared});

		shared->type = ty;
		ctx_->addVariableType(ident, ty);

		return sig;
	}

	static MirFnSignature from_hir(std::shared_ptr<Scope> ctx, HirFn hir) {
		auto fallback = HirType{
				Span{}, HirPath{Span{}, {TokenIdent{Span{}, "void"}}, std::nullopt}, 0};
		auto ret =
				ctx->getType(MirTypeLit::from_hir(ctx, hir.ret.value_or(fallback)));

		std::vector<MirFnParam> params;

		for (auto &param : hir.params) {
			auto p = MirFnParam::from_hir(ctx, param);
			params.push_back(p);
		}

		auto ident = MirIdent::from_hir(hir.ident);
		auto sig = MirFnSignature{ret, ident, params, ret};

		auto shared = std::make_shared<MirFnSignature>(sig);
		auto ty = ctx->addType(MirPath{ident}, 0, MirType{shared});

		shared->type = ty;
		ctx->addVariableType(ident, ty);

		return sig;
	}
};

class MirFn : public MirFnSignature {
public:
	MirFn(TypeHandle type, MirIdent ident, std::vector<MirFnParam> params,
				TypeHandle ret, MirBlock block, MirGenerics generics)
			: MirFnSignature(type, ident, params, ret), block(block),
				generics(generics) {}
	MirFn(MirFnSignature sig, MirBlock block, MirGenerics generics)
			: MirFnSignature(sig), block(block), generics(generics) {}

	MirBlock block;
	MirGenerics generics;

	static MirFn from_hir(std::shared_ptr<Scope> ctx_, HirFn hir) {
		auto sig = MirFnSignature::from_hir(ctx_, hir);
		auto ctx = Scope::fn(ctx_, sig.ret);

		auto generics =
				MirGenerics::from_hir(hir.generics.value_or(HirGenerics::empty()));

		for (auto &param : sig.params) {
			ctx->addVariableType(param.ident, param.type);
		}

		auto block = MirBlock::from_hir(ctx, hir.block);

		if (block.items.empty() ||
				!std::holds_alternative<MirReturn>(block.items.back())) {
			if (sig.ret != ctx->getType(MirPath{"void"}, 0)) {
				throw Error("non-void function body must end with a return statement",
										{{hir.ident.span(), "function defined here"}});
			} else {
				block.items.push_back(MirReturn{std::nullopt});
			}
		}

		return MirFn{sig, block, generics};
	}
};

MirFnCall MirFnCall::from_hir(std::shared_ptr<Scope> ctx, HirFnCall hir) {
	auto path = MirPath::from_hir(hir.path);

	if (path.parts.size() != 1) {
		throw std::runtime_error("modules not implemented");
	}

	auto fnType = ctx->getVariableType(path.parts[0]);
	auto maybeFn = ctx->getType(fnType);
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
			auto expected = ctx->getType(fn->params[i].type);
			auto given = ctx->getType(arg.type);

			throw Error(
					fmt::format(
							"mismatched types for argument `{}` in call to function `{}`",
							fn->params[i].ident.value(), path),
					{{arg.span(), fmt::format("expected `{}`, found `{}`",
																		expected.str(ctx), given.str(ctx))},
					 {fn->params[i].ident.span, "argument defined here"}});
		}

		args.push_back(arg);
	}

	return MirFnCall{hir.span(), path, args, fn->ret};
}

std::string MirType::str(const std::shared_ptr<Scope> ctx,
												 const std::shared_ptr<MirFnSignature> &fn) const {
	std::string formatted = "fn(";

	for (std::size_t i = 0; i < fn->params.size(); i++) {
		formatted += ctx->getType(fn->params[i].type).str(ctx);

		if (i + 1 < fn->params.size()) {
			formatted += ", ";
		}
	}

	formatted += "): ";
	formatted += ctx->getType(fn->ret).str(ctx);

	return formatted;
}

MirExpr MirExpr::from_hir(std::shared_ptr<Scope> ctx,
													std::shared_ptr<HirFnCall> hir) {
	auto call = MirFnCall::from_hir(ctx, *hir);
	auto handle = ctx->getVariableType(call.path.parts[0]);
	auto type = ctx->getType(handle);

	if (auto fn = std::get_if<std::shared_ptr<MirFnSignature>>(&type.kind)) {
		auto ret = (*fn)->ret;

		return MirExpr{std::make_shared<MirFnCall>(call), ret};
	} else {
		throw std::runtime_error("not a function");
	}
}

MirExpr MirExpr::from_hir(std::shared_ptr<Scope> ctx, HirExpr hir) {
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

	static MirIf from_hir(std::shared_ptr<Scope> ctx,
												std::shared_ptr<HirIf> hir) {
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

	static MirStmt lower(std::shared_ptr<Scope> ctx, HirAssign hir) {
		return MirAssign::from_hir(ctx, hir);
	}

	static MirStmt lower(std::shared_ptr<Scope> ctx, HirReassign hir) {
		return MirReassign::from_hir(ctx, hir);
	}

	static MirStmt lower(std::shared_ptr<Scope> ctx, HirReturn hir) {
		return MirReturn::from_hir(ctx, hir);
	}

	static MirStmt lower(std::shared_ptr<Scope> ctx, HirFnCall hir) {
		return MirFnCall::from_hir(ctx, hir);
	}

	static MirLoop from_hir(std::shared_ptr<Scope> ctx_, HirFor hir) {
		std::optional<MirStmt> setup;
		std::optional<MirExpr> cond;
		std::optional<MirStmt> step;

		auto ctx = Scope::loop(ctx_);

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

	static MirLoop from_hir(std::shared_ptr<Scope> ctx_, HirWhile hir) {
		auto ctx = Scope::loop(ctx_);

		auto cond = MirExpr::from_hir(ctx, hir.cond);
		auto block = MirBlock::from_hir(ctx, hir.block);

		return MirLoop{std::nullopt, cond, std::nullopt, block};
	}

	static MirLoop from_hir(std::shared_ptr<Scope> ctx_, HirLoop hir) {
		auto ctx = Scope::loop(ctx_);
		auto block = MirBlock::from_hir(ctx, hir.block);

		return MirLoop{std::nullopt, std::nullopt, std::nullopt, block};
	}

private:
	static MirStmt lower(std::shared_ptr<Scope> ctx, HirStmtItem hir) {
		throw std::runtime_error("not implemented");
	}
};

using Mir = std::variant<MirFn, MirFnSignature, MirStruct, MirLit, MirExpr,
												 MirLoop, MirIf, MirAssign, MirReassign, MirReturn>;

MirBlockItem MirBlock::lower(std::shared_ptr<Scope> ctx, HirBreak hir) {
	return MirBreak::from_hir(ctx, hir);
}

MirBlockItem MirBlock::lower(std::shared_ptr<Scope> ctx, HirContinue hir) {
	return MirContinue::from_hir(ctx, hir);
}

MirBlockItem MirBlock::lower(std::shared_ptr<Scope> ctx, HirAssign hir) {
	return MirAssign::from_hir(ctx, hir);
}

MirBlockItem MirBlock::lower(std::shared_ptr<Scope> ctx, HirReassign hir) {
	return MirReassign::from_hir(ctx, hir);
}

MirBlockItem MirBlock::lower(std::shared_ptr<Scope> ctx, HirReturn hir) {
	return MirReturn::from_hir(ctx, hir);
}

MirBlockItem MirBlock::lower(std::shared_ptr<Scope> ctx, HirFnCall hir) {
	return MirFnCall::from_hir(ctx, hir);
}

MirBlockItem MirBlock::lower(std::shared_ptr<Scope> ctx,
														 std::shared_ptr<HirFor> hir) {
	auto mir = MirLoop::from_hir(ctx, *hir);

	return std::make_shared<MirLoop>(std::move(mir));
}

MirBlockItem MirBlock::lower(std::shared_ptr<Scope> ctx,
														 std::shared_ptr<HirIf> hir) {
	auto mir = MirIf::from_hir(ctx, hir);

	return std::make_shared<MirIf>(std::move(mir));
}

MirBlockItem MirBlock::lower(std::shared_ptr<Scope> ctx,
														 std::shared_ptr<HirWhile> hir) {
	auto mir = MirLoop::from_hir(ctx, *hir);

	return std::make_shared<MirLoop>(std::move(mir));
}

class HirLowerer {
public:
	HirLowerer(Parser parser) : ctx(Scope::root()), parser(parser) {}

	std::shared_ptr<Scope> ctx;

	std::vector<Mir> lower() {
		std::vector<Hir> hir = parser.collect();

		std::vector<Mir> mir;

		for (auto &hir : hir) {
			mir.push_back(lower(hir));
		}

		// make sure there's a main function with no args and that returns i32
		auto mainHandle = ctx->getVariableType(MirIdent{"main"});
		auto main = ctx->getType(mainHandle);

		if (auto fn = std::get_if<std::shared_ptr<MirFnSignature>>(&main.kind)) {
			if ((*fn)->params.size() != 0) {
				throw std::runtime_error("main function must have no arguments");
			}

			if ((*fn)->ret != ctx->getType(MirPath{"i32"}, 0)) {
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

	Mir lower(BaseHir hir) {
		throw Error("not allowed at the top level",
								{{hir.span(), "not allowed here"}});
	}

	Mir lower(HirFn hir) { return MirFn::from_hir(ctx, hir); }
	Mir lower(HirStruct hir) { return MirStruct::from_hir(ctx, hir); }
	Mir lower(HirExtern hir) { return MirFnSignature::from_hir(ctx, hir); }

private:
	Parser parser;
};

#pragma once

#include <cstddef>
#include <filesystem>
#include <functional>
#include <memory>
#include <vector>

#include "basic_parser.hpp"
#include "token.hpp"

class BaseHir {
public:
	BaseHir(Span span) : span_(span) {}

	Span span() const { return span_; }

protected:
	Span span_;
};

class HirBinOp;
class HirUnOp;
class HirFnCall;
class HirStructInstance;
class HirArrayInstance;
class HirAssignable;

using HirExprItem =
		std::variant<TokenLit, TokenIdent, std::shared_ptr<HirAssignable>,
								 std::shared_ptr<HirArrayInstance>,
								 std::shared_ptr<HirStructInstance>, std::shared_ptr<HirBinOp>,
								 std::shared_ptr<HirUnOp>, std::shared_ptr<HirFnCall>>;

class HirExpr : public BaseHir {
public:
	HirExprItem expr;

	HirExpr(Span span, HirExprItem expr) : BaseHir(span), expr(std::move(expr)) {}

	static HirExpr parse(BasicParser &t);

	static std::optional<HirExpr> tryParse(BasicParser &t) {
		auto tx = t.tx();

		try {
			auto hir = parse(t);
			tx.commit();
			return hir;
		} catch (std::runtime_error &e) {
			return std::nullopt;
		}
	}

private:
	template <int P> static HirExpr parse(BasicParser &t);

	template <int P> static std::optional<HirExpr> tryParse(BasicParser &t) {
		auto tx = t.tx();

		try {
			auto hir = parse<P>(t);
			tx.commit();
			return hir;
		} catch (std::runtime_error &e) {
			return std::nullopt;
		}
	}
};

class HirGenerics : public BaseHir {
public:
	HirGenerics(Span span, std::vector<TokenIdent> generics)
			: BaseHir(span), generics(std::move(generics)) {}

	std::vector<TokenIdent> generics;

	static HirGenerics empty() { return HirGenerics{Span{}, {}}; }

	static HirGenerics parse(BasicParser &t) {
		auto langle = t.consume<TokenDelim>(Delim::LANGLE);
		std::vector<TokenIdent> generics;

		do {
			auto ident = t.consume<TokenIdent>();
			generics.push_back(std::move(ident));
		} while (t.tryConsume<TokenDelim>(Delim::COMMA));

		auto rangle = t.consume<TokenDelim>(Delim::RANGLE);

		return HirGenerics{langle.span().merge(rangle.span()), std::move(generics)};
	}
};

class HirArrayIndex : public BaseHir {
public:
	HirArrayIndex(Span span, HirExpr expr)
			: BaseHir(span), expr(std::move(expr)) {}

	HirExpr expr;

	static HirArrayIndex parse(BasicParser &t) {
		auto lbracket = t.consume<TokenDelim>(Delim::LBRACKET);
		auto expr = HirExpr::parse(t);
		auto rbracket = t.consume<TokenDelim>(Delim::RBRACKET);

		return HirArrayIndex{lbracket.span().merge(rbracket.span()),
												 std::move(expr)};
	}
};

class HirFieldAccess : public BaseHir {
public:
	HirFieldAccess(Span span, TokenIdent ident)
			: BaseHir(span), ident(std::move(ident)) {}

	TokenIdent ident;

	static HirFieldAccess parse(BasicParser &t) {
		auto dot = t.consume<TokenDelim>(Delim::PERIOD);
		auto ident = t.consume<TokenIdent>();

		return HirFieldAccess{dot.span().merge(ident.span()), std::move(ident)};
	}
};

using HirAssignableItem = std::variant<HirArrayIndex, HirFieldAccess>;

class HirPointerDeref;

using HirAssignableRootItem =
		std::variant<TokenIdent, std::shared_ptr<HirPointerDeref>,
								 std::shared_ptr<HirAssignable>>;

class HirAssignable : public BaseHir {
public:
	HirAssignable(Span span, HirAssignableRootItem root,
								std::vector<HirAssignableItem> parts)
			: BaseHir(span), root(std::move(root)), parts(std::move(parts)) {}

	HirAssignableRootItem root;
	std::vector<HirAssignableItem> parts;

	static HirAssignable parse(BasicParser &t);

	static std::optional<HirAssignable> tryParse(BasicParser &t) {
		auto tx = t.tx();

		try {
			auto hir = parse(t);
			tx.commit();
			return hir;
		} catch (std::runtime_error &e) {
			return std::nullopt;
		}
	}
};

class HirPointerDeref : public BaseHir {
public:
	HirPointerDeref(Span span, HirAssignable expr)
			: BaseHir(span), expr(std::move(expr)) {}

	HirAssignable expr;

	static HirPointerDeref parse(BasicParser &t) {
		auto op = t.consume<TokenOp>(Op::MUL);
		auto expr = HirAssignable::parse(t);

		return HirPointerDeref{op.span().merge(expr.span()), std::move(expr)};
	}
};

HirAssignable HirAssignable::parse(BasicParser &t) {
	Span span;
	Span end;
	std::optional<HirAssignableRootItem> root;
	std::vector<HirAssignableItem> parts;

	auto first = t.tryConsume<TokenDelim>(Delim::LPAREN);

	if (first) {
		span = first->span();
		auto nested = HirAssignable::parse(t);
		t.consume<TokenDelim>(Delim::RPAREN);
		root = std::make_shared<HirAssignable>(std::move(nested));
	} else if (t.peek<TokenOp>(Op::MUL)) {
		auto deref = HirPointerDeref::parse(t);
		span = deref.span();
		root = std::make_shared<HirPointerDeref>(std::move(deref));
	} else {
		auto ident = t.consume<TokenIdent>();
		span = ident.span();
		root = std::move(ident);
	}

	end = span;

	for (;;) {
		if (t.peek<TokenDelim>(Delim::LBRACKET)) {
			auto index = HirArrayIndex::parse(t);
			end = index.span();
			parts.push_back(std::move(index));
		} else if (t.peek<TokenDelim>(Delim::PERIOD)) {
			auto field = HirFieldAccess::parse(t);
			end = field.span();
			parts.push_back(std::move(field));
		} else {
			break;
		}
	}

	return HirAssignable{span.merge(end), *root, std::move(parts)};
}

class HirBinOp : public BaseHir {
public:
	HirBinOp(Span span, TokenOp op, HirExpr lhs, HirExpr rhs)
			: BaseHir(span), op(op), lhs(std::move(lhs)), rhs(std::move(rhs)) {}

	TokenOp op;
	HirExpr lhs;
	HirExpr rhs;
};

class HirUnOp : public BaseHir {
public:
	HirUnOp(Span span, TokenOp op, HirExpr expr)
			: BaseHir(span), op(op), expr(std::move(expr)) {}

	TokenOp op;
	HirExpr expr;

	static HirUnOp parse(BasicParser &t) {
		auto op = t.consume<TokenOp>(Op::SUB, Op::NOT);
		auto expr = HirExpr::parse(t);

		return HirUnOp{op.span().merge(expr.span()), op, std::move(expr)};
	}
};

class HirPath : public BaseHir {
public:
	HirPath(Span span, std::vector<TokenIdent> parts,
					std::optional<HirGenerics> generics)
			: BaseHir(span), parts(std::move(parts)), generics(std::move(generics)) {}

	std::vector<TokenIdent> parts;
	std::optional<HirGenerics> generics;

	static HirPath empty() { return HirPath{Span{}, {}, std::nullopt}; }

	static HirPath parse(BasicParser &t) {
		std::vector<TokenIdent> parts;

		do {
			if (auto super = t.tryConsume<TokenKeyword>(Keyword::SUPER)) {
				parts.push_back(
						TokenIdent{super->span(), TokenKeyword::str(Keyword::SUPER)});
			} else {
				parts.push_back(t.consume<TokenIdent>());
			}
		} while (t.tryConsume<TokenDelim>(Delim::COLON_COLON));

		Span span = parts.front().span().merge(parts.back().span());

		std::optional<HirGenerics> generics;

		if (t.peek<TokenDelim>(Delim::LANGLE)) {
			generics = HirGenerics::parse(t);
			span = span.merge(generics->span());
		}

		return HirPath{span, std::move(parts), generics};
	}

	// == with a HirPath only compares the parts (not the span)
	bool operator==(const HirPath &other) const { return parts == other.parts; }

	// == with a TokenIdent compares true if there's only one part and it's equal
	bool operator==(const TokenIdent &other) const {
		return parts.size() == 1 && parts.front() == other;
	}

	// hash only hashes the parts (not the span)
	std::size_t hash() const {
		std::size_t h = 0;

		for (auto &part : parts) {
			h ^= part.hash();
		}

		return h;
	}

	HirPath join(const HirPath &other) const {
		std::vector<TokenIdent> newParts = parts;

		for (auto &part : other.parts) {
			if (part.value_ == "super") {
				newParts.pop_back();
			} else {
				newParts.push_back(part);
			}
		}

		return HirPath{span(), std::move(newParts), other.generics};
	}

	HirPath join(const TokenIdent &ident) const {
		std::vector<TokenIdent> newParts = parts;
		newParts.push_back(ident);

		return HirPath{span(), std::move(newParts), std::nullopt};
	}

	std::filesystem::path toPath(std::filesystem::path root) const {
		std::filesystem::path path = root;

		for (auto &part : parts) {
			if (part.value_ == "super") {
				path = path.parent_path();
			} else {
				path /= part.value_;
			}
		}

		return path;
	}
};

class HirArray;

using HirTypeItem = std::variant<HirPath, std::shared_ptr<HirArray>>;

class HirType : public BaseHir {
public:
	HirType(Span span, HirTypeItem item, std::size_t refCount)
			: BaseHir(span), refCount(refCount), item(std::move(item)) {}

	Span span;
	std::size_t refCount;
	HirTypeItem item;

	static HirType parse(BasicParser &t);
};

class HirArray : public BaseHir {
public:
	HirArray(Span span, std::shared_ptr<HirType> type, TokenLit size)
			: BaseHir(span), type(std::move(type)), size(std::move(size)) {}

	std::shared_ptr<HirType> type;
	TokenLit size;

	static HirArray parse(BasicParser &t) {
		auto lbracket = t.consume<TokenDelim>(Delim::LBRACKET);
		auto type = std::make_shared<HirType>(HirType::parse(t));
		t.consume<TokenDelim>(Delim::SEMICOLON);
		auto size = t.consume<TokenLit>();
		auto rbracket = t.consume<TokenDelim>(Delim::RBRACKET);

		return HirArray{lbracket.span().merge(rbracket.span()), type, size};
	}
};

HirType HirType::parse(BasicParser &t) {
	std::size_t refCount = 0;

	while (t.tryConsume<TokenOp>(Op::MUL)) {
		refCount++;
	}

	if (t.peek<TokenDelim>(Delim::LBRACKET)) {
		auto arr = HirArray::parse(t);
		return HirType{arr.span(), std::make_shared<HirArray>(std::move(arr)),
									 refCount};
	}

	auto item = HirPath::parse(t);

	return HirType{item.span(), std::move(item), refCount};
}

template <> struct std::hash<HirPath> {
	std::size_t operator()(const HirPath &path) const { return path.hash(); }
};

class HirTypedIdent : public BaseHir {
public:
	HirTypedIdent(Span span, TokenIdent ident, std::optional<HirType> type)
			: BaseHir(span), ident(ident), type(std::move(type)) {}

	TokenIdent ident;
	std::optional<HirType> type;

	static HirTypedIdent parse(BasicParser &t) {
		auto ident = t.consume<TokenIdent>();
		Span span = ident.span();

		std::optional<HirType> type;

		if (auto colon = t.tryConsume<TokenDelim>(Delim::COLON)) {
			auto ty = HirType::parse(t);

			span = span.merge(ty.span);
			type = std::move(ty);
		}

		return HirTypedIdent{span, ident, type};
	}
};

class HirAssign : public BaseHir {
public:
	HirAssign(Span span, TokenKeyword let, HirTypedIdent ident, TokenOp eq,
						HirExpr expr)
			: BaseHir(span), let(let), ident(std::move(ident)), eq(eq),
				expr(std::move(expr)) {}

	TokenKeyword let;
	HirTypedIdent ident;
	TokenOp eq;
	HirExpr expr;

	static HirAssign parse(BasicParser &t) {
		auto let = t.tryConsume<TokenKeyword>(Keyword::LET);

		if (!let) {
			throw std::runtime_error("expected let");
		}

		auto ident = HirTypedIdent::parse(t);
		auto eq = t.consume<TokenOp>(Op::EQ);
		auto expr = HirExpr::parse(t);

		return HirAssign{let->span().merge(expr.span()), *let, ident, eq,
										 std::move(expr)};
	}

	static std::optional<HirAssign> tryParse(BasicParser &t) {
		auto tx = t.tx();

		try {
			auto hir = parse(t);
			tx.commit();
			return hir;
		} catch (std::runtime_error &e) {
			return std::nullopt;
		}
	}
};

class HirReassign : public BaseHir {
public:
	HirReassign(Span span, HirAssignable path, TokenOp eq, HirExpr expr)
			: BaseHir(span), path(path), eq(eq), expr(std::move(expr)) {}

	HirAssignable path;
	TokenOp eq;
	HirExpr expr;

	static HirReassign parse(BasicParser &t) {
		auto path = HirAssignable::parse(t);
		auto eq = t.consume<TokenOp>(Op::EQ);
		auto expr = HirExpr::parse(t);

		return HirReassign{path.span().merge(expr.span()), path, eq,
											 std::move(expr)};
	}

	static std::optional<HirReassign> tryParse(BasicParser &t) {
		auto tx = t.tx();

		try {
			auto hir = parse(t);
			tx.commit();
			return hir;
		} catch (std::runtime_error &e) {
			return std::nullopt;
		}
	}
};

class HirReturn : public BaseHir {
public:
	HirReturn(Span span, TokenKeyword ret, std::optional<HirExpr> expr)
			: BaseHir(span), ret(ret), expr(std::move(expr)) {}

	TokenKeyword ret;
	std::optional<HirExpr> expr;

	static HirReturn parse(BasicParser &t) {
		auto ret = t.tryConsume<TokenKeyword>(Keyword::RETURN);

		if (!ret) {
			throw std::runtime_error("expected return");
		}

		std::optional<HirExpr> expr;
		Span span = ret->span();

		if (!t.peek<TokenDelim>(Delim::SEMICOLON)) {
			expr = HirExpr::parse(t);
			span = span.merge(expr->span());
		}

		return HirReturn{span, *ret, std::move(expr)};
	}

	static std::optional<HirReturn> tryParse(BasicParser &t) {
		auto tx = t.tx();

		try {
			auto hir = parse(t);
			tx.commit();
			return hir;
		} catch (std::runtime_error &e) {
			return std::nullopt;
		}
	}
};

class HirBreak : public BaseHir {
public:
	HirBreak(Span span, TokenKeyword br) : BaseHir(span), br(br) {}

	TokenKeyword br;

	static HirBreak parse(BasicParser &t) {
		auto br = t.tryConsume<TokenKeyword>(Keyword::BREAK);

		if (!br) {
			throw std::runtime_error("expected break");
		}

		return HirBreak{br->span(), *br};
	}

	static std::optional<HirBreak> tryParse(BasicParser &t) {
		auto tx = t.tx();

		try {
			auto hir = parse(t);
			tx.commit();
			return hir;
		} catch (std::runtime_error &e) {
			return std::nullopt;
		}
	}
};

class HirContinue : public BaseHir {
public:
	HirContinue(Span span, TokenKeyword cont) : BaseHir(span), cont(cont) {}

	TokenKeyword cont;

	static HirContinue parse(BasicParser &t) {
		auto cont = t.tryConsume<TokenKeyword>(Keyword::CONTINUE);

		if (!cont) {
			throw std::runtime_error("expected continue");
		}

		return HirContinue{cont->span(), *cont};
	}

	static std::optional<HirContinue> tryParse(BasicParser &t) {
		auto tx = t.tx();

		try {
			auto hir = parse(t);
			tx.commit();
			return hir;
		} catch (std::runtime_error &e) {
			return std::nullopt;
		}
	}
};

class HirFnCall : public BaseHir {
public:
	HirFnCall(Span span, HirPath path, TokenDelim lparen,
						std::vector<HirExpr> args, TokenDelim rparen,
						std::optional<HirGenerics> generics)
			: BaseHir(span), path(std::move(path)), lparen(lparen),
				args(std::move(args)), rparen(rparen), generics(generics) {}

	HirPath path;
	TokenDelim lparen;
	std::vector<HirExpr> args;
	TokenDelim rparen;
	std::optional<HirGenerics> generics;

	static HirFnCall parse(BasicParser &t) {
		auto path = HirPath::parse(t);

		std::optional<HirGenerics> generics;

		if (t.peek<TokenDelim>(Delim::LANGLE)) {
			generics = HirGenerics::parse(t);
		}

		auto lparen = t.tryConsume<TokenDelim>(Delim::LPAREN);

		if (!lparen) {
			throw std::runtime_error("expected lparen");
		}

		auto rparen = t.peek<TokenDelim>(Delim::RPAREN);

		std::vector<HirExpr> args;

		while (!rparen) {
			args.push_back(HirExpr::parse(t));

			rparen = t.peek<TokenDelim>(Delim::RPAREN);

			if (rparen) {
				break;
			} else {
				t.consume<TokenDelim>(Delim::COMMA);
			}
		}

		t.tryConsume<TokenDelim>(Delim::COMMA);
		rparen = t.consume<TokenDelim>(Delim::RPAREN);

		return HirFnCall{path.span().merge(rparen->span()),
										 path,
										 *lparen,
										 std::move(args),
										 *rparen,
										 generics};
	}

	static std::optional<HirFnCall> tryParse(BasicParser &t) {
		auto tx = t.tx();

		try {
			auto hir = parse(t);
			tx.commit();
			return hir;
		} catch (std::runtime_error &e) {
			return std::nullopt;
		}
	}
};

class HirFor;
class HirIf;
class HirWhile;

using HirStmtItem =
		std::variant<HirAssign, HirReassign, HirReturn, HirFnCall, HirBreak,
								 HirContinue, std::shared_ptr<HirFor>, std::shared_ptr<HirIf>,
								 std::shared_ptr<HirWhile>>;

class HirStmt : public BaseHir {
public:
	HirStmtItem stmt;

	HirStmt(Span span, HirStmtItem stmt) : BaseHir(span), stmt(std::move(stmt)) {}

	bool needsSemi() const {
		return std::holds_alternative<HirAssign>(stmt) ||
					 std::holds_alternative<HirReassign>(stmt) ||
					 std::holds_alternative<HirReturn>(stmt) ||
					 std::holds_alternative<HirFnCall>(stmt) ||
					 std::holds_alternative<HirBreak>(stmt) ||
					 std::holds_alternative<HirContinue>(stmt);
	}

	static HirStmt parse(BasicParser &t);

	static std::optional<HirStmt> tryParse(BasicParser &t) {
		auto tx = t.tx();

		try {
			auto hir = parse(t);
			tx.commit();
			return hir;
		} catch (std::runtime_error &e) {
			return std::nullopt;
		}
	}
};

class HirBlock : public BaseHir {
public:
	TokenDelim lbrace;
	std::vector<HirStmt> stmts;
	TokenDelim rbrace;

	HirBlock(Span span, TokenDelim lbrace, std::vector<HirStmt> stmts,
					 TokenDelim rbrace)
			: BaseHir(span), lbrace(lbrace), stmts(std::move(stmts)), rbrace(rbrace) {
	}

	static HirBlock parse(BasicParser &t) {
		auto lbrace = t.consume<TokenDelim>(Delim::LBRACE);
		auto rbrace = t.peek<TokenDelim>(Delim::RBRACE);

		std::vector<HirStmt> stmts;

		while (!rbrace) {
			auto stmt = HirStmt::parse(t);

			if (stmt.needsSemi()) {
				t.consume<TokenDelim>(Delim::SEMICOLON);
			}

			stmts.push_back(std::move(stmt));
			rbrace = t.peek<TokenDelim>(Delim::RBRACE);
		}

		rbrace = t.consume<TokenDelim>(Delim::RBRACE);

		return HirBlock{lbrace.span().merge(rbrace->span()), lbrace,
										std::move(stmts), *rbrace};
	}
};

class HirFn : public BaseHir {
public:
	HirFn(Span span, TokenKeyword fn, TokenIdent ident,
				std::vector<HirTypedIdent> params, HirBlock block,
				std::optional<HirType> ret, std::optional<HirGenerics> generics)
			: BaseHir(span), fn(fn), ident(ident), params(std::move(params)),
				block(std::move(block)), ret(ret), generics(generics) {}

	bool export_ = false;
	TokenKeyword fn;
	TokenIdent ident;
	std::vector<HirTypedIdent> params;

	HirBlock block;
	std::optional<HirType> ret;
	std::optional<HirGenerics> generics;

	static HirFn parse(BasicParser &t) {
		auto fn = t.tryConsume<TokenKeyword>(Keyword::FN);

		if (!fn) {
			throw std::runtime_error("expected fn");
		}

		auto ident = t.consume<TokenIdent>();

		std::optional<HirGenerics> generics;

		if (t.peek<TokenDelim>(Delim::LANGLE)) {
			generics = HirGenerics::parse(t);
		}

		t.consume<TokenDelim>(Delim::LPAREN);
		auto rparen = t.peek<TokenDelim>(Delim::RPAREN);

		std::vector<HirTypedIdent> params;

		while (!rparen) {
			auto param = HirTypedIdent::parse(t);
			params.push_back(param);

			rparen = t.peek<TokenDelim>(Delim::RPAREN);

			if (rparen) {
				break;
			} else {
				t.consume<TokenDelim>(Delim::COMMA);
			}
		}

		t.tryConsume<TokenDelim>(Delim::COMMA);
		rparen = t.consume<TokenDelim>(Delim::RPAREN);

		std::optional<HirType> ret;

		if (t.tryConsume<TokenDelim>(Delim::COLON)) {
			ret = HirType::parse(t);
		}

		auto block = HirBlock::parse(t);

		return HirFn{fn->span().merge(block.span()),
								 *fn,
								 ident,
								 params,
								 block,
								 ret,
								 generics};
	}
};

class HirStructFieldAssign : public BaseHir {
public:
	HirStructFieldAssign(Span span, TokenIdent ident, TokenDelim colon,
											 HirExpr expr)
			: BaseHir(span), ident(ident), colon(colon), expr(std::move(expr)) {}

	TokenIdent ident;
	TokenDelim colon;
	HirExpr expr;

	static HirStructFieldAssign parse(BasicParser &t) {
		auto ident = t.consume<TokenIdent>();
		auto colon = t.consume<TokenDelim>(Delim::COLON);
		auto expr = HirExpr::parse(t);

		return HirStructFieldAssign{ident.span().merge(expr.span()), ident, colon,
																std::move(expr)};
	}
};

class HirArrayInstance : public BaseHir {
public:
	HirArrayInstance(Span span, std::vector<HirExpr> values, HirExpr size)
			: BaseHir(span), values(std::move(values)), size(size) {}

	std::vector<HirExpr> values;
	HirExpr size;

	static HirArrayInstance parse(BasicParser &t) {
		auto lbracket = t.tryConsume<TokenDelim>(Delim::LBRACKET);

		if (!lbracket) {
			throw std::runtime_error("expected [");
		}

		auto rbracket = t.peek<TokenDelim>(Delim::RBRACKET);

		std::vector<HirExpr> values;

		while (!rbracket) {
			auto value = HirExpr::parse(t);
			values.push_back(value);

			if (values.size() == 1) {
				if (t.tryConsume<TokenDelim>(Delim::SEMICOLON)) {
					auto size = HirExpr::parse(t);

					rbracket = t.consume<TokenDelim>(Delim::RBRACKET);
					return HirArrayInstance{lbracket->span().merge(rbracket->span()),
																	std::move(values), size};
				}
			}

			rbracket = t.peek<TokenDelim>(Delim::RBRACKET);

			if (rbracket) {
				break;
			} else {
				t.consume<TokenDelim>(Delim::COMMA);
			}
		}

		auto s = HirExpr{Span{}, TokenLit{Span{}, int(values.size())}};

		t.tryConsume<TokenDelim>(Delim::COMMA);
		rbracket = t.consume<TokenDelim>(Delim::RBRACKET);

		std::size_t size = values.size();

		return HirArrayInstance{lbracket->span().merge(rbracket->span()),
														std::move(values),
														HirExpr{Span{}, TokenLit{Span{}, int(size)}}};
	}

	static std::optional<HirArrayInstance> tryParse(BasicParser &t) {
		auto tx = t.tx();

		try {
			auto hir = parse(t);
			tx.commit();
			return hir;
		} catch (std::runtime_error &e) {
			return std::nullopt;
		}
	}
};

class HirStructInstance : public BaseHir {
public:
	HirStructInstance(Span span, HirPath path,
										std::vector<HirStructFieldAssign> fields)
			: BaseHir(span), path(std::move(path)), fields(std::move(fields)) {}

	HirPath path;
	std::vector<HirStructFieldAssign> fields;

	static HirStructInstance parse(BasicParser &t) {
		auto path = HirPath::parse(t);

		if (!t.tryConsume<TokenDelim>(Delim::LBRACE)) {
			throw std::runtime_error("expected {");
		}

		auto rbrace = t.peek<TokenDelim>(Delim::RBRACE);

		std::vector<HirStructFieldAssign> fields;

		while (!rbrace) {
			auto field = HirStructFieldAssign::parse(t);
			fields.push_back(field);

			rbrace = t.peek<TokenDelim>(Delim::RBRACE);

			if (rbrace) {
				break;
			} else {
				t.consume<TokenDelim>(Delim::COMMA);
			}
		}

		t.tryConsume<TokenDelim>(Delim::COMMA);
		rbrace = t.consume<TokenDelim>(Delim::RBRACE);

		return HirStructInstance{path.span().merge(rbrace->span()), path,
														 std::move(fields)};
	}

	static std::optional<HirStructInstance> tryParse(BasicParser &t) {
		auto tx = t.tx();

		try {
			auto hir = parse(t);
			tx.commit();
			return hir;
		} catch (std::runtime_error &e) {
			return std::nullopt;
		}
	}
};

class HirStruct : public BaseHir {
public:
	HirStruct(Span span, TokenKeyword struct_, TokenIdent ident,
						std::vector<HirTypedIdent> fields,
						std::optional<HirGenerics> generics)
			: BaseHir(span), struct_(struct_), ident(ident),
				fields(std::move(fields)), generics(generics) {}

	bool export_ = false;
	TokenKeyword struct_;
	TokenIdent ident;
	std::vector<HirTypedIdent> fields;
	std::optional<HirGenerics> generics;

	static HirStruct parse(BasicParser &t) {
		auto struct_ = t.tryConsume<TokenKeyword>(Keyword::STRUCT);

		if (!struct_) {
			throw std::runtime_error("expected struct");
		}

		auto ident = t.consume<TokenIdent>();

		std::optional<HirGenerics> generics;

		if (t.peek<TokenDelim>(Delim::LANGLE)) {
			generics = HirGenerics::parse(t);
		}

		t.consume<TokenDelim>(Delim::LBRACE);
		auto rbrace = t.peek<TokenDelim>(Delim::RBRACE);

		std::vector<HirTypedIdent> fields;

		while (!rbrace) {
			auto field = HirTypedIdent::parse(t);
			fields.push_back(field);

			rbrace = t.peek<TokenDelim>(Delim::RBRACE);

			if (rbrace) {
				break;
			} else {
				t.consume<TokenDelim>(Delim::COMMA);
			}
		}

		t.tryConsume<TokenDelim>(Delim::COMMA);
		rbrace = t.consume<TokenDelim>(Delim::RBRACE);

		return HirStruct{struct_->span().merge(rbrace->span()), *struct_, ident,
										 fields, generics};
	}
};

class HirWhile : public BaseHir {
public:
	HirWhile(Span span, TokenKeyword while_, HirExpr cond, HirBlock block)
			: BaseHir(span), while_(while_), cond(std::move(cond)),
				block(std::move(block)) {}

	TokenKeyword while_;
	HirExpr cond;
	HirBlock block;

	static HirWhile parse(BasicParser &t) {
		auto while_ = t.tryConsume<TokenKeyword>(Keyword::WHILE);

		if (!while_) {
			throw std::runtime_error("expected while");
		}

		auto cond = HirExpr::parse(t);
		auto block = HirBlock::parse(t);

		return HirWhile{while_->span().merge(block.span()), *while_,
										std::move(cond), block};
	}

	static std::optional<HirWhile> tryParse(BasicParser &t) {
		auto tx = t.tx();

		try {
			auto hir = parse(t);
			tx.commit();
			return hir;
		} catch (std::runtime_error &e) {
			return std::nullopt;
		}
	}
};

class HirElse : public BaseHir {
public:
	HirElse(Span span, TokenKeyword else_,
					std::optional<std::pair<TokenKeyword, HirExpr>> cond, HirBlock block)
			: BaseHir(span), else_(else_), cond(std::move(cond)),
				block(std::move(block)) {}

	TokenKeyword else_;
	std::optional<std::pair<TokenKeyword, HirExpr>> cond;
	HirBlock block;

	static HirElse parse(BasicParser &t) {
		auto else_ = t.consume<TokenKeyword>(Keyword::ELSE);

		std::optional<std::pair<TokenKeyword, HirExpr>> cond;

		if (t.tryConsume<TokenKeyword>(Keyword::IF)) {
			auto if_ = t.consume<TokenKeyword>(Keyword::IF);
			t.consume<TokenDelim>(Delim::LPAREN);
			auto expr = HirExpr::parse(t);
			t.consume<TokenDelim>(Delim::RPAREN);

			cond = std::make_pair(if_, expr);
		}

		auto block = HirBlock::parse(t);

		return HirElse{else_.span().merge(block.span()), else_, std::move(cond),
									 block};
	}
};

class HirIf : public BaseHir {
public:
	HirIf(Span span, TokenKeyword if_, HirExpr cond, HirBlock block,
				std::vector<HirElse> else_)
			: BaseHir(span), if_(if_), cond(std::move(cond)), block(std::move(block)),
				else_(std::move(else_)) {}

	TokenKeyword if_;
	HirExpr cond;
	HirBlock block;
	std::vector<HirElse> else_;

	static HirIf parse(BasicParser &t) {
		auto if_ = t.tryConsume<TokenKeyword>(Keyword::IF);

		if (!if_) {
			throw std::runtime_error("expected if");
		}

		t.consume<TokenDelim>(Delim::LPAREN);
		auto cond = HirExpr::parse(t);
		t.consume<TokenDelim>(Delim::RPAREN);
		auto block = HirBlock::parse(t);

		std::vector<HirElse> else_;

		while (t.peek<TokenKeyword>(Keyword::ELSE)) {
			else_.push_back(HirElse::parse(t));
		}

		return HirIf{if_->span().merge(block.span()), *if_, std::move(cond), block,
								 else_};
	}

	static std::optional<HirIf> tryParse(BasicParser &t) {
		auto tx = t.tx();

		try {
			auto hir = parse(t);
			tx.commit();
			return hir;
		} catch (std::runtime_error &e) {
			return std::nullopt;
		}
	}
};

class HirFor : public BaseHir {
public:
	HirFor(Span span, TokenKeyword for_, std::optional<HirStmt> init,
				 std::optional<HirExpr> cond, std::optional<HirStmt> update,
				 HirBlock block)
			: BaseHir(span), for_(for_), init(std::move(init)), cond(std::move(cond)),
				update(std::move(update)), block(std::move(block)) {}

	TokenKeyword for_;
	std::optional<HirStmt> init;
	std::optional<HirExpr> cond;
	std::optional<HirStmt> update;
	HirBlock block;

	static HirFor parse(BasicParser &t) {
		auto for_ = t.tryConsume<TokenKeyword>(Keyword::FOR);

		if (!for_) {
			throw std::runtime_error("expected for");
		}

		std::optional<HirStmt> init;
		std::optional<HirExpr> cond;
		std::optional<HirStmt> update;

		t.consume<TokenDelim>(Delim::LPAREN);

		if (auto stmt = HirStmt::tryParse(t)) {
			init = std::move(*stmt);
		}

		t.consume<TokenDelim>(Delim::SEMICOLON);

		if (auto expr = HirExpr::tryParse(t)) {
			cond = std::move(*expr);
		}

		t.consume<TokenDelim>(Delim::SEMICOLON);

		if (auto stmt = HirStmt::tryParse(t)) {
			update = std::move(*stmt);
		}

		t.consume<TokenDelim>(Delim::RPAREN);

		auto block = HirBlock::parse(t);

		return HirFor{for_->span().merge(block.span()),
									*for_,
									std::move(init),
									std::move(cond),
									std::move(update),
									block};
	}

	static std::optional<HirFor> tryParse(BasicParser &t) {
		auto tx = t.tx();

		try {
			auto hir = parse(t);
			tx.commit();
			return hir;
		} catch (std::runtime_error &e) {
			return std::nullopt;
		}
	}
};

HirStmt HirStmt::parse(BasicParser &t) {
	if (auto ret = HirReturn::tryParse(t)) {
		return HirStmt{ret->span(), std::move(*ret)};
	}

	if (auto br = HirBreak::tryParse(t)) {
		return HirStmt{br->span(), std::move(*br)};
	}

	if (auto cont = HirContinue::tryParse(t)) {
		return HirStmt{cont->span(), std::move(*cont)};
	}

	if (auto for_ = HirFor::tryParse(t)) {
		return HirStmt{for_->span(), std::make_shared<HirFor>(*for_)};
	}

	if (auto if_ = HirIf::tryParse(t)) {
		return HirStmt{if_->span(), std::make_shared<HirIf>(*if_)};
	}

	if (auto while_ = HirWhile::tryParse(t)) {
		return HirStmt{while_->span(), std::make_shared<HirWhile>(*while_)};
	}

	if (auto assign = HirAssign::tryParse(t)) {
		return HirStmt{assign->span(), std::move(*assign)};
	}

	if (auto fnCall = HirFnCall::tryParse(t)) {
		return HirStmt{fnCall->span(), std::move(*fnCall)};
	}

	if (auto reassign = HirReassign::tryParse(t)) {
		return HirStmt{reassign->span(), std::move(*reassign)};
	}

	throw std::runtime_error("todo stmt");
}

class HirImport : public BaseHir {
public:
	HirImport(Span span, TokenKeyword import, HirPath path, TokenDelim semi)
			: BaseHir(span), import(import), path(std::move(path)), semi(semi) {}

	TokenKeyword import;
	HirPath path;
	TokenDelim semi;

	static HirImport parse(BasicParser &t) {
		auto import = t.tryConsume<TokenKeyword>(Keyword::IMPORT);

		if (!import) {
			throw std::runtime_error("expected import");
		}

		auto path = HirPath::parse(t);
		auto semi = t.consume<TokenDelim>(Delim::SEMICOLON);

		return HirImport{import->span().merge(semi.span()), *import, path, semi};
	}
};

class HirExtern : public BaseHir {
public:
	HirExtern(Span span, TokenIdent ident, std::vector<HirTypedIdent> params,
						std::optional<HirType> ret, bool variadic)
			: BaseHir(span), ident(ident), params(std::move(params)), ret(ret),
				variadic(variadic) {}

	TokenIdent ident;
	std::vector<HirTypedIdent> params;
	std::optional<HirType> ret;
	bool variadic = false;

	static HirExtern parse(BasicParser &t) {
		auto extern_ = t.tryConsume<TokenKeyword>(Keyword::EXTERN);

		if (!extern_) {
			throw std::runtime_error("expected extern");
		}

		t.consume<TokenKeyword>(Keyword::FN);
		auto ident = t.consume<TokenIdent>();

		t.consume<TokenDelim>(Delim::LPAREN);
		auto rparen = t.peek<TokenDelim>(Delim::RPAREN);

		std::vector<HirTypedIdent> params;
		bool variadic = false;

		while (!rparen) {
			if (t.tryConsume<TokenOp>(Op::ELLIPSIS)) {
				variadic = true;
				break;
			}

			auto param = HirTypedIdent::parse(t);
			params.push_back(param);

			rparen = t.peek<TokenDelim>(Delim::RPAREN);

			if (rparen) {
				break;
			} else {
				t.consume<TokenDelim>(Delim::COMMA);
			}
		}

		t.tryConsume<TokenDelim>(Delim::COMMA);
		rparen = t.consume<TokenDelim>(Delim::RPAREN);

		std::optional<HirType> ret;

		if (t.tryConsume<TokenDelim>(Delim::COLON)) {
			ret = HirType::parse(t);
		}

		t.consume<TokenDelim>(Delim::SEMICOLON);

		return HirExtern{extern_->span().merge(rparen->span()), ident, params, ret,
										 variadic};
	}
};

class HirConst : public BaseHir {
public:
	HirConst(Span span, TokenKeyword const_, HirTypedIdent ident, TokenOp eq,
					 HirExpr expr)
			: BaseHir(span), const_(const_), ident(std::move(ident)), eq(eq),
				expr(std::move(expr)) {}

	bool export_ = false;
	TokenKeyword const_;
	HirTypedIdent ident;
	TokenOp eq;
	HirExpr expr;

	static HirConst parse(BasicParser &t) {
		auto const_ = t.tryConsume<TokenKeyword>(Keyword::CONST);

		if (!const_) {
			throw std::runtime_error("expected const");
		}

		auto ident = HirTypedIdent::parse(t);

		auto eq = t.consume<TokenOp>(Op::EQ);
		auto expr = HirExpr::parse(t);

		return HirConst{const_->span().merge(expr.span()), *const_, ident, eq,
										std::move(expr)};
	}
};

class HirLoop : public BaseHir {
public:
	HirLoop(Span span, TokenKeyword loop, HirBlock block)
			: BaseHir(span), loop(loop), block(std::move(block)) {}

	TokenKeyword loop;
	HirBlock block;

	static HirLoop parse(BasicParser &t) {
		auto loop = t.tryConsume<TokenKeyword>(Keyword::LOOP);

		if (!loop) {
			throw std::runtime_error("expected loop");
		}

		auto block = HirBlock::parse(t);

		return HirLoop{loop->span().merge(block.span()), *loop, block};
	}
};

HirExpr HirExpr::parse(BasicParser &t) { return parse<1>(t); }

template <int P> HirExpr HirExpr::parse(BasicParser &t) {
	auto expr = HirExpr::parse<P + 1>(t);

	while (auto next = t.peek<TokenOp>()) {
		if (next->precedence() != P) {
			break;
		}

		auto op = t.consume<TokenOp>();
		auto rhs = HirExpr::parse<P + 1>(t);

		expr = HirExpr{expr.span().merge(rhs.span()),
									 std::make_shared<HirBinOp>(expr.span().merge(rhs.span()), op,
																							expr, rhs)};
	}

	return expr;
}

template <> HirExpr HirExpr::parse<12>(BasicParser &t) {
	if (auto lit = t.tryConsume<TokenLit>()) {
		return HirExpr{lit->span(), *lit};
	}

	if (auto fnCall = HirFnCall::tryParse(t)) {
		return HirExpr{fnCall->span(), std::make_shared<HirFnCall>(*fnCall)};
	}

	if (auto array = HirArrayInstance::tryParse(t)) {
		return HirExpr{array->span(), std::make_shared<HirArrayInstance>(*array)};
	}

	if (auto struct_ = HirStructInstance::tryParse(t)) {
		return HirExpr{struct_->span(),
									 std::make_shared<HirStructInstance>(*struct_)};
	}

	if (auto path = HirAssignable::tryParse(t)) {
		return HirExpr{path->span(), std::make_shared<HirAssignable>(*path)};
	}

	if (auto ident = t.tryConsume<TokenIdent>()) {
		return HirExpr{ident->span(), *ident};
	}

	if (auto lparen = t.tryConsume<TokenDelim>(Delim::LPAREN)) {
		auto expr = HirExpr::parse(t);
		auto rparen = t.consume<TokenDelim>(Delim::RPAREN);

		expr.span_ = lparen->span().merge(rparen.span());

		return expr;
	}

	if (auto unOp = t.tryConsume<TokenOp>(Op::SUB, Op::NOT, Op::BIT_AND)) {
		auto expr = HirExpr::parse<12>(t);

		return HirExpr{unOp->span().merge(expr.span()),
									 std::make_shared<HirUnOp>(unOp->span().merge(expr.span()),
																						 *unOp, expr)};
	}

	throw std::runtime_error("unexpected token");
}

using Hir = std::variant<HirExpr, HirAssign, HirReassign, HirReturn, HirFn,
												 HirStruct, HirWhile, HirIf, HirFor, HirImport,
												 HirExtern, HirConst, HirLoop>;

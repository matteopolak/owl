#pragma once

#include <memory>
#include <vector>

#include "basic_parser.h"
#include "token.h"

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

using HirExprItem =
		std::variant<TokenLit, TokenIdent, std::shared_ptr<HirBinOp>,
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
};

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

int getPrecedence(Op op) {
	switch (op) {
	case Op::OR:
		return 1;
	case Op::AND:
		return 2;
	case Op::EQEQ:
	case Op::NEQ:
		return 3;
	case Op::LT:
	case Op::GT:
	case Op::LTE:
	case Op::GTE:
		return 4;
	case Op::ADD:
	case Op::SUB:
		return 5;
	case Op::MUL:
	case Op::DIV:
	case Op::MOD:
		return 6;
	case Op::POW:
		return 7;
	default:
		return 0; // Not a binary operator
	}
}

bool isBinaryOp(Op op) { return getPrecedence(op) > 0; }

bool isUnaryOp(Op op) { return op == Op::NOT || op == Op::SUB; }

bool isRightAssociative(Op op) { return op == Op::POW; }

HirExpr parsePrimaryExpr(BasicParser &t) {
	if (auto lit = t.tryConsume<TokenLit>()) {
		return HirExpr{lit->span(), *lit};
	} else if (auto ident = t.tryConsume<TokenIdent>()) {
		return HirExpr{ident->span(), *ident};
	} else if (token->is<TokenOp>() && token->variant == Op::LPAREN) {
		t.consume();
		auto expr = HirExpr::parse(t);
		t.consume<TokenDelim>(Delim::RPAREN);
		return expr;
	} else {
		throw std::runtime_error("Unexpected token");
	}
}

HirExpr parseUnaryExpr(BasicParser &t) {
	auto op = t.peek();
	if (op && isUnaryOp(op->op)) {
		t.consume();
		auto expr = parseUnaryExpr(t);
		return HirExpr{op->span().merge(expr.span()),
									 std::make_shared<HirUnOp>(op->span().merge(expr.span()),
																						 op->op, std::move(expr))};
	}

	return parsePrimaryExpr(t);
}

HirExpr parseBinaryExpr(BasicParser &t, int precedence) {
	auto lhs = parseUnaryExpr(t);

	while (true) {
		auto op = t.peek();
		if (!op || !isBinaryOp(op->op) || getPrecedence(op->op) < precedence) {
			break;
		}
		t.consume();

		auto rhs = parseBinaryExpr(t, getPrecedence(op->op) +
																			(isRightAssociative(op->op) ? 0 : 1));
				lhs = HirExpr{lhs.span().merge(rhs.span()),
                     std::make_shared<HirBinOp>(lhs.span().merge(rhs.span()),
                     op->op, std::move(lhs), std::move(rhs)};
	}

	return lhs;
}

HirExpr HirExpr::parse(BasicParser &t) { return parseBinaryExpr(t, 1); }

class HirPath : public BaseHir {
public:
	HirPath(Span span, std::vector<TokenIdent> parts)
			: BaseHir(span), parts(std::move(parts)) {}

	std::vector<TokenIdent> parts;

	static HirPath parse(BasicParser &t) {
		std::vector<TokenIdent> parts;

		do {
			parts.push_back(t.consume<TokenIdent>());
		} while (t.tryConsume<TokenDelim>(Delim::COLON_COLON));

		Span span = parts.front().span().merge(parts.back().span());

		return HirPath{span, std::move(parts)};
	}
};

class HirTypedIdent : public BaseHir {
public:
	HirTypedIdent(Span span, TokenIdent ident,
								std::optional<std::pair<TokenDelim, HirPath>> type)
			: BaseHir(span), ident(ident), type(std::move(type)) {}

	TokenIdent ident;
	std::optional<std::pair<TokenDelim, HirPath>> type;

	static HirTypedIdent parse(BasicParser &t) {
		auto ident = t.consume<TokenIdent>();
		Span span = ident.span();

		std::optional<std::pair<TokenDelim, HirPath>> type;

		if (auto colon = t.tryConsume<TokenDelim>(Delim::COLON)) {
			auto ty = HirPath::parse(t);

			type = std::make_pair(*colon, std::move(ty));
			span = span.merge(ty.span());
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
		auto let = t.consume<TokenKeyword>(Keyword::LET);
		auto ident = HirTypedIdent::parse(t);
		auto eq = t.consume<TokenOp>(Op::EQ);
		auto expr = HirExpr::parse(t);

		return HirAssign{let.span().merge(expr.span()), let, ident, eq,
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
	HirReassign(Span span, TokenIdent ident, TokenOp eq, HirExpr expr)
			: BaseHir(span), ident(ident), eq(eq), expr(std::move(expr)) {}

	TokenIdent ident;
	TokenOp eq;
	HirExpr expr;

	static HirReassign parse(BasicParser &t) {
		auto ident = t.consume<TokenIdent>();
		auto eq = t.consume<TokenOp>(Op::EQ);
		auto expr = HirExpr::parse(t);

		return HirReassign{ident.span().merge(expr.span()), ident, eq,
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
		auto ret = t.consume<TokenKeyword>(Keyword::RETURN);
		auto expr = HirExpr::parse(t);

		return HirReturn{ret.span().merge(expr.span()), ret, std::move(expr)};
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

class HirFnCall : public BaseHir {
public:
	HirFnCall(Span span, HirPath path, TokenDelim lparen,
						std::vector<HirExpr> args, TokenDelim rparen)
			: BaseHir(span), path(std::move(path)), lparen(lparen),
				args(std::move(args)), rparen(rparen) {}

	HirPath path;
	TokenDelim lparen;
	std::vector<HirExpr> args;
	TokenDelim rparen;

	static HirFnCall parse(BasicParser &t) {
		auto path = HirPath::parse(t);

		auto lparen = t.consume<TokenDelim>(Delim::LPAREN);
		auto rparen = t.peek<TokenDelim>(Delim::RPAREN);

		std::vector<HirExpr> args;

		while (!rparen) {
			args.push_back(HirExpr::parse(t));

			auto comma = t.tryConsume<TokenDelim>(Delim::COMMA);

			rparen = t.peek<TokenDelim>(Delim::RPAREN);

			if (rparen) {
				break;
			}
		}

		rparen = t.consume<TokenDelim>(Delim::RPAREN);

		return HirFnCall{path.span().merge(rparen->span()), path, lparen,
										 std::move(args), *rparen};
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

using HirStmtItem = std::variant<HirAssign, HirReassign, HirReturn, HirFnCall>;

class HirStmt : public BaseHir {
public:
	HirStmtItem stmt;

	HirStmt(Span span, HirStmtItem stmt) : BaseHir(span), stmt(std::move(stmt)) {}

	static HirStmt parse(BasicParser &t) {
		if (auto assign = HirAssign::tryParse(t)) {
			return HirStmt{assign->span(), std::move(*assign)};
		}

		if (auto reassign = HirReassign::tryParse(t)) {
			return HirStmt{reassign->span(), std::move(*reassign)};
		}

		if (auto ret = HirReturn::tryParse(t)) {
			return HirStmt{ret->span(), std::move(*ret)};
		}

		if (auto fnCall = HirFnCall::tryParse(t)) {
			return HirStmt{fnCall->span(), std::move(*fnCall)};
		}

		throw std::runtime_error("todo stmt");
	}

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
			auto next = t.tokens[t.index];
			auto stmt = HirStmt::parse(t);
			t.consume<TokenDelim>(Delim::SEMICOLON);

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
	HirFn(Span span, TokenKeyword fn, TokenIdent ident, TokenDelim lparen,
				std::vector<HirTypedIdent> params, TokenDelim rparen, HirBlock block)
			: BaseHir(span), fn(fn), ident(ident), lparen(lparen),
				params(std::move(params)), rparen(rparen), block(std::move(block)) {}

	bool export_ = false;
	TokenKeyword fn;
	TokenIdent ident;
	TokenDelim lparen;
	std::vector<HirTypedIdent> params;
	TokenDelim rparen;
	HirBlock block;

	static HirFn parse(BasicParser &t) {
		auto fn = t.consume<TokenKeyword>(Keyword::FN);
		auto ident = t.consume<TokenIdent>();

		auto lparen = t.consume<TokenDelim>(Delim::LPAREN);
		auto rparen = t.peek<TokenDelim>(Delim::RPAREN);

		std::vector<HirTypedIdent> params;

		while (!rparen) {
			auto param = HirTypedIdent::parse(t);
			params.push_back(param);

			auto delim = t.tryConsume<TokenDelim>(Delim::COMMA);
			rparen = t.peek<TokenDelim>(Delim::RPAREN);

			if (rparen) {
				break;
			}
		}

		rparen = t.consume<TokenDelim>(Delim::RPAREN);

		auto block = HirBlock::parse(t);

		return HirFn{fn.span().merge(block.span()),
								 fn,
								 ident,
								 lparen,
								 params,
								 *rparen,
								 block};
	}
};

class HirStruct : public BaseHir {
public:
	HirStruct(Span span, TokenKeyword struct_, TokenIdent ident,
						std::vector<HirTypedIdent> fields)
			: BaseHir(span), struct_(struct_), ident(ident),
				fields(std::move(fields)) {}

	bool export_ = false;
	TokenKeyword struct_;
	TokenIdent ident;
	std::vector<HirTypedIdent> fields;

	static HirStruct parse(BasicParser &t) {
		auto struct_ = t.consume<TokenKeyword>(Keyword::STRUCT);
		auto ident = t.consume<TokenIdent>();

		auto lbrace = t.consume<TokenDelim>(Delim::LBRACE);
		auto rbrace = t.peek<TokenDelim>(Delim::RBRACE);

		std::vector<HirTypedIdent> fields;

		while (!rbrace) {
			auto field = HirTypedIdent::parse(t);
			fields.push_back(field);

			auto comma = t.tryConsume<TokenDelim>(Delim::COMMA);

			rbrace = t.peek<TokenDelim>(Delim::RBRACE);

			if (rbrace) {
				break;
			}
		}

		rbrace = t.consume<TokenDelim>(Delim::RBRACE);

		return HirStruct{struct_.span().merge(rbrace->span()), struct_, ident,
										 fields};
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
		auto while_ = t.consume<TokenKeyword>(Keyword::WHILE);
		auto cond = HirExpr::parse(t);
		auto block = HirBlock::parse(t);

		return HirWhile{while_.span().merge(block.span()), while_, std::move(cond),
										block};
	}
};

class HirIf : public BaseHir {
public:
	HirIf(Span span, TokenKeyword if_, HirExpr cond, HirBlock block,
				std::optional<std::pair<TokenKeyword, HirBlock>> elseBlock)
			: BaseHir(span), if_(if_), cond(std::move(cond)), block(std::move(block)),
				elseBlock(std::move(elseBlock)) {}

	TokenKeyword if_;
	HirExpr cond;
	HirBlock block;
	std::optional<std::pair<TokenKeyword, HirBlock>> elseBlock;

	static HirIf parse(BasicParser &t) {
		auto if_ = t.consume<TokenKeyword>(Keyword::IF);
		auto cond = HirExpr::parse(t);
		auto block = HirBlock::parse(t);

		std::optional<std::pair<TokenKeyword, HirBlock>> elseBlock;

		if (auto else_ = t.tryConsume<TokenKeyword>(Keyword::ELSE)) {
			auto block = HirBlock::parse(t);

			elseBlock = std::make_pair(*else_, block);
		}

		return HirIf{if_.span().merge(block.span()), if_, std::move(cond), block,
								 elseBlock};
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
		auto for_ = t.consume<TokenKeyword>(Keyword::FOR);

		std::optional<HirStmt> init;
		std::optional<HirExpr> cond;
		std::optional<HirStmt> update;

		if (auto stmt = HirStmt::tryParse(t)) {
			init = std::move(*stmt);
		}

		auto delim = t.consume<TokenDelim>(Delim::SEMICOLON);

		if (auto expr = HirExpr::tryParse(t)) {
			cond = std::move(*expr);
		}

		auto delim2 = t.consume<TokenDelim>(Delim::SEMICOLON);

		if (auto stmt = HirStmt::tryParse(t)) {
			update = std::move(*stmt);
		}

		auto block = HirBlock::parse(t);

		return HirFor{for_.span().merge(block.span()),
									for_,
									std::move(init),
									std::move(cond),
									std::move(update),
									block};
	}
};

class HirImport : public BaseHir {
public:
	HirImport(Span span, TokenKeyword import, HirPath path, TokenDelim semi)
			: BaseHir(span), import(import), path(std::move(path)), semi(semi) {}

	TokenKeyword import;
	HirPath path;
	TokenDelim semi;

	static HirImport parse(BasicParser &t) {
		auto import = t.consume<TokenKeyword>(Keyword::IMPORT);
		auto path = HirPath::parse(t);
		auto semi = t.consume<TokenDelim>(Delim::SEMICOLON);

		return HirImport{import.span().merge(semi.span()), import, path, semi};
	}
};

class HirExtern : public BaseHir {
public:
	HirExtern(Span span, TokenKeyword extern_, HirFn fn)
			: BaseHir(span), extern_(extern_), fn(std::move(fn)) {}

	TokenKeyword extern_;
	HirFn fn;

	static HirExtern parse(BasicParser &t) {
		auto extern_ = t.consume<TokenKeyword>(Keyword::EXTERN);
		auto fn = HirFn::parse(t);

		return HirExtern{extern_.span().merge(fn.span()), extern_, fn};
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
		auto const_ = t.consume<TokenKeyword>(Keyword::CONST);
		auto ident = HirTypedIdent::parse(t);

		auto eq = t.consume<TokenOp>(Op::EQ);
		auto expr = HirExpr::parse(t);

		return HirConst{const_.span().merge(expr.span()), const_, ident, eq,
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
		auto loop = t.consume<TokenKeyword>(Keyword::LOOP);
		auto block = HirBlock::parse(t);

		return HirLoop{loop.span().merge(block.span()), loop, block};
	}
};

using Hir = std::variant<HirExpr, HirAssign, HirReassign, HirReturn, HirFn,
												 HirStruct, HirWhile, HirIf, HirFor, HirImport,
												 HirExtern, HirConst, HirLoop>;

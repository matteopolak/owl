#pragma once

#include <memory>
#include <vector>

#include "basic_parser.h"
#include "token.h"

class BaseNode {
public:
	BaseNode(Span span) : span_(span) {}

	Span span() { return span_; }

protected:
	Span span_;
};

class NodeBinOp;
class NodeUnOp;

using NodeExprItem =
		std::variant<TokenLit, TokenIdent, std::unique_ptr<NodeBinOp>,
								 std::unique_ptr<NodeUnOp>>;

class NodeExpr : public BaseNode {
public:
	NodeExprItem expr;

	NodeExpr(Span span, NodeExprItem expr)
			: BaseNode(span), expr(std::move(expr)) {}

	static std::optional<NodeExpr> parse(BasicParser &t) { return std::nullopt; }
};

class NodeBinOp : public BaseNode {
public:
	NodeBinOp(Span span, TokenOp op, NodeExpr lhs, NodeExpr rhs)
			: BaseNode(span), op(op), lhs(std::move(lhs)), rhs(std::move(rhs)) {}

	TokenOp op;
	NodeExpr lhs;
	NodeExpr rhs;
};

class NodeUnOp : public BaseNode {
public:
	NodeUnOp(Span span, TokenOp op, NodeExpr expr)
			: BaseNode(span), op(op), expr(std::move(expr)) {}

	TokenOp op;
	NodeExpr expr;

	static std::optional<NodeUnOp> parse(BasicParser &t) {
		auto op = t.tryPeek<TokenOp>();

		if (!op || (op->op != Op::SUB && op->op != Op::NOT)) {
			return std::nullopt;
		}

		t.consume<TokenOp>();

		auto expr = NodeExpr::parse(t);

		if (!expr) {
			throw std::runtime_error("expected expression");
		}

		return NodeUnOp{op->span().merge(expr->span()), *op, std::move(*expr)};
	}
};

class NodePath : public BaseNode {
public:
	NodePath(Span span, std::vector<TokenIdent> parts)
			: BaseNode(span), parts(std::move(parts)) {}

	std::vector<TokenIdent> parts;

	static std::optional<NodePath> parse(BasicParser &t) {
		std::vector<TokenIdent> parts;

		while (auto ident = t.tryConsume<TokenIdent>()) {
			parts.push_back(*ident);

			auto delim = t.tryPeek<TokenDelim>();

			if (delim && delim->delim == Delim::COLON_COLON) {
				t.consume<TokenDelim>();

				continue;
			} else {
				return NodePath{ident->span().merge(delim->span()), std::move(parts)};
			}
		}

		return std::nullopt;
	}
};

class NodeTypedIdent : public BaseNode {
public:
	NodeTypedIdent(Span span, TokenIdent ident,
								 std::optional<std::pair<TokenDelim, NodePath>> type)
			: BaseNode(span), ident(ident), type(std::move(type)) {}

	TokenIdent ident;
	std::optional<std::pair<TokenDelim, NodePath>> type;

	static std::optional<NodeTypedIdent> parse(BasicParser &t) {
		auto ident = t.tryPeek<TokenIdent>();

		if (!ident) {
			return std::nullopt;
		}

		t.consume<TokenIdent>();

		std::optional<std::pair<TokenDelim, NodePath>> type;

		if (auto colon = t.tryConsume<TokenDelim>()) {
			if (colon->delim == Delim::COLON) {
				auto path = NodePath::parse(t);

				if (!path) {
					throw std::runtime_error("expected type path");
				}

				type = std::make_pair(*colon, std::move(*path));
			} else {
				throw std::runtime_error("expected ':'");
			}
		}

		return NodeTypedIdent{ident->span().merge(type->second.span()), *ident,
													std::move(type)};
	}
};

class NodeAssign : public BaseNode {
public:
	NodeAssign(Span span, TokenKeyword let, NodeTypedIdent ident, TokenOp eq,
						 NodeExpr expr)
			: BaseNode(span), let(let), ident(std::move(ident)), eq(eq),
				expr(std::move(expr)) {}

	TokenKeyword let;
	NodeTypedIdent ident;
	TokenOp eq;
	NodeExpr expr;

	static std::optional<NodeAssign> parse(BasicParser &t) {
		auto let = t.tryPeek<TokenKeyword>();

		if (!let || let->keyword != Keyword::LET) {
			return std::nullopt;
		}

		t.consume<TokenKeyword>();

		auto ident = NodeTypedIdent::parse(t);

		if (!ident) {
			throw std::runtime_error("expected identifier");
		}

		auto eq = t.consume<TokenOp>();
		auto expr = NodeExpr::parse(t);

		if (!expr) {
			throw std::runtime_error("expected expression");
		}

		return NodeAssign{let->span().merge(expr->span()), *let, std::move(*ident),
											eq, std::move(*expr)};
	}
};

class NodeReassign : public BaseNode {
public:
	NodeReassign(Span span, TokenIdent ident, TokenOp eq, NodeExpr expr)
			: BaseNode(span), ident(ident), eq(eq), expr(std::move(expr)) {}

	TokenIdent ident;
	TokenOp eq;
	NodeExpr expr;

	static std::optional<NodeReassign> parse(BasicParser &t) {
		auto ident = t.tryPeek<TokenIdent>();

		if (!ident) {
			return std::nullopt;
		}

		t.consume<TokenIdent>();

		auto eq = t.tryConsume<TokenOp>();

		if (!eq || eq->op != Op::EQ) {
			return std::nullopt;
		}

		auto expr = NodeExpr::parse(t);

		if (!expr) {
			throw std::runtime_error("expected expression");
		}

		return NodeReassign{ident->span().merge(expr->span()), *ident, *eq,
												std::move(*expr)};
	}
};

class NodeReturn : public BaseNode {
public:
	NodeReturn(Span span, TokenKeyword ret, std::optional<NodeExpr> expr)
			: BaseNode(span), ret(ret), expr(std::move(expr)) {}

	TokenKeyword ret;
	std::optional<NodeExpr> expr;

	static std::optional<NodeReturn> parse(BasicParser &t) {
		auto ret = t.tryPeek<TokenKeyword>();

		if (!ret || ret->keyword != Keyword::RETURN) {
			return std::nullopt;
		}

		t.consume<TokenKeyword>();

		auto expr = NodeExpr::parse(t);

		return NodeReturn{ret->span().merge(expr->span()), *ret, std::move(expr)};
	}
};

class NodeFnCall : public BaseNode {
public:
	NodeFnCall(Span span, NodePath path, TokenDelim lparen,
						 std::vector<NodeExpr> args, TokenDelim rparen)
			: BaseNode(span), path(std::move(path)), lparen(lparen),
				args(std::move(args)), rparen(rparen) {}

	NodePath path;
	TokenDelim lparen;
	std::vector<NodeExpr> args;
	TokenDelim rparen;

	static std::optional<NodeFnCall> parse(BasicParser &t) {
		auto path = NodePath::parse(t);

		if (!path) {
			return std::nullopt;
		}

		auto lparen = t.tryConsume<TokenDelim>();

		if (!lparen || lparen->delim != Delim::LPAREN) {
			return std::nullopt;
		}

		std::vector<NodeExpr> args;

		while (auto expr = NodeExpr::parse(t)) {
			args.push_back(std::move(*expr));

			auto delim = t.consume<TokenDelim>();

			if (delim.delim == Delim::COMMA) {
				continue;
			} else if (delim.delim != Delim::RPAREN) {
				return NodeFnCall{path->span().merge(delim.span()), std::move(*path),
													*lparen, std::move(args), delim};
			} else {
				throw std::runtime_error("expected ',' or ')'");
			}
		}

		auto delim = t.consume<TokenDelim>();

		if (delim.delim != Delim::RPAREN) {
			return NodeFnCall{path->span().merge(delim.span()), std::move(*path),
												*lparen, std::move(args), delim};
		}

		throw std::runtime_error("expected expression");
	}
};

using NodeStmtItem =
		std::variant<NodeAssign, NodeReassign, NodeReturn, NodeFnCall>;

class NodeStmt : public BaseNode {
public:
	NodeStmtItem stmt;

	NodeStmt(Span span, NodeStmtItem stmt)
			: BaseNode(span), stmt(std::move(stmt)) {}

	static std::optional<NodeStmt> parse(BasicParser &t) {
		if (auto assign = NodeAssign::parse(t)) {
			return NodeStmt{assign->span(), std::move(*assign)};
		}

		if (auto reassign = NodeReassign::parse(t)) {
			return NodeStmt{reassign->span(), std::move(*reassign)};
		}

		if (auto ret = NodeReturn::parse(t)) {
			return NodeStmt{ret->span(), std::move(*ret)};
		}

		if (auto fnCall = NodeFnCall::parse(t)) {
			return NodeStmt{fnCall->span(), std::move(*fnCall)};
		}

		return std::nullopt;
	}
};

class NodeBlock : public BaseNode {
public:
	TokenDelim lbrace;
	std::vector<NodeStmt> stmts;
	TokenDelim rbrace;

	NodeBlock(Span span, TokenDelim lbrace, std::vector<NodeStmt> stmts,
						TokenDelim rbrace)
			: BaseNode(span), lbrace(lbrace), stmts(std::move(stmts)),
				rbrace(rbrace) {}

	static std::optional<NodeBlock> parse(BasicParser &t) {
		auto lbrace = t.tryPeek<TokenDelim>();

		if (!lbrace || lbrace->delim != Delim::LBRACE) {
			return std::nullopt;
		}

		t.consume<TokenDelim>();

		std::vector<NodeStmt> stmts;

		while (auto stmt = NodeStmt::parse(t)) {
			stmts.push_back(std::move(*stmt));
		}

		auto rbrace = t.tryConsume<TokenDelim>();

		if (!rbrace || rbrace->delim != Delim::RBRACE) {
			throw std::runtime_error("expected '}'");
		}

		return NodeBlock{lbrace->span().merge(rbrace->span()), *lbrace,
										 std::move(stmts), *rbrace};
	}
};

class NodeFn : public BaseNode {
public:
	NodeFn(Span span, TokenKeyword fn, TokenIdent ident, TokenDelim lparen,
				 std::vector<NodeTypedIdent> params, TokenDelim rparen, NodeBlock block)
			: BaseNode(span), fn(fn), ident(ident), lparen(lparen),
				params(std::move(params)), rparen(rparen), block(std::move(block)) {}

	TokenKeyword fn;
	TokenIdent ident;
	TokenDelim lparen;
	std::vector<NodeTypedIdent> params;
	TokenDelim rparen;
	NodeBlock block;

	static std::optional<NodeFn> parse(BasicParser &t) {
		auto fn = t.tryPeek<TokenKeyword>();

		if (!fn || fn->keyword != Keyword::FN) {
			return std::nullopt;
		}

		t.consume<TokenKeyword>();

		auto ident = t.consume<TokenIdent>();
		auto lparen = t.consume<TokenDelim>();

		std::vector<NodeTypedIdent> params;

		while (auto param = NodeTypedIdent::parse(t)) {
			params.push_back(std::move(*param));

			auto delim = t.tryConsume<TokenDelim>();

			if (delim && delim->delim == Delim::COMMA) {
				continue;
			} else if (delim && delim->delim == Delim::RPAREN) {
				break;
			} else {
				throw std::runtime_error("expected ',' or ')'");
			}
		}

		auto rparen = t.consume<TokenDelim>();

		auto block = NodeBlock::parse(t);

		if (!block) {
			throw std::runtime_error("expected block");
		}

		return NodeFn{fn->span().merge(block->span()),
									*fn,
									ident,
									lparen,
									std::move(params),
									rparen,
									std::move(*block)};
	}
};

class NodeStruct : public BaseNode {
public:
	NodeStruct(Span span, TokenKeyword struct_, TokenIdent ident,
						 std::vector<NodeTypedIdent> fields)
			: BaseNode(span), struct_(struct_), ident(ident),
				fields(std::move(fields)) {}

	TokenKeyword struct_;
	TokenIdent ident;
	std::vector<NodeTypedIdent> fields;

	static std::optional<NodeStruct> parse(BasicParser &t) {
		auto struct_ = t.tryPeek<TokenKeyword>();

		if (!struct_ || struct_->keyword != Keyword::STRUCT) {
			return std::nullopt;
		}

		t.consume<TokenKeyword>();

		auto ident = t.consume<TokenIdent>();

		std::vector<NodeTypedIdent> fields;

		auto lbrace = t.tryConsume<TokenDelim>();

		if (!lbrace || lbrace->delim != Delim::LBRACE) {
			throw std::runtime_error("expected '{'");
		}

		while (auto field = NodeTypedIdent::parse(t)) {
			fields.push_back(std::move(*field));

			auto delim = t.consume<TokenDelim>();

			if (delim.delim == Delim::COMMA) {
				continue;
			} else if (delim.delim == Delim::RBRACE) {
				break;
			} else {
				throw std::runtime_error("expected ',' or '}'");
			}
		}

		auto rbrace = t.tryConsume<TokenDelim>();

		if (!rbrace || rbrace->delim != Delim::RBRACE) {
			throw std::runtime_error("expected '}'");
		}

		return NodeStruct{struct_->span().merge(rbrace->span()), *struct_, ident,
											std::move(fields)};
	}
};

class NodeWhile : public BaseNode {
public:
	NodeWhile(Span span, TokenKeyword while_, NodeExpr cond, NodeBlock block)
			: BaseNode(span), while_(while_), cond(std::move(cond)),
				block(std::move(block)) {}

	TokenKeyword while_;
	NodeExpr cond;
	NodeBlock block;

	static std::optional<NodeWhile> parse(BasicParser &t) {
		auto while_ = t.tryPeek<TokenKeyword>();

		if (!while_ || while_->keyword != Keyword::WHILE) {
			return std::nullopt;
		}

		t.consume<TokenKeyword>();

		auto cond = NodeExpr::parse(t);

		if (!cond) {
			throw std::runtime_error("expected expression");
		}

		auto block = NodeBlock::parse(t);

		if (!block) {
			throw std::runtime_error("expected block");
		}

		return NodeWhile{while_->span().merge(block->span()), *while_,
										 std::move(*cond), std::move(*block)};
	}
};

class NodeIf : public BaseNode {
public:
	NodeIf(Span span, TokenKeyword if_, NodeExpr cond, NodeBlock block,
				 std::optional<std::pair<TokenKeyword, NodeBlock>> elseBlock)
			: BaseNode(span), if_(if_), cond(std::move(cond)),
				block(std::move(block)), elseBlock(std::move(elseBlock)) {}

	TokenKeyword if_;
	NodeExpr cond;
	NodeBlock block;
	std::optional<std::pair<TokenKeyword, NodeBlock>> elseBlock;

	static std::optional<NodeIf> parse(BasicParser &t) {
		auto if_ = t.tryPeek<TokenKeyword>();

		if (!if_ || if_->keyword != Keyword::IF) {
			return std::nullopt;
		}

		t.consume<TokenKeyword>();

		auto cond = NodeExpr::parse(t);

		if (!cond) {
			throw std::runtime_error("expected expression");
		}

		auto block = NodeBlock::parse(t);

		if (!block) {
			throw std::runtime_error("expected block");
		}

		std::optional<std::pair<TokenKeyword, NodeBlock>> elseBlock;

		if (auto else_ = t.tryConsume<TokenKeyword>()) {
			if (else_->keyword == Keyword::ELSE) {
				auto block = NodeBlock::parse(t);

				if (!block) {
					throw std::runtime_error("expected block");
				}

				elseBlock = std::make_pair(*else_, std::move(*block));
			} else {
				throw std::runtime_error("expected 'else'");
			}
		}

		return NodeIf{if_->span().merge(block->span()), *if_, std::move(*cond),
									std::move(*block), std::move(elseBlock)};
	}
};

class NodeFor : public BaseNode {
public:
	NodeFor(Span span, TokenKeyword for_, std::optional<NodeStmt> init,
					std::optional<NodeExpr> cond, std::optional<NodeStmt> update,
					NodeBlock block)
			: BaseNode(span), for_(for_), init(std::move(init)),
				cond(std::move(cond)), update(std::move(update)),
				block(std::move(block)) {}

	TokenKeyword for_;
	std::optional<NodeStmt> init;
	std::optional<NodeExpr> cond;
	std::optional<NodeStmt> update;
	NodeBlock block;

	static std::optional<NodeFor> parse(BasicParser &t) {
		auto for_ = t.tryPeek<TokenKeyword>();

		if (!for_ || for_->keyword != Keyword::FOR) {
			return std::nullopt;
		}

		t.consume<TokenKeyword>();

		std::optional<NodeStmt> init;
		std::optional<NodeExpr> cond;
		std::optional<NodeStmt> update;

		if (auto stmt = NodeStmt::parse(t)) {
			init = std::move(*stmt);
		}

		auto delim = t.tryConsume<TokenDelim>();

		if (!delim || delim->delim != Delim::SEMICOLON) {
			throw std::runtime_error("expected ';'");
		}

		if (auto expr = NodeExpr::parse(t)) {
			cond = std::move(*expr);
		}

		if (auto stmt = NodeStmt::parse(t)) {
			update = std::move(*stmt);
		}

		auto block = NodeBlock::parse(t);

		if (!block) {
			throw std::runtime_error("expected block");
		}

		return NodeFor{for_->span().merge(block->span()),
									 *for_,
									 std::move(init),
									 std::move(cond),
									 std::move(update),
									 std::move(*block)};
	}
};

class NodeImport : public BaseNode {
public:
	NodeImport(Span span, TokenKeyword import, NodePath path)
			: BaseNode(span), import(import), path(std::move(path)) {}

	TokenKeyword import;
	NodePath path;

	static std::optional<NodeImport> parse(BasicParser &t) {
		auto import = t.tryPeek<TokenKeyword>();

		if (!import || import->keyword != Keyword::IMPORT) {
			return std::nullopt;
		}

		t.consume<TokenKeyword>();

		auto path = NodePath::parse(t);

		if (!path) {
			throw std::runtime_error("expected path");
		}

		return NodeImport{import->span().merge(path->span()), *import,
											std::move(*path)};
	}
};

class NodeExtern : public BaseNode {
public:
	NodeExtern(Span span, TokenKeyword extern_, NodeFn fn)
			: BaseNode(span), extern_(extern_), fn(std::move(fn)) {}

	TokenKeyword extern_;
	NodeFn fn;

	static std::optional<NodeExtern> parse(BasicParser &t) {
		auto extern_ = t.tryPeek<TokenKeyword>();

		if (!extern_ || extern_->keyword != Keyword::EXTERN) {
			return std::nullopt;
		}

		t.consume<TokenKeyword>();

		auto fn = NodeFn::parse(t);

		if (!fn) {
			throw std::runtime_error("expected function");
		}

		return NodeExtern{extern_->span().merge(fn->span()), *extern_,
											std::move(*fn)};
	}
};

class NodeConst : public BaseNode {
public:
	NodeConst(Span span, TokenKeyword const_, NodeTypedIdent ident, TokenOp eq,
						NodeExpr expr)
			: BaseNode(span), const_(const_), ident(std::move(ident)), eq(eq),
				expr(std::move(expr)) {}

	TokenKeyword const_;
	NodeTypedIdent ident;
	TokenOp eq;
	NodeExpr expr;

	static std::optional<NodeConst> parse(BasicParser &t) {
		auto const_ = t.tryPeek<TokenKeyword>();

		if (!const_ || const_->keyword != Keyword::CONST) {
			return std::nullopt;
		}

		t.consume<TokenKeyword>();

		auto ident = NodeTypedIdent::parse(t);

		if (!ident) {
			throw std::runtime_error("expected identifier");
		}

		auto eq = t.consume<TokenOp>();
		auto expr = NodeExpr::parse(t);

		if (!expr) {
			throw std::runtime_error("expected expression");
		}

		return NodeConst{const_->span().merge(expr->span()), *const_,
										 std::move(*ident), eq, std::move(*expr)};
	}
};

class NodeLoop : public BaseNode {
public:
	NodeLoop(Span span, TokenKeyword loop, NodeBlock block)
			: BaseNode(span), loop(loop), block(std::move(block)) {}

	TokenKeyword loop;
	NodeBlock block;

	static std::optional<NodeLoop> parse(BasicParser &t) {
		auto loop = t.tryPeek<TokenKeyword>();

		if (!loop || loop->keyword != Keyword::LOOP) {
			return std::nullopt;
		}

		t.consume<TokenKeyword>();

		auto block = NodeBlock::parse(t);

		if (!block) {
			throw std::runtime_error("expected block");
		}

		return NodeLoop{loop->span().merge(block->span()), *loop,
										std::move(*block)};
	}
};

using NodeExportable = std::variant<NodeFn, NodeStruct, NodeExtern, NodeConst>;

class NodeExport : public BaseNode {
public:
	NodeExport(Span span, TokenKeyword export_, NodeExportable node)
			: BaseNode(span), export_(export_), node(std::move(node)) {}

	TokenKeyword export_;
	NodeExportable node;
};

using Node =
		std::variant<NodeExpr, NodeAssign, NodeReassign, NodeReturn, NodeFn,
								 NodeStruct, NodeWhile, NodeIf, NodeFor, NodeImport, NodeExport,
								 NodeExtern, NodeConst, NodeLoop>;

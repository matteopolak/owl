#include "token.h"

class BaseNode {};

class NodeExpr : public BaseNode {
public:
private:
	TokenLit lit;
	TokenOp op;
	TokenIdent ident;
};

class NodeAssign : public BaseNode {
public:
private:
	TokenIdent ident;
	TokenOp op;
	NodeExpr expr;
};

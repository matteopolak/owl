#include <vector>

#include "token.h"

class Parser {
public:
	Parser(std::vector<Token> tokens) : tokens(tokens) {}

private:
	std::vector<Token> tokens;
};

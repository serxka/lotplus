#include <stdbool.h>
#include <stdint.h>
#include <string.h>

#include "compiler.h"
#include "containers.h"

// Stores our current token we are matching against
static token_t token;
static token_t last_token;

// See if our current token is what we want,
// `if (match(T_ADD)) /* do thing */`
static bool match(token_type_t type) {
	if (token.type == type) {
		last_token = token;
		token = lex_next();
		return true;
	} else {
		return false;
	}
}

static token_t next(void) {
	last_token = token;
	token = lex_next();
	return last_token;
}

static void unstep(void) {
	token = last_token;
	lex_unstep();
}

// Like `match(token_type_t type)` but instead we error if we don't get want we
// want
static bool expect(token_type_t type) {
	if (match(type))
		return true;
	else
		panic("expected token %s at %ld:%ld, found token %s",
		      token_debug_str[type], lex_column(token.span_s),
		      lex_linenum(token.span_s), token_debug_str[token.type]);
}

// pretty self explanatory i reckon smh, git gud
// no, really, lower value == higher predence
// the left and right values are the associativity, (lower), (higher) is
// left-associative, guess what right is if change these make sure to add to the
// filter below as well
// TODO: fix these, i fucked them up, they are the wrong way
static const struct {
	uint8_t r;
} prefix_binding_pow[] = {
        [A_NEG] = {23},
        [A_PREINC] = {21},
        [A_PREDEC] = {21},
};

static const struct {
	uint8_t l;
	uint8_t r;
} infix_binding_pow[] = {
        [A_SET] = {33, 32}, [A_ADD] = {16, 17}, [A_SUB] = {16, 17},
        [A_MUL] = {18, 19}, [A_DIV] = {18, 19}, [A_MOD] = {18, 19},
};

static const struct {
	uint8_t l;
} postfix_binding_pow[] = {
        [A_POSTINC] = {25}, [A_POSTDEC] = {25}, [A_MEMB] = {27},
        [A_IDX] = {29},     [A_CALL] = {31},
};

static bool is_prefix_op(token_type_t type) {
	if ((type >= T_ADD && type <= T_SUB)
	    || (type == T_INC || type == T_DEC))
		return true;
	else
		return false;
}

static bool is_bin_op(token_type_t type) {
	if ((type >= T_ADD && type <= T_MOD) || (type == T_SET))
		return true;
	else
		return false;
}

static bool is_postfix_op(token_type_t type) {
	if ((type >= T_INC && type <= T_DEC)
	    || (type == T_LBRA || type == T_LPAR || type == T_ARW))
		return true;
	else
		return false;
}

static inline ast_kind_t token_to_ast_kind(token_type_t type) {
	switch (type) {
	case T_ADD:
		return A_ADD;
	case T_SUB:
		return A_SUB;
	case T_MUL:
		return A_MUL;
	case T_DIV:
		return A_DIV;
	case T_MOD:
		return A_MOD;
	case T_SET:
		return A_SET;
	default:
		return A_INV;
	}
}

static inline ast_kind_t pre_ast_kind(token_type_t type) {
	switch (type) {
	case T_INC:
		return A_PREINC;
	case T_DEC:
		return A_PREDEC;
	case T_SUB:
		return A_NEG;
	default:
		return token_to_ast_kind(type);
	}
}

static inline ast_kind_t post_ast_kind(token_type_t type) {
	switch (type) {
	case T_INC:
		return A_POSTINC;
	case T_DEC:
		return A_POSTDEC;
	case T_LBRA:
		return A_IDX;
	case T_LPAR:
		return A_CALL;
	case T_ARW:
		return A_MEMB;
	default:
		return token_to_ast_kind(type);
	}
}

str_t parse_symbol_path(void) {
	str_t path = str_empty();
	// Match the first optional dot to see if it is an absolute path
	if (match(T_DOT))
		str_push(&path, '.');
	expect(T_IDENT);
	str_cat(&path, &last_token.string);
	// While we match identifiers push them
	while (match(T_DOT)) {
		expect(T_IDENT);
		str_push(&path, '.');
		str_cat(&path, &last_token.string);
	}
	return path;
}

uint64_t parse_type(void) {
	/*bool pointer = */ match(T_PTR);
	expect(T_IDENT);

	return (uint64_t)-1;
}

node_t *parse_expression(void);

node_t *parse_arguments(void) {
	node_t *args = ast_empty(A_PARAM);
	node_t *expr = parse_expression();
	ast_push_child(args, expr);
	while (match(T_COMMA)) {
		expr = parse_expression();
		if (expr == NULL)
			panic("expected argument, found none: %ld:%ld",
			      lex_column(token.span_s),
			      lex_linenum(token.span_s));
		ast_push_child(args, expr);
	}
	return args;
}

node_t *expression_base(void) {
	if (token.type == T_IDENT || token.type == T_DOT) {
		str_t ident = parse_symbol_path();
		return ast_leaf(A_IDENT, (union ast_val){.identifier = ident});
	} else if (match(T_INT)) {
		return ast_leaf(A_INTLIT,
		                (union ast_val){.sint = last_token.sint});
	} else if (match(T_STR)) {
		return ast_leaf(A_STRLIT,
		                (union ast_val){.string = last_token.string});
	}
	return NULL;
}

node_t *expression_bp(uint8_t min_bp) {
	node_t *lhs = NULL;
	// Check if prefix
	if (is_prefix_op(token.type)) {
		ast_kind_t op = pre_ast_kind(token.type);
		next();
		node_t *rhs = expression_bp(prefix_binding_pow[op].r);
		lhs = ast_binary(op, lhs, rhs);
	} else if (token.type == T_LPAR) {
		next();
		lhs = expression_bp(0);
		expect(T_RPAR);
	} else { // If not, base expression
		lhs = expression_base();
		if (lhs == NULL)
			return NULL;
	}

	// The handle bin and post ops
	while (true) {
		ast_kind_t op = post_ast_kind(token.type);
		// Postfix operators
		if (is_postfix_op(token.type)) {
			uint8_t l_bp = postfix_binding_pow[op].l;
			if (l_bp < min_bp)
				break;
			next();

			if (op == A_IDX) {
				node_t *rhs = expression_bp(0);
				expect(T_RBRA);
				lhs = ast_binary(A_IDX, lhs, rhs);
			} else if (op == A_CALL) {
				node_t *rhs = parse_arguments();
				expect(T_RPAR);
				lhs = ast_binary(A_CALL, lhs, rhs);
			} else if (op == A_MEMB) {
				expect(T_IDENT);
				node_t *rhs = ast_leaf(
				        A_IDENT,
				        (union ast_val){
				                .identifier =
				                        last_token.string});
				lhs = ast_binary(A_MEMB, lhs, rhs);
			} else {
				lhs = ast_unary(op, lhs);
			}
			continue;
		}

		// Infix operators
		if (is_bin_op(token.type)) {
			uint8_t l_bp = infix_binding_pow[op].l;
			uint8_t r_bp = infix_binding_pow[op].r;
			if (l_bp < min_bp)
				break;
			next();

			node_t *rhs = expression_bp(r_bp);
			if (rhs == NULL)
				panic("unfinshed expression, missing right hand side on line: %ld:%ld",
				      lex_column(token.span_s),
				      lex_linenum(token.span_s));
			lhs = ast_binary(op, lhs, rhs);
			continue;
		}
		break;
	}
	return lhs;
}

node_t *parse_expression(void) {
	return expression_bp(0);
}

node_t *parse_variable(symbols_t *local) {
	if (token.type == T_IDENT) {
		str_t ident = token.string;
		uint64_t typeid = -1;
		node_t *expr = NULL;
		next();
		if (!match(T_COL)) { // Check if is a new variable or something
			             // else
			unstep();
			return NULL;
		}
		if (token.type
		    != T_SET) // If we don't have a '=' there must be a type
			typeid = parse_type();
		if (match(T_SET)) { // If we have a '=' parse an expression
			if (token.type == T_SEMI)
				panic("expected expression at %ld:%ld, found ;",
				      lex_column(token.span_s),
				      lex_linenum(token.span_s));
			expr = parse_expression();
		}
		expect(T_SEMI); // Must be finished with a semicolon

		// Insert it into the symbol table
		symbols_insert(local, sym_kv_new_variable(ident, typeid));
		node_t *ident_node =
		        ast_leaf(A_IDENT, (union ast_val){.identifier = ident});
		return ast_binary(A_VAR, ident_node, expr);
	}
	return NULL;
}

node_t *parse_return(void) {
	if (match(T_RET)) {
		node_t *ret = ast_unary(A_RET, parse_expression());
		expect(T_SEMI);
		return ret;
	}
	return NULL;
}

node_t *parse_compound_statement(symbols_t *local);

node_t *parse_statement(symbols_t *local) {
	node_t *stmt = NULL;
	if ((stmt = parse_return()) != NULL) {
		return stmt;
	} else if ((stmt = parse_variable(local)) != NULL) {
		return stmt;
	} else if ((stmt = parse_expression()) != NULL) {
		expect(T_SEMI);
		return stmt;
	} else if (token.type
	           == T_LBRC) { // It is probably a compound statement
		return parse_compound_statement(local);
	}
	return NULL;
}

node_t *parse_compound_statement(symbols_t *local) {
	expect(T_LBRC);
	node_t *block = ast_empty(A_BLCK);
	node_t *child = NULL;
	while ((child = parse_statement(local)) != NULL) {
		ast_push_child(block, child);
	}
	expect(T_RBRC);
	return block;
}

bool parse_1parameter(symbols_t *local) {
	if (match(T_IDENT)) {
		if (str_cmp(&last_token.string, &str_lit("void")))
			return false;
		str_t ident = last_token.string;
		expect(T_COL);
		uint64_t typeid = parse_type();
		symbols_insert(local, sym_kv_new_parameter(ident, typeid));
	}
	return false;
}

void parse_while_parameter(symbols_t *local) {
	if (parse_1parameter(local))
		return;
	while (match(T_COMMA)) {
		parse_1parameter(local);
	}
}

node_t *parse_function(symbols_t *local) {
	if (match(T_IDENT)) {
		str_t func_name = last_token.string;
		if (!match(T_LPAR))
			return NULL;

		symbols_t scope = symbols_new();
		parse_while_parameter(&scope);
		expect(T_RPAR);
		expect(T_COL);
		uint64_t ret_type = parse_type();
		node_t *block = parse_compound_statement(&scope);
		symbols_insert(local,
		               sym_kv_new_func(func_name, scope, ret_type));
		node_t *func = ast_empty(A_FN);
		ast_push_child(
		        func,
		        ast_leaf(A_IDENT,
		                 (union ast_val){.identifier = func_name}));
		ast_push_child(func, block);
		return func;
	}
	return NULL;
}

node_t *parse_module(symbols_t *local) {
	if (match(T_MODUL)) {
		str_t path = parse_symbol_path();
		expect(T_SEMI);
		// Add module to the symbol table
		symbols_insert(local, sym_kv_new_module(path));
		return (node_t *)-1; // We did parse, but we aren't adding a AST
		                     // node
	} else { // It was not a module we should parse something else
		return NULL;
	}
}

node_t *parse_struct(symbols_t *local) {
	return NULL;
}

node_t *parse_unit(void) {
	token = lex_next();                // Preload our first token
	symbols_t symbols = symbols_new(); // Create a symbol table
	node_t *unit = ast_empty(A_UNIT);  // Create our root AST node
	ast_push_child(
	        unit,
	        ast_leaf(A_SYMBLS,
	                 (union ast_val){
	                         .sym_table = symbols})); // Push our symbol
	                                                  // table into the AST
	while (true) {
		// Try and parse all possible top level items in a translation
		// unit
		node_t *child = NULL;
		if ((child = parse_module(&symbols)) != NULL)
			continue;
		if ((child = parse_function(&symbols)) != NULL)
			goto append;
		if ((child = parse_struct(&symbols)) != NULL)
			goto append;
		// If we can't then there MUST be a EOF, if not something went
		// wrong
		expect(T_EOF);
		return unit;

	append:
		ast_push_child(unit, child);
		continue;
	}
}

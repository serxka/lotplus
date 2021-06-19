#include <stdbool.h>
#include <stdint.h>
#include <string.h>

#include "containers.h"
#include "compiler.h"

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

// Like `match(token_type_t type)` but instead we error if we don't get want we want
static bool expect(token_type_t type) {
	if (match(type))
		return true;
	else
		panic("expected token %s at %ld:%ld, found token %s", token_debug_str[type], token.span_s, lex_linenum(token.span_s), token_debug_str[token.type]);
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
	/*bool pointer = */match(T_PTR);
	expect(T_IDENT);

	return (uint64_t)-1;
}

node_t *expression(symbols_t *local) {
	return NULL;
}

node_t *parse_statement(symbols_t *local) {
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
		symbols_insert(local, sym_kv_new_func(func_name, scope, ret_type));
		node_t *func = ast_empty(A_FN);
		ast_push_child(func, ast_leaf(A_IDENT, (union ast_val){.identifier = func_name}));
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
		return (node_t*)-1; // We did parse, but we aren't adding a AST node
	} else { // It was not a module we should parse something else
		return NULL;
	}
}

node_t *parse_struct(symbols_t *local) {
	return NULL;
}

node_t *parse_unit(void) {
	token = lex_next(); // Preload our first token
	symbols_t symbols = symbols_new(); // Create a symbol table
	node_t *unit = ast_empty(A_UNIT); // Create our root AST node
	ast_push_child(unit, ast_leaf(A_SYMBLS, (union ast_val){.sym_table = symbols})); // Push our symbol table into the AST
	while(true) {
		// Try and parse all possible top level items in a translation unit
		node_t *child = NULL;
		if ((child = parse_module(&symbols)) != NULL)
			continue;
		if ((child = parse_function(&symbols)) != NULL)
			goto append;
		if ((child = parse_struct(&symbols)) != NULL)
			goto append;
		// If we can't then there MUST be a EOF, if not something went wrong
		expect(T_EOF);
		return unit;

		append:
			ast_push_child(unit, child);
			continue;
	}
}


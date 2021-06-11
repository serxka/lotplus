#include <stdbool.h>
#include <stdint.h>
#include <string.h>

#include "containers.h"
#include "compiler.h"

static token_t token;
static table_t symbols;

typedef struct sym_kv_s {
	union {
		struct sym_kv_func {
			struct table_t *params;
			struct table_t *block;
		} func;
		struct sym_kv_var {
			uint64_t type_id;
		} variable;
	};
	enum {
		SYM_KV_FUNC,
		SYM_KV_PARAMETER,
		SYM_KV_VARIABLE,
		SYM_KV_INNER,
	} type;
}sym_kv_t;

static inline bool match(token_t t) {
	if (token == t) {
		token = lex_next();
		return true;
	} else {
		return false;
	}
}

static inline bool expect(token_t t) {
	if (match(t))
		return 1;
	else
		panic("failed to match token, expected %s found %s", token_debug_str[t], token_debug_str[token]);
}

static node_t *compound_statement(void) {
	expect(T_LBRC);

	expect(T_RBRC);
	return NULL;
}

static node_t *type(void) {
	bool pointer = match(T_ASTR);
	expect(T_IDENT);

	return NULL;
}

static node_t *parameter_declaration(void) {
	expect(T_IDENT);
	if (!strcmp(lex_tstr, "void")) {
		node_t *type = ast_leaf(A_TYPE, (void*)strdup(lex_tstr));
		return ast_node(A_PARA, NULL, type);
	} else {
		expect(T_COL);
		node_t *ident = ast_leaf(A_IDENT, (void*)strdup(lex_tstr));
		return ast_node(A_PARA, ident, type());
	}
}

static node_t *parameter_list(void) {
	node_t *left = parameter_declaration();
	if (match(T_COMMA)) {
		return ast_node(A_LIST, left, parameter_list());
	} else {
		return ast_node(A_LIST, left, NULL);
	}
}

static table_t parameters_symbols(node_t *root) {
	table_t params = table_new();
	node_t *next = root;
	do {
		if (next->lh != NULL) {
			
			// table_insert((char*)next->lh->leaf, );
		}
	} while((next = root->rh) != NULL && next->op == A_LIST);
}

static node_t *function_definition(void) {
	if (match(T_IDENT) && match(T_LPAR)) {
		char *func_name = strdup(lex_tstr);
		table_t parameters = parameters_symbols(parameter_list());
		expect(T_RPAR);
		expect(T_COL);
		// node_t *ret = type();
		// node_t *body = compound_statement();
	}

	return NULL;
}

static node_t *translation_unit(void) {
	node_t *tree = NULL;
	for (;;) {
		if ((tree = function_definition()) != NULL) {
			
		}

		if (match(T_EOF))
			return tree;
	}
}

node_t *parse(void) {
	token = lex_next();
	symbols = table_new();
	return translation_unit();
}

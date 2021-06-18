#include <stdbool.h>
#include <stdint.h>
#include <string.h>

#include "containers.h"
#include "compiler.h"

static token_t token;
table_t symbols;

typedef struct sym_kv_s {
	union {
		struct sym_kv_var {
			uint64_t type_id;
		} var;
		struct sym_kv_func {
			table_t block;
			struct sym_kv_var ret;
		} func;
	};
	enum {
		SYM_KV_FUNC,
		SYM_KV_PARAMETER,
		SYM_KV_VARIABLE,
		SYM_KV_INNER,
		SYM_KV_MODULE,
	} type;
}sym_kv_t;


static inline struct sym_kv_var sym_extract_type(node_t *type) {
	return (struct sym_kv_var){0};
}

static sym_kv_t *sym_new_param(node_t *type) {
	sym_kv_t *param = calloc(1, sizeof(sym_kv_t));
	if (param == NULL)
		panic("failed to allocate symbol value");
	
	param->type = SYM_KV_PARAMETER;
	param->var = sym_extract_type(type);
	
	return param;
}

static sym_kv_t *sym_new_func(node_t *ret, table_t block) {
	sym_kv_t *func = calloc(1, sizeof(sym_kv_t));
	if (func == NULL)
		panic("failed to allocate symbol value");
	
	func->type = SYM_KV_FUNC;
	func->func.block = block;
	func->func.ret = sym_extract_type(ret);
	
	return func;
}

static sym_kv_t *sym_new_module(void) {
	sym_kv_t *module = calloc(1, sizeof(sym_kv_t));
	if (module == NULL)
		panic("failed to allocate symbol value");
	module->type = SYM_KV_MODULE;
	
	return module;
}

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

static node_t *conditional_statement(table_t *local) {
	return (node_t*)NULL;
}

static node_t *expression(table_t *local);

static node_t *base_expression(void) {
	if (match(T_IDENT))
		return ast_leaf(A_SYMB, (void*)strdup(lex_tstr));
	else if (match(T_STR))
		return ast_leaf(A_STRLIT, (void*)strdup(lex_tstr));
	else if (match(T_INT))
		return ast_leaf(A_INTLIT, (void*)lex_ival);
	else
		return expression(NULL);
}

static node_t *postfix_expression(void) {
	
}

static node_t *unary_expression(void) {
	node_t *base = NULL;
	if ((base = base_expression()) == NULL) {
		
	}
}

static node_t *expression(table_t *local) {
	
}

static node_t *compound_statement(table_t *local);

static node_t *statement(table_t *local) {
	node_t *s = NULL;
	if ((s = expression(local)) != NULL)
		return s;
	if ((s = compound_statement(local)) != NULL)
		return s;
}

static node_t *compound_statement(table_t *local) {
	expect(T_LBRC);
	
	node_t *tree = NULL, *left = NULL;
	while (true) {
		if ((tree = statement(local)) == NULL)
			break;
					
		if (left == NULL)
			left = tree;
		else
			left = ast_node(A_LIST, left, tree);
	}
	
	expect(T_RBRC);
	return ast_unary(A_BLOCK, left);
}

static node_t *type(void) {
	bool pointer = match(T_ASTR);
	expect(T_IDENT);

	return NULL;
}

static node_t *parameter_declaration(void) {
	expect(T_IDENT);
	if (!strcmp(lex_tstr, "void")) {
		return NULL;
	} else {
		expect(T_COL);
		node_t *ident = ast_leaf(A_IDENT, (void*)strdup(lex_tstr));
		return ast_node(A_PARA, ident, type());
	}
}

static node_t *parameter_list(table_t *local) {
	node_t *left = parameter_declaration();
	if (left == NULL) // we have no parameters
		return NULL;
	
	// Add to our symbol table
	table_insert(local, left->lh->leaf, sym_new_param(left->rh));
	if (match(T_COMMA))
		return ast_node(A_LIST, left, parameter_list(local));
	else
		return ast_node(A_LIST, left, NULL);
}

static node_t *function_definition(table_t *local) {
	if (match(T_IDENT)) {
		char *func_name = strdup(lex_tstr);
		if (!match(T_LPAR)) {
			free(func_name);
			return NULL;
		}
		
		table_t block = table_new();
		
		/*node_t *parameters = */parameter_list(&block);
		
		expect(T_RPAR);
		expect(T_COL);
		
		node_t *ret = type();
		node_t *body = compound_statement(&block);

		sym_kv_t *func = sym_new_func(ret, block);
		table_insert(local, func_name, func);
		
		node_t *func_symb = ast_leaf(A_SYMB, (void*)func_name);
		return ast_node(A_FN, func_symb, body);
	} else {
		return NULL;
	}
}

static str_t symbol_path(void) {
	str_t path = str_empty();
	if (match(T_DOT)) // absolute path
		str_push(&path, '.');
	while (match(T_IDENT)) {
		str_cat(&path, lex_tstr);
		if (!match(T_DOT))
			break;
		str_push(&path, '.');
	}
	return path;
}

static node_t *module_declaration(table_t *symbols) {
	if (match(T_MODUL)) {
		str_t path = symbol_path();
		expect(T_SEMI);
		table_insert(symbols, path.d, sym_new_module());
		return ast_leaf(A_MODULE, path.d);
	} else {
		return NULL;
	}
}

static node_t *translation_unit(void) {
	node_t *tree = NULL, *left = NULL;
	while (true) {
		if ((tree = function_definition(&symbols)) != NULL) {
			goto append;
		} else if ((tree = module_declaration(&symbols)) != NULL) {
			goto append;
		}
		
		expect(T_EOF);
		return ast_unary(A_UNIT, left);
		
		append:
			if (left == NULL)
				left = tree;
			else
				left = ast_node(A_LIST, left, tree);
			continue;
	}
}

node_t *parse(void) {
	token = lex_next();
	symbols = table_new();
	return translation_unit();
}

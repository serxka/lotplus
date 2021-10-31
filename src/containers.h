#ifndef _CONTAINERS_H_
#define _CONTAINERS_H_

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

#include "enums.h"

typedef struct vec_s {
	void **data;
	size_t len;
	size_t cap;
} vec_t;

vec_t vec_new(void);
void *vec_idx(vec_t *v, size_t idx);
void *vec_pop(vec_t *v);
void *vec_top(vec_t *v);
void vec_free(vec_t *v);
void vec_push(vec_t *v, void *item);


typedef struct str_s {
	size_t len; // How many characters in the string
	size_t cap; // How many characters the string pointer can hold
	char *d;    // Pointer to the actual underlying string data
} str_t;

#define NULL_STR ((str_t){0})
#define IS_FAT_STR(s) (s->len > s->cap)

// Create a new heap allocated string from data
str_t str_new(const char *data);
// Create a new string struct with a know len,
str_t str_from(const char *data, size_t len);
// Create a new fat string pointer from address and length
str_t str_fat(const char *data, size_t len);
// Special wrapper for creating from string literals
#define str_lit(s)                                                             \
	((str_t){.len = (sizeof(s) / sizeof(s[0])) - 1,                        \
	         .cap = 0,                                                     \
	         .d = (char *)s})
// Create an empty string, with no size or allocation
str_t str_empty(void);
// Concatenate a NUL terminated string onto a owned string
void str_cat_raw(str_t *s, const char *data);
// Concatenate a str_t onto the first one
void str_cat(str_t *s1, const str_t *s2);
// Compare the contents of two strings
bool str_cmp(const str_t *s1, const str_t *s2);
// Push a character onto the string
void str_push(str_t *s, char c);
// Duplicate the string with a new heap allocation
str_t str_dup(str_t *s);
// Promote a fat string pointer to heap allocated one
str_t str_promote(str_t *s);
// Free a string if it is heap allocated
void str_free(str_t *s);


typedef struct sym_kv_s {
	str_t key;
	union {
		struct {
			uint64_t typeid;
		} var;
		struct {
			struct {
				void *_1;
				size_t _2;
			} block; // make sure to cast
			uint64_t ret;
		} func;
	};
	enum {
		SYM_KV_FUNC,     // A function symbol
		SYM_KV_INNER,    // Function parameter or block owned variable
		SYM_KV_MODULE,   // A module declartion
		SYM_KV_VARIABLE, // A variable declaration
	} type;
} sym_kv_t;

typedef struct symbols_s {
	sym_kv_t *data;
	size_t cap;
} symbols_t;

sym_kv_t sym_kv_new_func(str_t key, symbols_t block, uint64_t typeid);
sym_kv_t sym_kv_new_parameter(str_t key, uint64_t typeid);
sym_kv_t sym_kv_new_variable(str_t key, uint64_t typeid);
sym_kv_t sym_kv_new_module(str_t key);

symbols_t symbols_new(void);
void symbols_insert(symbols_t *s, const sym_kv_t kv);
void symbols_delete(symbols_t *s, const str_t *key);
sym_kv_t *symbols_get(symbols_t *s, const str_t *key);


typedef struct node_children_t {
	struct node_s **nodes;
	size_t len;
	size_t cap;
} node_children_t;

typedef struct node_s {
	node_children_t children;
	union ast_val {
		symbols_t sym_table;
		str_t identifier;
		str_t string;
		int64_t sint;
	} val;
	ast_kind_t kind;
} node_t;

node_t *ast_empty(ast_kind_t kind);
node_t *ast_leaf(ast_kind_t kind, union ast_val val);
node_t *ast_unary(ast_kind_t kind, node_t *c);
node_t *ast_binary(ast_kind_t kind, node_t *lh, node_t *rh);
void ast_push_child(node_t *r, node_t *c);

#endif // _CONTAINERS_H_

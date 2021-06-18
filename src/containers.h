#ifndef _CONTAINERS_H_
#define _CONTAINERS_H_

#include <stddef.h>
#include <stdbool.h>

typedef enum {A_ASD} ast_op_t;

typedef struct vec_s {
	void **data;
	size_t len;
	size_t cap;
}vec_t;

vec_t vec_new(void);
void *vec_idx(vec_t *v, size_t idx);
void *vec_pop(vec_t *v);
void *vec_top(vec_t *v);
void vec_free(vec_t *v);
void vec_push(vec_t *v, void *item);

typedef struct node_s {
	union {
		struct {
			struct node_s *lh;
			struct node_s *rh;
		};
		void *leaf;
	};
	ast_op_t op;
}node_t;

node_t *ast_node(ast_op_t op, node_t *lh, node_t *rh);
node_t *ast_unary(ast_op_t op, node_t *lh);
node_t *ast_leaf(ast_op_t op, void *value);

typedef struct key_value_s {
	char *key;
	void *value;
}key_value_t;

typedef struct table_s {
	key_value_t *data;
	size_t cap;
}table_t;

table_t table_new(void);
void table_insert(table_t *t, const char *key, void *value);
void table_delete(table_t *t, const char *key);
void *table_get(table_t *t, const char *key);
void table_iter_reset(table_t *t);
bool table_next(void);

typedef struct str_s {
	size_t len; // How many characters in the string
	size_t cap; // How many characters the string pointer can hold
	char *d; // Pointer to the actual underlying string data
	bool fat_char; // Is this heap allocated or a pointer without terminating NUL?
}str_t;

// Create a new heap allocated string from data
str_t str_new(const char *data);
// Create a new string struct with a know len, 
str_t str_from(const char *data, size_t len);
// Create a new fat string pointer from address and length
str_t str_fat(const char *data, size_t len);
// Create an empty string, with no size or allocation
str_t str_empty(void);
// Concatenate a NUL terminated string onto a owned string
void str_cat(str_t *s, const char *data);
// Push a character onto the string
void str_push(str_t *s, char c);
// Duplicate the string with a new heap allocation
str_t str_dup(str_t *s);
// Promote a fat string pointer to heap allocated one
str_t str_promote(str_t *s);
// Free a string if it is heap allocated
void str_free(str_t *s);

#endif // _CONTAINERS_H_

#ifndef _CONTAINERS_H_
#define _CONTAINERS_H_

#include <stddef.h>

#include "enums.h"

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

#endif // _CONTAINERS_H_

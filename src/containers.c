#include <stddef.h>
#include <stdlib.h>
#include <string.h>

#include "containers.h"
#include "compiler.h"

static void vec_grow(vec_t *v);

vec_t vec_new(void) {
	return (vec_t){
		.data = NULL,
		.len = 0,
		.cap = 0,
	};
}

void vec_push(vec_t *v, void *item) {
	if (v->len >= v->cap)
		vec_grow(v);
	v->data[v->len++] = item;
}

void *vec_pop(vec_t *v) {
	if (v->len == 0)
		panic("tried to pop from vector when len was 0");
	else
		return v->data[--v->len];
	return NULL;
}

void *vec_top(vec_t *v) {
	if (v->len == 0)
		panic("tried to get top of vector when len was 0");
	else
		return v->data[v->len - 1];
	return NULL;
}

void *vec_idx(vec_t *v, size_t idx) {
	if (idx >= v->len)
		panic("tried access vector at %ld when len was %ld", idx, v->len);
	else
		return v->data[idx];
	return NULL;
}

void vec_free(vec_t *v) {
	free(v->data);
	memset(v, 0, sizeof(vec_t));
}

static void vec_grow(vec_t *v) {
	size_t new_size;
	if (v->data == NULL)
		new_size = 4;
	else
		new_size = v->cap * 2;
	
	void **data = realloc(v->data, new_size * sizeof(void*));
	if (data == NULL)
		panic("failed to realloc vector");
	
	v->data = data;
	v->cap = new_size;
}

node_t *ast_node(ast_op_t op, node_t *lh, node_t *rh) {
	node_t *node = calloc(1, sizeof(node_t));
	if (node == NULL)
		panic("failed to calloc ast node");
	node->op = op;
	node->lh = lh;
	node->rh = rh;
	return node;
}

node_t *ast_unary(ast_op_t op, node_t *lh) {
	if ((op & 0x40) != 0)
		panic("tired to create a node without a node ast operation tag");
	return ast_node(op, lh, NULL);
}

node_t *ast_leaf(ast_op_t op, void *value) {
	if ((op & 0x40) == 0)
		panic("tired to create a leaf without a leaf ast operation tag");
	node_t *node = ast_node(op, NULL, NULL);
	node->leaf = value;
	return node;
}

static size_t table_get_free(table_t *t);

table_t table_new(void) {
	return (table_t){
		.data = NULL,
		.cap = 0,
	};
}

void table_insert(table_t *t, const char *key, void *value) {
	size_t idx = table_get_free(t);
	t->data[idx] = (key_value_t){.key = strdup(key), .value = value};
}

void table_delete(table_t *t, const char *key) {
	for (size_t i = 0; i < t->cap; ++i) {
		if (t->data[i].key == NULL)
			continue;
		if (!strcmp(t->data[i].key, key)) {
			free(t->data[i].key);
			t->data[i] = (key_value_t){.key = NULL, .value = NULL};
			return;
		}
	}
	warning("tried to delete key \"%s\" from table when it didn't exist", key);
}

void *table_get(table_t *t, const char *key) {
	for (size_t i = 0; i < t->cap; ++i) {
		if (t->data[i].key == NULL)
			continue;
		if (!strcmp(t->data[i].key, key))
			return t->data[i].key;
	}
	return NULL;
}

static size_t table_get_free(table_t *t) {
	// Look for a free slot
	for (size_t i = 0; i < t->cap; ++i) {
		if (t->data[i].key == NULL) {
			return i;
		}
	}
	// if not found allocate new space
	size_t new_cap = t->cap + 16;
	key_value_t *new_data = calloc(new_cap, sizeof(key_value_t)); // calloc so we know all empty fields are null
	if (new_data == NULL)
		panic("failed to realloc table");
	memcpy(new_data, t->data, t->cap); // copy over old data to new area
	free(t->data);
	t->data = new_data;
	t->cap = new_cap;
	
	return table_get_free(t);
}

static struct table_iter {
	table_t *t;
	size_t cur;
}t_iter;

void table_iter_reset(table_t *t) {
	t_iter.t = t;
	t_iter.cur = 0;
}

bool table_next(void) {
	if (t_iter.cur >= t_iter.t->cap) {
		return false;
	} else {
		if (t_iter.t->data[t_iter.cur++].key != NULL)
			return true;
		else
			return table_next();
	}
}

#define IS_FAT_STR(s) (s->len > s->cap)

static void str_grow(str_t *s, size_t amt);

str_t str_new(const char *data) {
	size_t len = strlen(data);
	char *str = (char*)calloc(len + 1, sizeof(char));
	if (str == NULL)
		panic("failed to allocate string");
	memcpy(str, data, len + 1);
	return (str_t){.len = len, .cap = len, .d = str};
}

str_t str_from(const char *data, size_t len) {
	char *str = (char*)calloc(len + 1, sizeof(char));
	if (str == NULL)
		panic("failed to allocate string");
	memcpy(str, data, len + 1);
	return (str_t){.len = len, .cap = len, .d = str};
}

str_t str_fat(const char *data, size_t len) {
	return (str_t){.len = len, .cap = 0, .d = (char*)data};
}

str_t str_empty(void) {
	return (str_t){0};
}

void str_cat(str_t *s, const char *data) {
	size_t cat_len = strlen(data);
	if (s->len + cat_len > s->cap)
		str_grow(s, cat_len);
	memcpy(s->d + s->len, data, cat_len);
	s->len += cat_len;
	s->d[s->len] = 0;
}

void str_push(str_t *s, char c) {
	if (s->len >= s->cap)
		str_grow(s, 32);
	s->d[s->len++] = c;
	s->d[s->len] = 0;
}

str_t str_dup(str_t *s) {
	char *str = (char*)calloc(s->len + 1, sizeof(char));
	if (str == NULL)
		panic("failed to allocate string");
	memcpy(str, s->d, s->len);
	return (str_t){.len = s->len, .cap = s->len, .d = str};
}

str_t str_promote(str_t *s) {
	if (!IS_FAT_STR(s))
		panic("tried to promote a string to heap when it already was");
	return str_from(s->d, s->len);
}

void str_free(str_t *s) {
	if (s->len <= s->cap)
		free(s->d);
	memset(s, 0, sizeof(str_t));
}

static void str_grow(str_t *s, size_t amt) {
	if (IS_FAT_STR(s))
		*s = str_promote(s);

	size_t new_cap = s->cap + amt;
	char *str = realloc(s->d, new_cap * sizeof(char) + 1);
	if (str == NULL)
		panic("failed to reallocate string");
	s->cap = new_cap;
	s->d = str;
}

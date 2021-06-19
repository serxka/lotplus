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

node_t *ast_empty(ast_kind_t kind) {
	node_t *node = (node_t*)calloc(1, sizeof(node_t));
	if (node == NULL)
		panic("failed to allocated ast node");
	node->kind = kind;
	return node;
}

node_t *ast_leaf(ast_kind_t kind, union ast_val val) {
	node_t *node = (node_t*)calloc(1, sizeof(node_t));
	if (node == NULL)
		panic("failed to allocated ast node");
	node->kind = kind;
	node->val = val;
	return node;
}

static void ast_node_children_grow(node_children_t *v);

void ast_push_child(node_t *r, node_t *c) {
	if (r->children.len >= r->children.cap)
		ast_node_children_grow(&r->children);
	r->children.nodes[r->children.len++] = c;
}

static void ast_node_children_grow(node_children_t *c) {
	size_t new_size;
	if (c->nodes == NULL)
		new_size = 4;
	else
		new_size = c->cap * 2;

	node_t **data = (node_t**)realloc(c->nodes, new_size * sizeof(node_t*));
	if (data == NULL)
		panic("failed to realloc node_t children");

	c->nodes = data;
	c->cap = new_size;
}


sym_kv_t sym_kv_new_func(str_t key, symbols_t block, uint64_t typeid) {
	return (sym_kv_t){
		.key = key,
		.func = {.block = {._1 = block.data, ._2 = block.cap}, .ret = typeid},
		.type = SYM_KV_FUNC,
	};	
}


sym_kv_t sym_kv_new_parameter(str_t key, uint64_t typeid) {
	return (sym_kv_t){
		.key = key,
		.var = {.typeid = typeid},
		.type = SYM_KV_INNER,
	};
}

sym_kv_t sym_kv_new_module(str_t key) {
	return (sym_kv_t){
		.key = key,
		.type = SYM_KV_MODULE,
	};
}

static void delete_sym_kv(sym_kv_t *kv) {
	str_free(&(kv->key));
}

static size_t symbols_get_free(symbols_t *s);

symbols_t symbols_new(void) {
	return (symbols_t){
		.data = NULL,
		.cap = 0,
	};
}

void symbols_insert(symbols_t *s, const sym_kv_t kv) {
	uint64_t idx = symbols_get_free(s);
	s->data[idx] = kv;
}

void symbols_delete(symbols_t *s, const str_t *key) {
	for (uint64_t i = 0; i < s->cap; ++i) {
		if (s->data[i].key.d == NULL_STR.d)
			continue;
		if (str_cmp(&s->data[i].key, key))
			delete_sym_kv(s->data + i);
		return;
	}
}

sym_kv_t *symbols_get(symbols_t *s, const str_t *key) {
	for (uint64_t i = 0; i < s->cap; ++i)
		if (str_cmp(&s->data[i].key, key))
			return s->data + i;
	return NULL;
}

static size_t symbols_get_free(symbols_t *s) {
	// For a free slot in the table
	for (size_t i = 0; i < s->cap; ++i)
		if (s->data[i].key.d == NULL_STR.d)
			return i;

	// If not found allocate new space
	size_t new_cap = s->cap + 32;
	sym_kv_t *new_data = (sym_kv_t*)calloc(new_cap, sizeof(sym_kv_t)); // calloc so we know all empty fields are null
	if (new_data == NULL)
		panic("failed to realloc symbol table");
	memcpy(new_data, s->data, s->cap); // copy over old data to new area
	free(s->data);
	s->data = new_data;
	s->cap = new_cap;
	
	return symbols_get_free(s);
}


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

void str_cat_raw(str_t *s, const char *data) {
	size_t cat_len = strlen(data);
	if (s->len + cat_len > s->cap)
		str_grow(s, cat_len);
	memcpy(s->d + s->len, data, cat_len);
	s->len += cat_len;
	s->d[s->len] = 0;
}

void str_cat(str_t *s1, const str_t *s2) {
	if (s1->len + s2->len > s1->cap)
		str_grow(s1, s2->len);
	memcpy(s1->d + s1->len, s2->d, s2->len);
	s1->len += s2->len;
	s1->d[s1->len] = 0;
}

bool str_cmp(const str_t *s1, const str_t *s2) {
	if (s1->len != s2->len)
		return false;
	for (size_t i = 0; i < s1->len; ++i)
		if (s1->d[i] != s2->d[i])
			return false;
	return true;
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
	if (!IS_FAT_STR(s))
		free(s->d);
	*s = NULL_STR;
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

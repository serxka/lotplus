#include <stddef.h>
#include <stdlib.h>
#include <string.h>

#include "compiler.h"
#include "containers.h"

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
}

void *vec_top(vec_t *v) {
	if (v->len == 0)
		panic("tried to get top of vector when len was 0");
	else
		return v->data[v->len - 1];
}

void *vec_idx(vec_t *v, size_t idx) {
	if (idx >= v->len)
		panic("tried access vector at %ld when len was %ld", idx, v->len);
	else
		return v->data[idx];
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

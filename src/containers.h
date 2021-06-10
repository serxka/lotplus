#ifndef _CONTAINERS_H_
#define _CONTAINERS_H_

#include <stddef.h>

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

#endif // _CONTAINERS_H_
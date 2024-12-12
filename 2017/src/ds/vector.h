#ifndef VECTOR_H
#define VECTOR_H

#include <stdio.h>
#include <stdlib.h>

typedef struct {
    int *array;
    size_t length;
    size_t capacity;
} Vec;

int vec_get(Vec *v, size_t idx);
int vec_pop(Vec *v);
void vec_dump(Vec *v);
void vec_free(Vec *v);
void vec_init(Vec *v, size_t initial_size);
void vec_push(Vec *v, int a);
void vec_extend(Vec *v, Vec *u);

#endif

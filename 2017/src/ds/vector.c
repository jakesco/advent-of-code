#include "vector.h"

void
vec_init(Vec *v, size_t initial_size)
{
    v->array = malloc(initial_size * sizeof(int));
    if (!v->array) {
        fprintf(stderr, "Failed to allocat vector.");
        exit(EXIT_FAILURE);
    }
    v->length = 0;
    v->capacity = initial_size;
}


void
vec_push(Vec *v, int a)
{
    if (v->length == v->capacity) {
        v->capacity = (v->capacity * 3) / 2 + 8;
        int *tmp = realloc(v->array, v->capacity * sizeof(int));
        if (!tmp) {
            fprintf(stderr, "Failed to reallocate vector.");
            exit(EXIT_FAILURE);
        }
        v->array = tmp;
    }
    v->array[v->length++] = a;
}


int
vec_pop(Vec *v)
{
    return v->array[--v->length];
}


int
vec_get(Vec *v, size_t idx)
{
    return v->array[idx];
}

void
vec_dump(Vec *v)
{
    printf("Length: %zu, Capacity %zu\n", v->length, v->capacity);
    printf("Content: [");
    for (size_t i = 0; i < v->length; i++) {
        printf("%d", v->array[i]);
        if (i < v->length - 1) {
            printf(", ");
        }
    }
    printf("]\n");
}

void
vec_extend(Vec *v, Vec *u)
{
    if (v->capacity < v->length + u->length) {
        v->capacity = v->length + u->length;
        int *tmp = realloc(v->array, v->capacity * sizeof(int));
        if (!tmp) {
            fprintf(stderr, "Failed to reallocate vector.");
            exit(EXIT_FAILURE);
        }
        v->array = tmp;
    }
    for (size_t i = 0; i < u->length; i++) {
        v->array[v->length++] = u->array[i];
    }
}


void
vec_free(Vec *v)
{
    free(v->array);
    v->array = NULL;
    v->length = v->capacity = 0;
}



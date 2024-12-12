#include <assert.h>
#include "../src/ds/vector.h"

void
test_vector (void)
{
    Vec v;
    vec_init(&v, 2);
    vec_push(&v, 1);
    vec_push(&v, 2);
    vec_push(&v, 3);
    assert(vec_pop(&v) == 3);
    assert(vec_pop(&v) == 2);
    vec_free(&v);
}

void
test_extend (void)
{
    Vec v, u;
    vec_init(&v, 2);
    vec_init(&u, 2);

    vec_push(&v, 0);
    vec_push(&v, 1);
    vec_push(&u, 2);
    vec_push(&u, 3);

    vec_extend(&v, &u);

    for (size_t i = 0; i < v.length; i++) {
        assert(v.array[i] == (int)i);
    }

    vec_free(&v); vec_free(&u);
}

int
main (void)
{
    test_vector();
    test_extend();
}


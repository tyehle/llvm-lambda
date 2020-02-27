#include "stdio.h"

#include "gc.c"

int* mk_int(int value) {
    int* location = __tl_allocate(8);
    location[0] = 1;
    location[1] = value;
    return location;
}

int* mk_closure(short arity, void* function, short env_size, int** env) {
    int* location = __tl_allocate(16 + 8*env_size);
    location[0] = 0;
    ((short*)location)[2] = arity;
    ((short*)location)[3] = env_size;
    ((void**)location)[1] = function;
    for(int i = 0; i < env_size; i++) {
        ((void**)location)[2+i] = env[i];
    }
    return location;
}

int main() {
    int* i1 = mk_int(4);
    int* c1 = mk_closure(2, (void*)0xFFFFFFFF, 1, &i1);
    int* env[] = {i1, c1};
    int* c2 = mk_closure(5, (void*)0xEEEEEEEE, 2, env);

    __push_scope(i1);

    printf("i1 = %p\n", i1);
    printf("c1 = %p\n", c1);
    printf("c2 = %p\n", c2);

    __run_gc();

    printf("Done\n");

    return 0;
}

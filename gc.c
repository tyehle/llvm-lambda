#include "stdlib.h"
#include "stdio.h"
#include "stdint.h"

typedef struct cell {
    int32_t* value;
    struct cell* prev;
} cell;

cell* __objects_in_scope = 0;
uint64_t __num_objects = 0;
cell* __all_objects = 0;

void __push(cell** where, void* what) {
    cell* new_cell = malloc(sizeof(cell));
    new_cell->prev = *where;
    new_cell->value = what;
    *where = new_cell;
}

void* __pop(cell** where) {
    if(*where == 0) {
        printf("Cannot pop from empty list!\n");
        exit(-1);
    }
    cell* old_cell = *where;
    *where = old_cell->prev;
    void* value = old_cell->value;
    free(old_cell);
    return value;
}

void __mark_heap_objects(int32_t* start) {
    int32_t mark = 0x80000000;

    // printf("Marking %p\n", start);
    // printf("Tag: 0x%08x\n", *start);

    // allow recursive data structures
    if(*start & mark) {
        // we have already seen this object
        // printf("Already marked\n");
        return;
    }

    // mark this object as visited
    *start |= mark;

    if(*start == (0 | mark)) {
        // this is a closure
        // | 4 byte tag | 2 byte arity | 2 byte size | 8? byte function pointer | 8? bytes env pointers ...
        uint16_t env_size = ((uint16_t*)start)[3];
        // printf("%d environment entries\n", env_size);
        int32_t** pointer_array = (int32_t**)&(start[2]);
        int32_t** env = &pointer_array[1];
        for(uint16_t i = 0; i < env_size; i++) {
            __mark_heap_objects(env[i]);
        }
    } else if (*start == (1 | mark)) {
        // this is a 32 bit int
    } else {
        // this is a user defined object
        printf("User defined objects aren't supported for GC\n");
        exit(-1);
    }

}

void __run_gc() {
    // printf("================================== Running GC ==================================\n");
    cell* current;

    // Mark all visible objects
    current = __objects_in_scope;
    while(current != 0) {
        // printf("Marking object in scope %p\n", current->value);
        __mark_heap_objects(current->value);
        current = current->prev;
    }

    // free everything that wasn't marked
    cell* previous = 0;
    current = __all_objects;
    while(current != 0) {
        int32_t tag = *(current->value);
        if(tag & 0x80000000) {
            // this object has been marked
            // remove the tag
            *(current->value) &= 0x7FFFFFFF;
            // move on to the next item
            previous = current;
            current = current->prev;
        } else {
            // delete the object
            // printf("Freeing %p\n", current->value);
            free(current->value);
            // remove the item from the object list
            __num_objects -= 1;
            if(previous == 0) {
                current = current->prev;
                __pop(&__all_objects);
            } else {
                previous->prev = current->prev;
                free(current);
                current = previous->prev;
            }
        }
    }
}

void* __tl_allocate(size_t bytes) {
    void* new_mem = malloc(bytes);

    // run some gc periodically
    if(__num_objects % 16 == 0) {
        __run_gc();
    }

    // printf("Allocated %lu bytes at %p\n", bytes, new_mem);

    if(new_mem == 0) {
        // we are probably out of memory
        // try to free some :fingers_crossed:
        __run_gc();
        new_mem = malloc(bytes);
        if(new_mem == 0) {
            printf("Out of memory\n");
            exit(-1);
        }
    }
    __num_objects += 1;
    __push(&__all_objects, new_mem);
    return new_mem;
}

void __push_scope(int32_t* loc) {
    __push(&__objects_in_scope, loc);
}

void __pop_scope() {
    __pop(&__objects_in_scope);
}

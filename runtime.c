#include "stdint.h"
#include "stdio.h"
#include "stdlib.h"


typedef struct __layout {
    uint16_t gc_flags;
    uint16_t size;
    uint16_t num_pointers;
    uint16_t placeholder;
} __layout;


typedef struct __heap_object {
    struct __heap_object* object_link;
    struct __heap_object* scope_link;
    __layout layout;
} __heap_object;


#define ALIVE 1
__heap_object* __all_objects = 0;
uint64_t __num_objects = 0;
extern __heap_object* __in_scope;


void __run_gc();


__heap_object* __allocate(size_t num_bytes) {
    unsigned long header_size = sizeof(__heap_object);
    unsigned long size = header_size + num_bytes;

    if(num_bytes % 8 != 0) {
        printf("Only allocate a whole number of words\n");
        exit(-1);
    }

    __heap_object* obj = malloc(size);

    // periodically run the GC
    if(__num_objects % 16 == 15) {
        __run_gc();
    }

    #if defined(DEBUG)
        printf("Allocated %lu bytes at %p\n", size, obj);
    #endif

    if(obj == 0) {
        // we are probably out of memory
        // try to free some :fingers_crossed:
        __run_gc();
        obj = malloc(size);
        if(obj == 0) {
            printf("Out of memory\n");
            exit(-1);
        }
    }

    __num_objects += 1;
    obj->object_link = __all_objects;
    __all_objects = obj;

    obj->layout.gc_flags = 0;
    obj->layout.num_pointers = 0;
    obj->layout.size = num_bytes / 8;

    return obj;
}


void __mark_heap_objects(__heap_object* obj) {
    #if defined(DEBUG)
        printf("Marking %p\n", obj);
        printf(
            "  metadata: %04x %04x %04x %04x\n",
            obj->layout.gc_flags,
            obj->layout.arity,
            obj->layout.size,
            obj->layout.num_pointers
        );
    #endif

    // allow recursive data structures
    if(obj->layout.gc_flags & ALIVE) {
        // we have already seen this object
        #if defined(DEBUG)
            printf("Already marked\n");
        #endif
        return;
    }

    // mark this object as visited
    obj->layout.gc_flags |= ALIVE;

    #if defined(DEBUG)
        printf("%d environment pointers\n", obj->layout.num_pointers);
    #endif
    __heap_object** pointer_array = (__heap_object**)(&obj[1]);
    for(uint16_t i = 0; i < obj->layout.num_pointers; i++) {
        __mark_heap_objects(pointer_array[i]);
    }
}


void __run_gc() {
    #if defined(DEBUG)
        printf("============================= Running GC =============================\n");
    #endif
    __heap_object* current;

    // Mark all visible objects
    current = __in_scope;
    while(current != 0) {
        #if defined(DEBUG)
            printf("Marking object in scope %p\n", current);
        #endif
        __mark_heap_objects(current);
        current = current->scope_link;
    }

    // free everything that wasn't marked
    __heap_object* previous = 0;
    current = __all_objects;
    while(current != 0) {
        if(current->layout.gc_flags & ALIVE) {
            // this object has been marked
            // remove the tag
            current->layout.gc_flags &= ~ALIVE;
            // move on to the next item
            previous = current;
            current = current->object_link;
        } else {
            // delete the object
            #if defined(DEBUG)
                printf("Freeing %p\n", current);
            #endif
            // remove the item from the object list
            __num_objects -= 1;
            __heap_object* next = current->object_link;
            if(previous == 0) {
                __all_objects = next;
            } else {
                previous->object_link = next;
            }
            free(current);
            current = next;
        }
    }

    #if defined(DEBUG)
        printf("---------------------------- GC Complete -----------------------------\n");
    #endif
}


void __print_object(char* ptr) {
    __heap_object* obj = (__heap_object*)ptr;

    uint16_t size = obj->layout.size;

    printf(
        "obj@%p<%p,%p,%04x|%04x|%04x>[",
        obj,
        obj->object_link,
        obj->scope_link,
        obj->layout.gc_flags,
        size,
        obj->layout.num_pointers
    );

    void** values = (void**)(&obj[1]);

    for(uint16_t i = 0; i < size - 1; i++) {
        printf("%p,", values[i]);
    }
    printf("%p]\n", values[size - 1]);
}


// int main() {
//     __heap_object* a = __create_closure((void*)0xdeadbeef, 3, 1, 1);
//     __heap_object* b = __create_closure((void*)0x12345678, 4, 1, 1);
//     __heap_object* c = __create_closure((void*)0x12345678, 5, 0, 0);
//     __heap_object* d = __create_closure((void*)0x12345678, 6, 1, 1);
//     __heap_object* e = __create_closure((void*)0x12345678, 7, 0, 0);

//     __set_object_slot(a, 0, b);
//     __set_object_slot(b, 0, c);
//     __set_object_slot(d, 0, e);

//     __push_scope(a);

//     __run_gc();

//     __check_arity(a, 2);

//     return 0;
// }

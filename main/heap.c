#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "vm.h"

static inline size_t align(size_t n) {
	return (n + sizeof(intptr_t) - 1) & ~(sizeof(intptr_t) - 1);
}

static void thread(header **p) {
	if (!*p) {
		return;
	}
	header *temp = *p;
	*p = (header *)temp->thread;
	temp->thread = p;
}

static void scan(header *p) {
	// We support two types: arrays and arrays with pointers to arrays
	// if (p->type == TYPE_ARRAY_ARRAY) {
	// 	header **arrays = p->data;
	// 	for (int i = 0; i < p->length; i++) {
	// 		thread(arrays++);
	// 	}
	// }
}

static void update(header *old, header *new) {
	header **p = old->thread;
	while (p != NULL) {
		header *q = *p;
		*p = new;
		p = (header **)q;
	}
	old->thread = p;
}

static void move(header *old, header *new) {
	size_t size = old->size;
	memmove(new, old, size);
}

static void gc_mark_obj(header *obj) {
	if (obj == NULL) {
		return;
	}
	obj->marked = 1;
	// if (obj->type == TYPE_ARRAY_ARRAY) {
	// 	header **arrays = obj->data;
	// 	for (int i = 0; i < obj->length; i++) {
	// 		gc_mark_obj(arrays[i]); // It's turtles all the way down!
	// 	}
	// }
}

static void gc_mark_vm(VM *vm) {
	printf("sa osp: %d, glob obj size: %d\n", (int)vm->osp, (int)vm->global_object_size);
	int i;
	for (i = 0; i <= vm->osp; i++) {
		printf("osp mark\n");
		gc_mark_obj(vm->object_stack[i]);
		thread(vm->object_stack + i);
	}
	printf("dasdasd\n");
	for (i = 0; i < vm->global_object_size; i++) {
		printf("global mark %d \n", i);
		gc_mark_obj(vm->global_object[i]);
		thread(vm->global_object + i);
	}
}

static inline header *calcNew(header *new, header *curr) {
	return (header *)(((char *)new) + curr->size);
}

static void compact(VM *vm) {
	header *first = NULL;
	header *prev = NULL;
	header *curr = NULL;
	header *new = (header *)vm->heap;
	for (curr = vm->first; curr != NULL; curr = curr->next) {
		if (!curr->marked) {
			continue;
		}
		update(curr, new);
		scan(curr);

		if (first == NULL) {
			first = curr;
		}
		if (prev != NULL) {
			prev->next = curr;
		}

		new = calcNew(new, curr);
		prev = curr;
	}

	if (prev == NULL) {
		vm->first = NULL;
		vm->last = NULL;
		vm->free = vm->heap;
		return;
	}

	prev->next = NULL;

	prev = NULL;
	new = (header *)vm->heap;
	for (curr = first; curr != NULL; curr = curr->next) {
		update(curr, new);
		move(curr, new);

		if (prev != NULL) {
			prev->next = new;
		}
		new->marked = 0;
		prev = new;
		new = calcNew(new, new);
	}

	vm->first = (header *)vm->heap;
	vm->last = prev;
	vm->free = (char *)calcNew(prev, prev);
}

void vm_gc(VM *vm) {
	if (vm->first == NULL) {
		// No first...
		return;
	}
	gc_mark_vm(vm);
	compact(vm);
}

header *vm_malloc(VM *vm, size_t n) {
	size_t header_size = align(sizeof(header));
	size_t size = align(n) + header_size;
	char *next = vm->free;

	// Check for space
	if (next + size > ((char *)vm->heap + vm->heap_size)) {
		vm_gc(vm);
		next = vm->free;
		if (next + size > ((char *)vm->heap + vm->heap_size)) {
			return NULL;
		}
	}

	header *head = (header *)next;
	head->marked = 0;
	head->thread = NULL;
	head->size = size;
	head->data = next + header_size;

	vm->free = next + size;
	if (vm->first == NULL) {
		vm->first = head;
	}
	if (vm->last != NULL) {
		vm->last->next = head;
	}
	vm->last = head;
	return head;
}

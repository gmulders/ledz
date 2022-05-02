#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>

#include "esp_log.h"
#include "vm.h"

#define POP_FLOAT			(*((float*)(vm->stack + sp--)))
#define PUSH_FLOAT(val)		(vm->stack[++sp] = *((int*)(&(val))))
#define POP_INT				(vm->stack[sp--])
#define PUSH_INT(val)		(vm->stack[++sp] = (val))

#define INT_OPERATION(op)	ib = POP_INT; \
							ia = POP_INT; \
							PUSH_INT(ia op ib)

#define FLOAT_OPERATION(op)	fb = POP_FLOAT; \
							fa = POP_FLOAT; \
							fc = fa op fb; \
							PUSH_FLOAT(fc)

#ifdef DEBUG
	#define TRACE_BEGIN     vm_print_instr(vm->code, ip-1)
	#define TRACE_END    	vm_print_stack(vm->stack, sp)
#else
	#define TRACE_BEGIN
	#define TRACE_END
#endif
#define RAW_DISPATCH	goto *dispatch[vm->code[ip++]]
#define DISPATCH    	TRACE_END; RAW_DISPATCH

#define TRACE_BEGIN_DEBUG   vm_print_instr(vm->code, ip-1)
#define TRACE_END_DEBUG    	vm_print_stack(vm->stack, sp)
#define DISPATCH_DEBUG      TRACE_END_DEBUG; RAW_DISPATCH

#define LABEL(x)        Label##x:

static const char *TAG = "vm";

void vm_exec(VM *vm, int startip, int startsp) {

	// registers
	int ip = startip;	// instruction pointer register
	int sp = startsp;	// stack pointer register
	int osp = -1;		// object stack pointer
	int fp = -1;		// call stack pointer register
	int ofp = -1;		// call pointer in object stack register

	int ia = 0;
	int ib = 0;
	int ic = 0;
	float fa = 0;
	float fb = 0;
	float fc = 0;
	header *oa = NULL;
	int addr = 0;
	int offset = 0;

#undef OPCODES_H
#undef OPBEGIN
#undef OPEND
#undef OP
#define OPBEGIN static void *dispatch[] = {
#define OPEND };
#define OP(x)	&&Label##x
#include "opcodes.h"

	RAW_DISPATCH;                  // jump to first instruction interp code

LABEL(NoOp)
	TRACE_BEGIN;
	DISPATCH;

/* integer operations */
LABEL(IntConst)
	TRACE_BEGIN;
	vm->stack[++sp] = vm->code[ip++];  // push operand
	DISPATCH;
LABEL(IntAdd)
	TRACE_BEGIN;
	INT_OPERATION(+);
	DISPATCH;
LABEL(IntSub)
	TRACE_BEGIN;
	INT_OPERATION(-);
	DISPATCH;
LABEL(IntMul)
	TRACE_BEGIN;
	INT_OPERATION(*);
	DISPATCH;
LABEL(IntDiv)
	TRACE_BEGIN;
	INT_OPERATION(/);
	DISPATCH;
LABEL(IntMod)
	TRACE_BEGIN;
	INT_OPERATION(%);
	DISPATCH;
LABEL(IntNeg)
	TRACE_BEGIN;
	ia = POP_INT;
	PUSH_INT(-ia);
	DISPATCH;
LABEL(IntLt)
	TRACE_BEGIN;
	INT_OPERATION(<);
	DISPATCH;
LABEL(IntLte)
	TRACE_BEGIN;
	INT_OPERATION(<=);
	DISPATCH;
LABEL(IntEq)
	TRACE_BEGIN;
	INT_OPERATION(==);
	DISPATCH;
LABEL(IntGte)
	TRACE_BEGIN;
	INT_OPERATION(>=);
	DISPATCH;
LABEL(IntGt)
	TRACE_BEGIN;
	INT_OPERATION(>);
	DISPATCH;

/* float operations */
LABEL(FloatConst)
	TRACE_BEGIN;
	vm->stack[++sp] = vm->code[ip++];	// push operand
	DISPATCH;
LABEL(FloatAdd)
	TRACE_BEGIN;
	FLOAT_OPERATION(+);
	DISPATCH;
LABEL(FloatSub)
	TRACE_BEGIN;
	FLOAT_OPERATION(-);
	DISPATCH;
LABEL(FloatMul)
	TRACE_BEGIN;
	FLOAT_OPERATION(*);
	DISPATCH;
LABEL(FloatDiv)
	TRACE_BEGIN;
	FLOAT_OPERATION(/);
	DISPATCH;
LABEL(FloatNeg)
	TRACE_BEGIN;
	fa = -POP_FLOAT;
	PUSH_FLOAT(fa);
	DISPATCH;
LABEL(FloatLt)
	TRACE_BEGIN;
	FLOAT_OPERATION(<);
	DISPATCH;
LABEL(FloatLte)
	TRACE_BEGIN;
	FLOAT_OPERATION(<=);
	DISPATCH;
LABEL(FloatEq)
	TRACE_BEGIN;
	FLOAT_OPERATION(==);
	DISPATCH;
LABEL(FloatGte)
	TRACE_BEGIN;
	FLOAT_OPERATION(>=);
	DISPATCH;
LABEL(FloatGt)
	TRACE_BEGIN;
	FLOAT_OPERATION(>);
	DISPATCH;

/* integer / float conversions */
LABEL(Int2Float)
	TRACE_BEGIN;
	ia = POP_INT;
	fa = (float)ia;
	PUSH_FLOAT(fa);
	DISPATCH;
LABEL(Float2Int)
	TRACE_BEGIN;
	fa = POP_FLOAT;
	PUSH_INT(fa);
	DISPATCH;

/* boolean operators */
LABEL(BoolNot)
	TRACE_BEGIN;
	ia = POP_INT;
	ia = !ia;
	PUSH_INT(ia);
	DISPATCH;
LABEL(BoolAnd)
	TRACE_BEGIN;
	INT_OPERATION(&&);
	DISPATCH;
LABEL(BoolOr)
	TRACE_BEGIN;
	INT_OPERATION(||);
	DISPATCH;

/* program flow */
LABEL(BranchAbs)
	TRACE_BEGIN;
	ip = vm->code[ip];
	DISPATCH;
LABEL(Branch)
	TRACE_BEGIN;
	ia = vm->code[ip++];
	ip = ip + ia;
	DISPATCH;
LABEL(BranchTrue)
	TRACE_BEGIN;
	ia = vm->code[ip++];
	addr = ip + ia;
	if (vm->stack[sp--] != 0) ip = addr;
	DISPATCH;
LABEL(BranchFalse)
	TRACE_BEGIN;
	ia = vm->code[ip++];
	addr = ip + ia;
	if (vm->stack[sp--] == 0) ip = addr;
	DISPATCH;
LABEL(Halt)
	TRACE_BEGIN;
	TRACE_END;
	return;

/* memory operations */
LABEL(Load)
	TRACE_BEGIN;
	offset = vm->code[ip++];
    // load either a local or an arg;
    // - locals are fp+1, fp+2, ...,
    // - args are fp-5, fp-6, fp-7, ...
    vm->stack[++sp] = vm->stack[fp+offset];
	DISPATCH;
LABEL(GlobalLoad)
	TRACE_BEGIN;
	addr = vm->code[ip++];
	vm->stack[++sp] = vm->global[addr];
	DISPATCH;
LABEL(Store)
	TRACE_BEGIN;
	offset = vm->code[ip++];
    vm->stack[fp+offset] = vm->stack[sp--];
	DISPATCH;
LABEL(GlobalStore)
	TRACE_BEGIN;
	addr = vm->code[ip++];
	vm->global[addr] = vm->stack[sp--];
	DISPATCH;
LABEL(LoadObject)
	TRACE_BEGIN;
	offset = vm->code[ip++];
    // load either a local or an arg;
    // - locals are fp+0, fp+1, ...,
    // - args are fp-1, fp-2, fp-3, ...
    vm->object_stack[++osp] = vm->object_stack[ofp+offset];
	DISPATCH;
LABEL(GlobalLoadObject)
	TRACE_BEGIN;
	addr = vm->code[ip++];
	vm->object_stack[++osp] = vm->global_object[addr];
	DISPATCH;
LABEL(StoreObject)
	TRACE_BEGIN;
	offset = vm->code[ip++];
    vm->object_stack[ofp+offset] = vm->object_stack[osp--];
	DISPATCH;
LABEL(GlobalStoreObject)
	TRACE_BEGIN;
	addr = vm->code[ip++];
	vm->global_object[addr] = vm->object_stack[osp--];
	DISPATCH;

/* stack operations */
LABEL(Pop)
	TRACE_BEGIN;
	--sp;
	DISPATCH;
LABEL(Dup)
	TRACE_BEGIN;
	ia = POP_INT;
	PUSH_INT(ia);
	PUSH_INT(ia);
	DISPATCH;
LABEL(DupX1)
	TRACE_BEGIN;
	ia = POP_INT;
	ib = POP_INT;
	PUSH_INT(ia);
	PUSH_INT(ib);
	PUSH_INT(ia);
	DISPATCH;
LABEL(Swap)
	TRACE_BEGIN;
	ia = POP_INT;
	ib = POP_INT;
	PUSH_INT(ia);
	PUSH_INT(ib);
	DISPATCH;

/* function operations */
LABEL(Call)
//	printf("Call\n");

	TRACE_BEGIN;
	// expects all args on stack
	addr = vm->code[ip++];			// index of target function
	int nargs = vm->code[ip++];		// how many args got pushed
    int nlocals = vm->code[ip++];	// how many locals to allocate
	int noargs = vm->code[ip++];	// how many object args got pushed
    int nolocals = vm->code[ip++];	// how many object locals to allocate
//	printf("sp1: %d\n", sp);
    vm->stack[++sp] = nargs;		// save num args
    vm->stack[++sp] = noargs;		// save num object args
//	printf("sp2: %d\n", sp);
    vm->stack[++sp] = fp;			// save fp
//	printf("sp3: %d\n", sp);
    vm->stack[++sp] = ofp;			// save ofp
//	printf("sp4: %d\n", sp);
    vm->stack[++sp] = ip;			// push return address
//	printf("sp5: %d\n", sp);
    fp = sp;						// fp points at return addr on stack
    sp += nlocals;					// Reserve space for locals
    osp += nolocals;				// Reserve space for object locals
    ip = addr;						// jump to function
    // code preamble of func must push space for locals
	DISPATCH;
LABEL(Return)
	TRACE_BEGIN;
//	printf("Returning\n");
//	vm_print_stack(vm->stack, sp);
	ia = vm->code[ip++];
	if (ia == 1) {
//		printf("pop return value\n");
    	ib = vm->stack[sp--];		// pop return value
	} else {
//		printf("pop object return value\n");
		oa = vm->object_stack[osp--];
	}
//	printf("fp: %d\n", fp);
    sp = fp;						// jump over locals to fp which points at ret addr
//	printf("sp: %d\n", sp);
    osp = ofp;						// jump over locals to ofp which points at ret addr
//	printf("osp: %d\n", osp);
    ip = vm->stack[sp--];			// pop return address, jump to it
//	printf("ip: %d\n", ip);
    ofp = vm->stack[sp--];			// restore ofp
//	printf("ofp: %d\n", ofp);
    fp = vm->stack[sp--];			// restore fp
//	printf("fp: %d\n", fp);
    noargs = vm->stack[sp--];		// how many object args to throw away?
//	printf("noargs: %d\n", noargs);
    nargs = vm->stack[sp--];		// how many args to throw away?
//	printf("nargs: %d\n", nargs);
    sp -= nargs;					// pop args
//	printf("sp %d\n", sp);
	osp -= noargs;
//	printf("osp %d\n", osp);
	if (ia == 1) {					// leave result on stack
//		printf("push return value %d\n", ib);
		vm->stack[++sp] = ib;		// 
	} else {
//		printf("push object return value %d\n", ib);
		vm->object_stack[++osp] = oa;
	}
//	vm_print_stack(vm->stack, sp);
//	printf("Still returning sp=%d, ip=%d, osp=%d\n", sp, ip, osp);
	DISPATCH;
LABEL(CallIn)
	TRACE_BEGIN;
	int func = vm->code[ip++];
	vm->sp = sp;
	vm->osp = osp;
	vm->fns[func](vm);
	sp = vm->sp;
	osp = vm->osp;
	DISPATCH;

/* output */
LABEL(PrintInt)
	TRACE_BEGIN;
	printf("%d\n", vm->stack[sp--]);
	DISPATCH;
LABEL(PrintFloat)
	TRACE_BEGIN;
	printf("%f\n", POP_FLOAT);
	DISPATCH;

LABEL(NewArray)
	TRACE_BEGIN;
	ia = POP_INT;
	vm->sp = sp;
	vm->osp = osp;
	header *head = vm_malloc(vm, sizeof(int) * ia);
	if (head == NULL) {
		printf("Could not allocate memory. Stopping VM.");
		return;
	}
	head->length = ia;
	vm->object_stack[++osp] = head;
	DISPATCH;

LABEL(ArrayLength)
	TRACE_BEGIN;
	oa = vm->object_stack[osp--];
	PUSH_INT(oa->length);
	DISPATCH;
LABEL(ArrayLoad)
	TRACE_BEGIN;
	ia = POP_INT;
	oa = vm->object_stack[osp--];
	ib = *((int *)oa->data + ia);
	PUSH_INT(ib);
	DISPATCH;
LABEL(ArrayStore)
	TRACE_BEGIN;
	ia = POP_INT;
	ib = POP_INT;
	oa = vm->object_stack[osp--];
	int *x = (int *)oa->data + ia;
	*x = ib;
	DISPATCH;
}

void vm_init(
	VM *vm,
	int *code,
	int code_size,
	int stack_size,
	int object_stack_size,
	int global_size,
	int global_object_size,
	int heap_size,
	internal_function *fns,
	int fns_size
) {
	vm->code = code;
	vm->code_size = code_size;

	vm->stack = calloc(stack_size, sizeof(int));
	vm->stack_size = stack_size;

	vm->object_stack = calloc(object_stack_size, sizeof(header *));
	vm->object_stack_size = object_stack_size;

	vm->global = calloc(global_size, sizeof(int));
	vm->global_size = global_size;

	vm->global_object = calloc(global_object_size, sizeof(header *));
	vm->global_object_size = global_object_size;

	vm->heap = malloc(heap_size);
	vm->heap_size = heap_size;
	vm->first = NULL;
	vm->free = vm->heap;

	vm->fns = fns;
	vm->fns_size = fns_size;
}

void vm_free(VM *vm) {
	free(vm->stack);
	free(vm->global);
	free(vm);
}

VM *vm_create(int *code, int code_size, int stack_size, int global_size, int heap_size, internal_function *fns, int fns_size) {
	VM *vm = calloc(1, sizeof(VM));
	vm_init(vm, code, code_size, stack_size, stack_size, global_size, global_size, heap_size, fns, fns_size);
	return vm;
}

#ifdef DEBUG
void vm_print_instr(int *code, int ip) {
	int opcode = code[ip];
	VM_INSTRUCTION *inst = &vm_instructions[opcode];
	switch (inst->nargs) {
		case 0:
			ESP_LOGI(TAG, "%04d:  %-32s", ip, inst->name);
			break;
		case 1:
			ESP_LOGI(TAG, "%04d:  %-20s%-12d", ip, inst->name, code[ip + 1]);
			break;
		case 2:
			ESP_LOGI(TAG, "%04d:  %-20s%d,%11d", ip, inst->name, code[ip + 1], code[ip + 2]);
			break;
		case 3:
			ESP_LOGI(TAG, "%04d:  %-20s%d,%d,%-8d", ip, inst->name, code[ip + 1], code[ip + 2], code[ip + 3]);
			break;
		case 4:
			ESP_LOGI(TAG, "%04d:  %-20s%d,%d,%d,%-8d", ip, inst->name, code[ip + 1], code[ip + 2], code[ip + 3], code[ip + 4]);
			break;
		case 5:
			ESP_LOGI(TAG, "%04d:  %-20s%d,%d,%d,%d,%-8d", ip, inst->name, code[ip + 1], code[ip + 2], code[ip + 3], code[ip + 4], code[ip + 5]);
			break;
	}
}

void vm_print_stack(int *stack, int count) {
	ESP_LOGI(TAG, "stack=[");
	for (int i = 0; i <= count; i++) {
		ESP_LOGI(TAG, " %d", stack[i]);
	}
	ESP_LOGI(TAG, " ]\n");
}

void vm_print_data(int *globals, int count) {
	ESP_LOGI(TAG, "Data memory:\n");
	for (int i = 0; i < count; i++) {
		ESP_LOGI(TAG, "%04d: %d\n", i, globals[i]);
	}
}
#endif

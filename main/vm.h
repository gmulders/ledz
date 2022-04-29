#ifndef VM_H_
#define VM_H_

#include <stdlib.h>

#ifdef __cplusplus
extern "C" {
#endif

#define DEFAULT_STACK_SIZE      1000
#define DEFAULT_NUM_LOCALS      10

#define TYPE_ARRAY      		10
#define TYPE_ARRAY_ARRAY      	11

#include "opcodes.h"
//#define DEBUG 1


typedef struct VM VM;
typedef int (*internal_function)(VM *vm);

typedef struct header header;

struct header {
	struct header* next;
	size_t size;
	header** thread;
	int type;
	int length;
	char marked;
	void *data;
};

struct VM {
    int *code;
    int code_size;
	size_t cp;

    // Operand stack, grows upwards
    int *stack;
    size_t stack_size;
	size_t sp;

    // Operand stack, grows upwards
    header **object_stack;
    int object_stack_size;
	int osp;

    // global variables
    int *global;
    size_t global_size;

    // global objects
    header **global_object;
    size_t global_object_size;

    // heap space
    char *heap;
    size_t heap_size;
	header *first;
	header *last;
	char *free;

	internal_function *fns;
	size_t fns_size;
};

VM *vm_create(int *code, int code_size, int stack_size, int global_size, int heap_size, internal_function *fns, int fns_size);
void vm_free(VM *vm);
void vm_init(VM *vm, int *code, int code_size, int stack_size, int object_stack_size, int global_size, int global_object_size, int heap_size, internal_function *fns, int fns_size);
void vm_exec(VM *vm, int startip, int startsp);

header *vm_malloc(VM *vm, size_t n);

#ifdef DEBUG
typedef struct {
	char name[32];
	int nargs;
} VM_INSTRUCTION;

#define INST(op, cnt)	{ #op, cnt }
static VM_INSTRUCTION vm_instructions[] = {
	INST(NoOp, 0),
	INST(IntConst, 1),
	INST(IntAdd, 0),
	INST(IntSub, 0),
	INST(IntMul, 0),
	INST(IntDiv, 0),
	INST(IntMod, 0),
	INST(IntNeg, 0),
	INST(IntLt, 0),
	INST(IntLte, 0),
	INST(IntEq, 0),
	INST(IntGte, 0),
	INST(IntGt, 0),
	INST(FloatConst, 1),
	INST(FloatAdd, 0),
	INST(FloatSub, 0),
	INST(FloatMul, 0),
	INST(FloatDiv, 0),
	INST(FloatNeg, 0),
	INST(FloatLt, 0),
	INST(FloatLte, 0),
	INST(FloatEq, 0),
	INST(FloatGte, 0),
	INST(FloatGt, 0),
	INST(Int2Float, 0),
	INST(Float2Int, 0),
	INST(BoolNot, 0),
	INST(BoolAnd, 0),
	INST(BoolOr, 0),
	INST(BranchAbs, 0),
	INST(Branch, 1),
	INST(BranchTrue, 1),
	INST(BranchFalse, 1),
	INST(Halt, 0),
	INST(Load, 1),
	INST(GlobalLoad, 1),
	INST(Store, 1),
	INST(GlobalStore, 1),
	INST(LoadObject, 1),
	INST(GlobalLoadObject, 1),
	INST(StoreObject, 1),
	INST(GlobalStoreObject, 1),
	INST(Pop, 0),
    INST(Dup, 0),
    INST(DupX1, 0),
    INST(Swap, 0),
	INST(Call, 5),
	INST(Return, 1),
	INST(CallIn, 1),
	INST(PrintInt, 0),
	INST(PrintFloat, 0),
	INST(NewArray, 0),
	INST(ArrayLength, 0),
	INST(ArrayLoad, 0),
	INST(ArrayStore, 0)
};

void vm_print_instr(int *code, int ip);
void vm_print_stack(int *stack, int count);
void vm_print_data(int *globals, int count);
#endif // DEBUG

#ifdef __cplusplus
}
#endif

#endif

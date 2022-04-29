#ifndef OPCODES_H
#define OPCODES_H

#ifndef OP
#	define OP(x)	x
#	define OPBEGIN	typedef enum {
#	define OPEND	} VM_CODE;
#endif

OPBEGIN
	OP(NoOp),				// no operation

	/* integer operations */
	OP(IntConst),			// push constant integer
	OP(IntAdd),				// int add
	OP(IntSub),				// int subtract
	OP(IntMul),				// int multiply
	OP(IntDiv),				// int division
	OP(IntMod),				// int modulo
	OP(IntNeg),				// int negate
	OP(IntLt),				// int less than
	OP(IntLte),				// int less than or equals
	OP(IntEq),				// int equal
	OP(IntGte),				// int greater than
	OP(IntGt),				// int greater than or equals

	/* float operations */
	OP(FloatConst),			// push constant float
	OP(FloatAdd),			// float add
	OP(FloatSub),			// float subtract
	OP(FloatMul),			// float multiply
	OP(FloatDiv),			// float divide
	OP(FloatNeg),			// float negate
	OP(FloatLt),			// float less than
	OP(FloatLte),			// float less than or equals
	OP(FloatEq),			// float equal
	OP(FloatGte),			// float greater than or equals
	OP(FloatGt),			// float greater than

	/* integer / float conversions */
	OP(Int2Float),			// int to float
	OP(Float2Int),			// float to int

	/* boolean operators */
	OP(BoolNot),			// Boolean NOT
	OP(BoolAnd),			// Boolean AND
	OP(BoolOr),				// Boolean OR

	/* program flow */
	OP(BranchAbs),			// branch to absolute value
	OP(Branch),				// branch to relative value
	OP(BranchTrue),			// branch to relative value if true
	OP(BranchFalse),		// branch to relative value if false
	OP(Halt),				// halts the VM

	/* memory operations */
	OP(Load),				// load from local context
	OP(GlobalLoad),			// load from global memory
	OP(Store),				// store in local context
	OP(GlobalStore),		// store in global memory
	OP(LoadObject),			// load object from local context
	OP(GlobalLoadObject),	// load object from global memory
	OP(StoreObject),		// store object in local context
	OP(GlobalStoreObject),	// store object in global memory

	/* stack operations */
	OP(Pop),				// throw away top of stack
    OP(Dup),				// duplicate top of stack
    OP(DupX1),				// duplicate top of stack and place it two places down
    OP(Swap),				// swap the two top elements on the stack

	/* function operations */
	OP(Call),				// call function at address with nargs and nlocals
	OP(Return),				// return value from function
	OP(CallIn),				// call internal function identified by id

	/* output */
	OP(PrintInt),			// print stack top
	OP(PrintFloat),			// print stack top

	/* arrays */
	OP(NewArray),			// creates a new array
	OP(ArrayLength),		// returns the length of the array
	OP(ArrayLoad),			// load an element from the array
	OP(ArrayStore)			// store an element in the array
OPEND

#endif // OPCODES_H
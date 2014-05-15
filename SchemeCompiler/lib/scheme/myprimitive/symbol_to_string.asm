/* symbol_to_string.asm
* receive a pointer to T_SYMBOL object and put in  R0 
* a pointer to T_STRING object
*/

SYMBOL_TO_STRING:
	PUSH(FP);
	MOV(FP,SP);
	PUSH(R1);
	MOV(R1, FPARG(2)); //R0 is a pointer 
	PUSH(R1);
	CALL(IS_SOB_SYMBOL);
	DROP(1);
	CMP(R0, IMM(1)); //true
	JUMP_EQ(STS_RETURN);
	PUSH(IMM('S')); //print error message
	CALL(PUTCHAR);
	PUSH(IMM('T'));
	CALL(PUTCHAR);
	PUSH(IMM('S'));
	CALL(PUTCHAR);
	PUSH(IMM('-'));
	CALL(PUTCHAR);
	PUSH(IMM('N'));
	CALL(PUTCHAR);
	PUSH(IMM('O'));
	CALL(PUTCHAR);
	PUSH(IMM('T'));
	CALL(PUTCHAR);
	PUSH(IMM(' '));
	CALL(PUTCHAR);
	PUSH(IMM('S'));
	CALL(PUTCHAR);
	PUSH(IMM('Y'));
	CALL(PUTCHAR);
	PUSH(IMM('M'));
	CALL(PUTCHAR);
	PUSH(IMM('B'));
	CALL(PUTCHAR);
	PUSH(IMM('O'));
	CALL(PUTCHAR);
	PUSH(IMM('L'));
	CALL(PUTCHAR);
	PUSH(IMM('\n'));
	CALL(PUTCHAR);
	DROP(15);
	HALT;
	
STS_RETURN:
	MOV(R0, R1);
	MOV(R0, INDD(R0, 1)); //R0 is a pointer to bucket
	MOV(R0, IND(R0)); // R0 is a pointer to T_STRING
	POP(R1);
	POP(FP);
	RETURN;
	
	
	
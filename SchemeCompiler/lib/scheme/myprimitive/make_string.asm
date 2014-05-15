/* scheme/myprimitive/make_string.asm
 * take number k and char 
 * put in R0 a new T_STRING object  
 */
 
 MAKE_STRING:
	PUSH(FP);
	MOV(FP, SP);
	PUSH(R1);
	PUSH(R2);
	PUSH(R3);
	MOV(R1, FPARG(2));
	/* R1 is pointer to T_INTEGER */
	MOV(R1, INDD(R1, 1));
	/* R1 is a number */
	CMP(FPARG(1), IMM(2));
	JUMP_EQ(GET_CHAR_ARG);
	/* enter default char null (ascii 0) */ 
	MOV(R2, IMM('\0'));
	JUMP(MAKE_STRING_START_LOOP);
GET_CHAR_ARG:
	MOV(R2, FPARG(3));
	/* R1 is pointer to T_CHAR */
	MOV(R2, INDD(R2, 1));
	/* R2 is a char */
 MAKE_STRING_START_LOOP:
	MOV(R3, R1);
	/* R3 is a counter */
 MAKE_STRING_LOOP:
	CMP(R3, IMM(0));
	JUMP_EQ(MAKE_STRING_LOOP_END);
	/* push the char in r2 for r3 times */
	PUSH(IMM(R2));
	DECR(R3);
	JUMP(MAKE_STRING_LOOP);
 MAKE_STRING_LOOP_END:
	PUSH(R1);
	/* number of chars */
	CALL(MAKE_SOB_STRING);
	/* new string in r0 */
	INCR(R1);
	DROP(R1);
	POP(R3);
	POP(R2);
	POP(R1);
	POP(FP);
	RETURN;
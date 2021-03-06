/* scheme/myprimitive/equal_strings.asm
 * Takes to pointers to T_STRING objects and return T_BOOL in R0.
 */

 EQUAL_STRINGS:
	PUSH(FP);
	MOV(FP, SP);
	PUSH(R1);
	PUSH(R2);
	PUSH(R3);
	/* FIRST ARGUMENT: POINTER TO T_STRING */
	MOV(R1, FPARG(0)); 
	/* SECOND ARGUMENT: POINTER TO T_STRING */
	MOV(R2, FPARG(1));
	/* COMPARE LENGTH */
	MOV(R3, INDD(R1, 1));
	CMP(R3, INDD(R2, 1));
	JUMP_NE(RETURN_NOT_EQUAL_STRINGS);
	/* EQUAL LENGTH */
	/* COMPARE CHARS */
	ADD(R1, 2);
	ADD(R2, 2);
	/* R1 AND R2 POINTS TO FIRST CHAR */
 COMPARE_CHARS_LOOP:
	CMP(R3, IMM(0));
	JUMP_EQ(RETURN_EQUAL_STRINGS);
	MOV(R0, IND(R1));
	CMP(R0, IND(R2));
	JUMP_NE(RETURN_NOT_EQUAL_STRINGS);
	INCR(R1);
	INCR(R2);
	DECR(R3);
	/* R1 AND R2 POINTS TO NEXT CHAR */
	JUMP(COMPARE_CHARS_LOOP);
RETURN_NOT_EQUAL_STRINGS:
	MOV(R0, SOB_FALSE);
	JUMP(EQUAL_STRINGS_END);
RETURN_EQUAL_STRINGS:
	MOV(R0, SOB_TRUE);
EQUAL_STRINGS_END:
	POP(R3);
	POP(R2);
	POP(R1);
	POP(FP);
	RETURN;
/* scheme/myprimitive/bin_div.asm
 * take 2 t_integer objects and put the quotiente in r0
 */

BIN_DIV:
	PUSH(FP);
	MOV(FP, SP);
	PUSH(R1);
	PUSH(R2);
	/* put in r1 the first t_int arg */
	MOV(R1, FPARG(2));
	/* put in r2 the second t_int arg */
	MOV(R2, FPARG(3));
	/* put in r1 the first int value */
	MOV(R1, INDD(R1, 1));
	/* put in r2 the second int value */
	MOV(R2, INDD(R2, 1));
	DIV(R1, R2);
	PUSH(R1);
	CALL(MAKE_SOB_INTEGER);
	DROP(1);
	POP(R2);
	POP(R1);
	POP(FP);
	RETURN;
	
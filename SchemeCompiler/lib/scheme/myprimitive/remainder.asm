 /* scheme/myprimitive/remainder.asm
  * take 2 T_INTEGER objects and put in r0 the remainder of the quotient
  */
  
  REMAINDER:
	PUSH(FP);
	MOV(FP, SP);
	PUSH(R1);
	PUSH(R2);
	MOV(R1, FPARG(2)); 
	/* R1 is first argument */
	MOV(R1, INDD(R1, 1));
	/* R1 is the value */
	MOV(R2, FPARG(3)); 
	/* R2 is second argument */
	MOV(R2, INDD(R2, 1));
	/* R2 is the value */
	REM(R1, R2);
	/* R1 is the quotient */
	PUSH(R1);
	CALL(MAKE_SOB_INTEGER);
	DROP(1);
	/* R0 is T_INTEGER object */
	POP(R2);
	POP(R1);
	POP(FP);
	RETURN;
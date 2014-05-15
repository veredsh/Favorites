/* scheme/make_sob_symbol.asm
 * Takes a pointer to bucket as argument, and places in R0 
 * a pointer to the newly allocated symbol object
 * Programmers: Vered & Uriya, 2012
 */
 
 MAKE_SOB_SYMBOL:
	PUSH(FP);
	MOV(FP, SP);
	PUSH(IMM(2));
	CALL(MALLOC);
	DROP(1);
	MOV(IND(R0), T_SYMBOL);
	MOV(INDD(R0, 1), FPARG(0));
	POP(FP);
	RETURN;
/* cons.asm  
 *recive 2 argument and make sob_pair from them
 */
 
 CONS:
 
	PUSH(FP);
	MOV(FP,SP);		/*OPEN NEW FRAME*/
	PUSH(R2);
	PUSH(R3);
	CMP(FPARG(1),IMM(2));
	JUMP_EQ(CONTINUE_CONS)
	SHOW("for cons need to be 2 argument you send (print ->)",FPARG(1));
	HALT;
 CONTINUE_CONS:	
	MOV(R2,FPARG(2)); /* R2<- FIRST ARGUMENT GOING  BE THE CAR*/
	MOV(R3,FPARG(3)); /*R3<- SECOND ARGUMENT GOING BE CDR*/
	PUSH(R3);
	PUSH(R2);
	CALL(MAKE_SOB_PAIR);
	DROP(2);
	POP(R3);
	POP(R2);
	MOV(SP,FP);
	POP(FP);
 RETURN; 
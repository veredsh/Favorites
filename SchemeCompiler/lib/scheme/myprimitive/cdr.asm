/* cdr.asm  
 *recive a pair scheme object and put in R0 the cDr of it if not make error and halt
 */
 
 CDR:
 
	PUSH(FP);
	MOV(FP,SP);		/*OPEN NEW FRAME*/
	
	PUSH(R1); 
	CMP(FPARG(1),IMM(1)); /*<- ARGUMENT NUMBER (NEED TO BE 1)*/
	JUMP_EQ(CONTINUE_CDR)
	SHOW("for cdr need to be 1 argument you send (print ->)",FPARG(1));
	HALT;
 CONTINUE_CDR:	
	MOV(R1,FPARG(2)); /*PUT THE SOB_PAIR IN R1*/
	CMP(IND(R1),T_PAIR);
	JUMP_EQ(ARG_IS_OK_CDR);
	SHOW("The argumen is not list or pair. print the input for cdr",FPARG(2));
	HALT;
 ARG_IS_OK_CDR:
	MOV(R0,INDD(R1,2)); /*PUT IN R0 THE CDR VALUE FROM SOB_PAIR (THAT SEAT IN THE SECOND WORD (INDD(R1,2)))*/
	POP(R1);
	MOV(SP,FP);
	POP(FP);
 RETURN; 
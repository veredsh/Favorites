/* vector_length.asm  
 *recive vector as a argument and need to return the length of it
 */
 
 VECTOR_LENGTH:
 
	PUSH(FP);
	MOV(FP,SP);		/*OPEN NEW FRAME*/
	PUSH(R1);
	MOV(R1,FPARG(1)); /*R1 <- ARGUMENT NUMBER (NEED TO BE 1)*/
	CMP(R1,IMM(1));
	JUMP_EQ(CONTINUE_VECTOR_LENGTH)
	SHOW("for vector_length need to be 1 argument you send (print ->)",FPARG(1));
	HALT;
 CONTINUE_VECTOR_LENGTH:	
	MOV(R1,FPARG(2)); /* R1<- the only argument need to be VECTOR*/
	CMP(IND(R1),T_VECTOR);
	JUMP_EQ(ARG_IS_OK_VECTOR_LENGTH)
	SHOW("bad input you dont give string as argument you send (print ->)",FPARG(2));
	HALT;
 ARG_IS_OK_VECTOR_LENGTH:
	MOV(R0,INDD(R1,1)); /*R0<- THE MIDDLE WORD IN SOB_VECTOR INDICATE ABOUT THE LENGTH OF THE STRING*/
	PUSH(R0);
	CALL(MAKE_SOB_INTEGER);
	DROP(1);
	POP(R1);
	MOV(SP,FP);
	POP(FP);
 RETURN; 
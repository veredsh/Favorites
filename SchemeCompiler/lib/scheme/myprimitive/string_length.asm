/* string_length.asm  
 *recive string as a argument and need to return the length of it
 */
 
 STRING_LENGTH:
 
	PUSH(FP);
	MOV(FP,SP);		/*OPEN NEW FRAME*/
	PUSH(R1);
	MOV(R1,FPARG(1)); /*R1 <- ARGUMENT NUMBER (NEED TO BE 1)*/
	CMP(R1,IMM(1));
	JUMP_EQ(CONTINUE_STRING_LENGTH)
	SHOW("for string_length need to be 1 argument you send (print ->)",FPARG(1));
	HALT;
 CONTINUE_STRING_LENGTH:	
	MOV(R1,FPARG(2)); /* R1<- the only argument need to be string*/
	CMP(IND(R1),T_STRING);
	JUMP_EQ(ARG_IS_STRING_STRING_LENGTH)
	SHOW("bad input you dont give string as argument you send (print ->)",FPARG(2));
	HALT;
 ARG_IS_STRING_STRING_LENGTH:
	MOV(R0,INDD(R1,1)); /*R0<- THE MIDDLE WORD IN SOB_STRING INDICATE ABOUT THE LENGTH OF THE STRING*/
	PUSH(R0);
	CALL(MAKE_SOB_INTEGER);
	DROP(1);
	POP(R1);
	MOV(SP,FP);
	POP(FP);
 RETURN; 
/* char_to_integer.asm
 * converts sob_integer  to sob_char 
*/

CHAR_TO_INTEGER:

	PUSH(FP);
	MOV(FP, SP);
	PUSH(R1);
	CMP(FPARG(1),IMM(1)); /*<- ARGUMENT NUMBER (NEED TO BE 1)*/
	JUMP_EQ(ANOTHER_CHECK_CHAR_TO_INTEGER)
	SHOW("for CHAR_TO_INTEGER procedure need to be 1 argument you send (print ->)",FPARG(1));
	HALT;
ANOTHER_CHECK_CHAR_TO_INTEGER:	
    MOV(R1,FPARG(2));
	CMP(IND(R1),T_CHAR); /*<- ARGUMENT NEED TO BE SOB_CHAR*/
	JUMP_EQ(ANOTHER_CHECK_2_CHAR_TO_INTEGER)
	SHOW("THE ARGUMENT NEED TO BE T_CHAR YOU SEND(print ->)",IND(R1));
	HALT;
ANOTHER_CHECK_2_CHAR_TO_INTEGER:	
	CMP(INDD(R1,1),IMM(0)); /*<- ARGUMENT NEED TO BE >0*/
	JUMP_GE(ANOTHER_CHECK_3_CHAR_TO_INTEGER)
	SHOW("THE ARGUMENT NEED TO BE T_CHAR >0 YOU SEND(print ->)",INDD(R1,1));
	HALT;
ANOTHER_CHECK_3_CHAR_TO_INTEGER:	
	MOV(R1, INDD(R1, 1));
	PUSH(R1);
	CALL(MAKE_SOB_INTEGER);
	DROP(1);
	POP(R1);
	MOV(SP,FP);
	POP(FP);
RETURN;
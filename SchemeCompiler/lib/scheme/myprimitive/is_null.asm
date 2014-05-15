/* is_null.asm  
 *check if argument is NULL and return #t otherwise return #f
 */
 
 IS_NULL:
 
	PUSH(FP);
	MOV(FP,SP);		/*OPEN NEW FRAME*/
	PUSH(R1);
	MOV(R0,IMM(3));  /*DEFOLT THE ANS IS FALSE */
	MOV(R1,FPARG(1)); /*SAVE THE ARGUMENTS NUMBER TO R1*/
	CMP(R1,IMM(1));   /*NEED TO BE ONLY 1 ARGUMENT*/
	JUMP_EQ(CHECK_IF_NULL);
	SHOW("bad input argument for IS_NULL? need to be one argument. R1 is the num of arguments",R1);
	HALT;
 CHECK_IF_NULL:
	CMP(IND(FPARG(2)),T_NIL); /*CHECK THE ONLY ARGUMENT IF IT T_NIL IF NOT JUMP IF YES ANS=#T*/
	JUMP_NE(EXIT_NULL);
	MOV(R0,IMM(5));  /*IF I GET HERE SO I CHECK AND THE ANS IS #T */
 EXIT_NULL:
	POP(R1);
	MOV(SP,FP);
	POP(FP);
 RETURN; 
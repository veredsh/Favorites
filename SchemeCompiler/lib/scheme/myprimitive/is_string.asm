/* is_string.asm  
 *check if argument is STRING and return #t otherwise return #f
 */
 
 IS_STRING:
 
	PUSH(FP);
	MOV(FP,SP);		/*OPEN NEW FRAME*/
	PUSH(R1);
	MOV(R0,IMM(3));  /*DEFOLT THE ANS IS FALSE */
	MOV(R1,FPARG(1)); /*SAVE THE ARGUMENTS NUMBER TO R1*/
	CMP(R1,IMM(1));   /*NEED TO BE ONLY 1 ARGUMENT*/
	JUMP_EQ(CHECK_IF_STRING);
	SHOW("bad input argument for IS_STRING? need to be one argument. R1 is the num of arguments",R1);
	HALT;
 CHECK_IF_STRING:
	CMP(IND(FPARG(2)),T_STRING); /*CHECK THE ONLY ARGUMENT IF IT T_STRING IF NOT JUMP IF YES ANS=#T*/
	JUMP_NE(EXIT_STRING);
	MOV(R0,IMM(5));  /*IF I GET HERE SO I CHECK AND THE ANS IS #T */
EXIT_STRING:
	POP(R1);
	MOV(SP,FP);
	POP(FP);
 RETURN; 
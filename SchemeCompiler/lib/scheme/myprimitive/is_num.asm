/* is_num.asm  
 *check if argument is number and return #t otherwise return #f
 */
 
 IS_NUM:
 
	PUSH(FP);
	MOV(FP,SP);		/*OPEN NEW FRAME*/
	PUSH(R1);
	MOV(R0,IMM(3));  /*DEFOLT THE ANS IS FALSE */
	MOV(R1,FPARG(1)); /*SAVE THE ARGUMENTS NUMBER TO R1*/
	CMP(R1,IMM(1)); /*NEED TO BE ONLY 1 ARGUMENT*/
	JUMP_EQ(CHECK_IF_NUM);
	SHOW("bad input argument for IS_NUM? need to be one argument. R1 is the num of arguments",R1);
	HALT;
 CHECK_IF_NUM:
	CMP(IND(FPARG(2)),T_INTEGER); /*CHECK THE ONLY ARGUMENT IF IT T_INTEGER IF NOT JUMP IF YES ANS=#T*/
	JUMP_NE(EXIT_NUM);
	MOV(R0,IMM(5));  /*IF I GET HERE SO I CHECK AND THE ANS IS #T */
EXIT_NUM:
	POP(R1);
	MOV(SP,FP);
	POP(FP);
 RETURN; 
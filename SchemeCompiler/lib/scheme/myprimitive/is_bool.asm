/* is_bool.asm  
 *check if argument is #t or #f and retur #t otherwise return #f
 */
 
 IS_BOOL:
 
	PUSH(FP);
	MOV(FP,SP);		/*OPEN NEW FRAME*/
	PUSH(R1);
	MOV(R0,IMM(3));  /*DEFOLT THE ANS IS FALSE */
	MOV(R1,FPARG(1)); /*SAVE THE ARGUMENTS NUMBER TO R1*/
	CMP(R1,IMM(1));  /*NEED TO BE ONLY 1 ARGUMENT*/
	JUMP_EQ(CHECK_IF_BOOL);
	SHOW("bad input argument for IS_BOOL? need to be one argument. R1 is the num of arguments",R1);
	HALT;
 CHECK_IF_BOOL:
	CMP(IND(FPARG(2)),T_BOOL); /*CHECK THE ONLY ARGUMENT IF IT T_BOOL IF NOT JUMP IF YES ANS=#T*/
	JUMP_NE(EXIT_BOOL);
	MOV(R0,IMM(5));  /*IF I GET HERE SO I CHECK AND THE ANS IS #T */
EXIT_BOOL:
	POP(R1);
	MOV(SP,FP);
	POP(FP);
 RETURN; 
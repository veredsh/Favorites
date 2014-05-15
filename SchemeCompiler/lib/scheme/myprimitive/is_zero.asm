/* is_zero.asm  
 *check if argument is ZERO and return #t otherwise return #f
 */
 
 MYISZERO:
 
	PUSH(FP);
	MOV(FP,SP);		/*OPEN NEW FRAME*/
	PUSH(R1);
	MOV(R0,IMM(3));  /*DEFOLT THE ANS IS FALSE */
	MOV(R1,FPARG(1)); /*SAVE THE ARGUMENTS NUMBER TO R1*/
	CMP(R1,IMM(1));   /*NEED TO BE ONLY 1 ARGUMENT*/
	JUMP_EQ(CHECK_IF_ZERO);
	SHOW("bad input argument for IS_ZERO? need to be one argument. R1 is the num of arguments",R1);
	HALT;
 CHECK_IF_ZERO:
	CMP(IND(FPARG(2)),T_INTEGER); /*CHECK THE ONLY ARGUMENT IF IT T_INTEGER IF NOT RAISE ERROR*/
	JUMP_EQ(CHECK_THE_NUMBER2);
	SHOW("bad input argument for IS_ZERO? the argument is not number/integer. FPARG(2) is the num of arguments",FPARG(2));
	HALT;
 CHECK_THE_NUMBER2:	
	CMP(INDD(FPARG(2),1),IMM(0)); /*CHECK THE ONLY ARGUMENT IF IT 0 IF NOT JUMP IF YES ANS=#T*/
	JUMP_NE(EXIT_ZERO);
	MOV(R0,IMM(5));  /*IF I GET HERE SO I CHECK AND THE ANS IS #T */
 EXIT_ZERO:
	POP(R1);
	MOV(SP,FP);
	POP(FP);
 RETURN; 
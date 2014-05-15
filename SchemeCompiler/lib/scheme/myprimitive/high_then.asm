/* high_then.asm  
 *high between N numbers: X1>X2> ... >Xn return #t or #f
 *args = 1 or more 
 *special (> 5) -> #t (>)-> ERROE
 */
 
 HIGHTHEN:
 
	PUSH(FP);
	MOV(FP,SP);		/*OPEN NEW FRAME*/
	PUSH(R1);
	PUSH(R2);
	PUSH(R3)
	MOV(R1,IMM(2)); /* R1<-2 TO BE COUNTER UP UNTIL FPARG(1)*/
	
	CMP(FPARG(1),IMM(0));
	JUMP_GT(HIGH_THEN_LOOP_HIGH_THEN);
	SHOW("bad input argument for > need to be one or more arguments. FPARG(1) is the num of arguments",FPARG(1));
	HALT;
 HIGH_THEN_LOOP_HIGH_THEN:
	CMP(R1,FPARG(1)); /*BECAUSE R2 GET ALREADY THE FGPARG(2) */
	JUMP_GT(HIGH_THEN_LOOP_END_HIGH_THEN);
	MOV(R2,FPARG(R1)); /*save the first argument to R2*/
	MOV(R3,FPARG(R1+1)); /*save the SECOND argument to R3*/
	CMP(IND(R2),T_INTEGER); /*CHECK IF THE ARGUMENT IN R2  IS INTEGER*/
	JUMP_EQ(CHECK_OTHER_NUM_HIGH_THEN);
	SHOW("The argumen in the R2  is not integer. print IND(R2)",IND(R2));
	HALT;
 CHECK_OTHER_NUM_HIGH_THEN:	
	CMP(IND(R3),T_INTEGER); /*CHECK IF THE ARGUMENT IN R3  IS INTEGER*/
	JUMP_EQ(ARG_IS_OK_HIGH_THEN);
	SHOW("The argumen in the R3  is not integer. print IND(R3)",IND(R3));
	HALT;
 ARG_IS_OK_HIGH_THEN:	
	CMP(INDD(R2,1),INDD(R3,1)); /*COMPERE THE FIRST ARG VALUE WITH THE NEXT ONE NEED TO BE HIGHER*/
	JUMP_LE(ANS_FALSE_HIGH_THEN);
	INCR(R1);
	JUMP(HIGH_THEN_LOOP_HIGH_THEN);
 HIGH_THEN_LOOP_END_HIGH_THEN:
	MOV(R0,IMM(5)); /*THE LOOP IS ENDED END ALL ARGS ARE FIT TO THE CONDITION NEED TO RETURN #t*/
	JUMP(END_HIGH_THEN_HIGH_THEN);
 ANS_FALSE_HIGH_THEN:
	MOV(R0,IMM(3)); /*IF I GET HERE SO I NEED TO RETURN #f IMM(3)=FALSE*/
 END_HIGH_THEN_HIGH_THEN:	
	POP(R3);
	POP(R2);
	POP(R1);
	MOV(SP,FP);
	POP(FP);
 RETURN; 
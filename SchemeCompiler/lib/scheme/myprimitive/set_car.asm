/* set_car.asm  
 *recive a pair scheme object and ARGUMENT and change the recived pair whit the new argument
 */
 
 SETCAR:
 
	PUSH(FP);
	MOV(FP,SP);		/*OPEN NEW FRAME*/
	PUSH(R1);
	PUSH(R2);
	PUSH(R3);
	MOV(R1,FPARG(1)); /*R1 <- ARGUMENT NUMBER (NEED TO BE 2)*/
	CMP(R1,IMM(2));
	JUMP_EQ(CONTINUE_SET_CAR)
	SHOW("for set_car! need to be 2 argument you send (print ->)",FPARG(1));
	HALT;
 CONTINUE_SET_CAR:	
	MOV(R2,FPARG(2)); /*PUT THE SOB_PAIR IN R2*/
	MOV(R3,FPARG(3)); /*the argument we wont to reaplace with*/
	CMP(IND(R2),T_PAIR);
	JUMP_EQ(ARG_IS_OK_SET_CAR);
	SHOW("The argumen is not list or pair. print the input for set_car",FPARG(2));
	HALT;
 ARG_IS_OK_SET_CAR:
	MOV(INDD(R2,1),R3); /*CHANGE THE CDR VALUE WHITH NEW ONE
	MOV(R0,IMM(2)); /*SET_CAR RETURN VOID*/ 
	POP(R3);
	POP(R2);
	POP(R1);
	MOV(SP,FP);
	POP(FP);
 RETURN; 
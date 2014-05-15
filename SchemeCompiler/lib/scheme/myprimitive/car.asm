/* car.asm  
 *recive a pair scheme object and put in R0 the car of it if not make error and halt
 */
 
 CAR:
	
	PUSH(FP);
	MOV(FP,SP);		/*OPEN NEW FRAME*/
	
	PUSH(R1);
	CMP(FPARG(1),IMM(1)); /*<- ARGUMENT NUMBER (NEED TO BE 1)*/
	JUMP_EQ(CONTINUE_CAR)
	SHOW("for car need to be 1 argument you send (print ->)",FPARG(1));
	HALT;
 CONTINUE_CAR:	
	MOV(R1,FPARG(2)); /*PUT THE SOB_PAIR IN R1*/
	CMP(IND(R1),T_PAIR);
	JUMP_EQ(ARG_IS_OK_CAR);
	SHOW("The argumen is not list or pair. print the input for car",FPARG(2));
	HALT;
 ARG_IS_OK_CAR:
	MOV(R0,INDD(R1,1)); /*PUT IN R0 THE CAR VALUE FROM SOB_PAIR (THAT SEAT IN THE SECOND WORD (INDD(R1,1)))*/
	POP(R1);
	POP(FP);
 RETURN; 
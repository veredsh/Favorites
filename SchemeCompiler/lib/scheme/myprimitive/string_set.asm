/* string_set.asm
 * RECIVE pointer to a SOB_STRING number and char, and replace the n'th char of it whit given char

 */

 STRING_SET:
	  PUSH(FP);
	  MOV(FP, SP);

	  PUSH(R1);
	  PUSH(R2);
	  PUSH(R3);
	  CMP(FPARG(1),IMM(3)); /*NEED TO BE 3 ARGUMENT FOR THIS PROCEDURE*/
	  JUMP_EQ(ANOTHER_CHECK_STRING_SET);
	  SHOW("bad input argument for STRING_SET! need to be 3  arguments. PRINT->",FPARG(1));
	  HALT;
 ANOTHER_CHECK_STRING_SET:
	  MOV(R1, FPARG(2)); /*R1<- THE STRING*/
	  MOV(R2, FPARG(3)); /*R2<- THE NUM */
	  MOV(R3,FPARG(4));  /*R3<- THE CHAR WE WONT TO ENTER TO THE STRING */
	  CMP(IND(R1),T_STRING); /*CHECK IF ITS SOB_STRING*/
	  JUMP_EQ(ANOTHER_CHECK_2_STRING_SET);
	  SHOW("bad input THE FIRST ARGUMENT IS NOT A STRING. PRINT->",IND(R1));
	  HALT;
 ANOTHER_CHECK_2_STRING_SET:
	  CMP(IND(R2),T_INTEGER); /*CHECK IF ITS SOB_INTEGER*/
	  JUMP_EQ(ANOTHER_CHECK_3_STRING_SET);
	  SHOW("bad input THE SECOND ARGUMENT IS NOT SOB_INTEGER). PRINT->",IND(R2));
	  HALT;
 ANOTHER_CHECK_3_STRING_SET:	
	  CMP(INDD(R2,1),IMM(0)); /*CHECK IF the number  is <0 */
	  JUMP_GE(ANOTHER_CHECK_4_STRING_SET);
	  SHOW("bad input THE SECOND ARGUMENT NEED TO BE >0 . PRINT->",INDD(R2,1));
	  HALT;
 ANOTHER_CHECK_4_STRING_SET:
	  CMP(INDD(R2,1),INDD(R1,1)); /*CHECK IF the number  is < string length*/
	  JUMP_LT(ANOTHER_CHECK_5_STRING_SET);
	  SHOW("bad input THE SECOND ARGUMENT NEED TO BE  < (length string). PRINT->",INDD(R2,1));
	  HALT;
 ANOTHER_CHECK_5_STRING_SET:	  
	  CMP(INDD(R3,1),T_CHAR); /*CHECK IF 3 ARGUMENT IS T_CHAR */
	  JUMP_LT(ANOTHER_CHECK_6_STRING_SET);
	  SHOW("bad input THE 3 ARGUMENT IS NOT A CHAR. PRINT->",INDD(R3,1));
	  HALT;
 ANOTHER_CHECK_6_STRING_SET:	 
	  ADD(R1, IMM(2));
	  ADD(R1, INDD(R2, 1));
	  MOV(IND(R1), INDD(R3,1)); /*IND(R1) POINT TO THE N'TH CHAR IN THE GIVVEN STRING AND WE REPLACE THAT CHAR WHITH THE NEW ONE IN INDD(R3,1)*/
	  MOV(R0,IMM(2)); /*STRING-SET! RETURN VOID*/
	  POP(R3);
	  POP(R2);
	  POP(R1);
	  MOV(SP, FP);	  
	  POP(FP);
 RETURN;



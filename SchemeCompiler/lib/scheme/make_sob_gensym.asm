/* scheme/make_sob_gensym.asm
 * take a pointer to bucket and place the corresponding Scheme object in R0
 * 
 * Programmer: Vered & Uriya, 2012
 */

 MAKE_SOB_GENSYM:
  PUSH(FP);
  MOV(FP, SP);
  PUSH(IMM(2));
  CALL(MALLOC);
  DROP(1);
  MOV(IND(R0), T_GENSYM);
  MOV(INDD(R0, 1), FPARG(0));
  POP(FP);
  RETURN;

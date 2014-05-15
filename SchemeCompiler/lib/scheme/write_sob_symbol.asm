/* scheme/write_sob_symbol.asm
 * Take a pointer to a Scheme symbol object, and 
 * prints (to stdout) the character representation
 * of that object.
 * 
 * Programmers: Vered & Uriya, 2012
 */

 WRITE_SOB_SYMBOL:
  PUSH(FP);
  MOV(FP, SP);
  PUSH(R1);
  PUSH(R2);
  MOV(R0, FPARG(0)); /* R0 is a pointer to SYMBOL object */
  MOV(R1, INDD(R0, 1)); /* R1 is a pointer to the bucket */
  MOV(R2, IND(R1)); /* R2 is a pointer to T_STRING object */
  MOV(R1, INDD(R2, 1)); /* R1 is the length of the string */
  ADD(R2, IMM(2)); /* R2 is a pointer to the first char */
 L_WSYM_LOOP:
  CMP(R1, IMM(0));
  JUMP_EQ(L_WSYM_EXIT);
  PUSH(IND(R2));
  CALL(PUTCHAR);
  DROP(1);
  INCR(R2);
  DECR(R1);
  JUMP(L_WSYM_LOOP);
 L_WSYM_EXIT:
  POP(R2);
  POP(R1);
  POP(FP);
  RETURN;


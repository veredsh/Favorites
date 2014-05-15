/* scheme/myprimitive/make_vector.asm
 * take integer (and optionally scheme object) as argument 
 * put new vector sob in r0
 */

 MAKE_VECTOR:
 
	PUSH(FP);
	MOV(FP, SP);
	PUSH(R1);
	PUSH(R2);
	PUSH(R3);
	/* put first arg in R1 */
	MOV(R1, FPARG(2));
	/* put integer value in R1 */
	MOV(R1, INDD(R1, 1));
	/* put in R2 the vector content */
	CMP(FPARG(1), IMM(2));
	JUMP_EQ(GET_SOB_ARG);
	MOV(R2, IMM(SOB_NIL));
	JUMP(MAKE_VECTOR_START_LOOP);
GET_SOB_ARG:
	MOV(R2, FPARG(3));
MAKE_VECTOR_START_LOOP:
	MOV(R3, R1);
MAKE_VECTOR_LOOP:
	CMP(R3, IMM(0));
	JUMP_EQ(MAKE_VECTOR_LOOP_END);
	PUSH(R2);
	DECR(R3);
	JUMP(MAKE_VECTOR_LOOP);
MAKE_VECTOR_LOOP_END:
	/* push vector size */
	PUSH(R1);
	CALL(MAKE_SOB_VECTOR);
	INCR(R1);
	DROP(R1);
	POP(R3);
	POP(R2);
	POP(R1);
	POP(FP);
	RETURN;
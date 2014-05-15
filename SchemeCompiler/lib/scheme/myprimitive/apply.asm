/* lib/scheme/myprimitive/apply.asm
 * receive pointer to sob T_CLOSURE and pointer to sob T_PAIR
 * apply the given procedure on the given arg list
 */

 APPLY:
	PUSH(FP);
	MOV(FP, SP);
	/* reverse the given list */
	MOV(R11, FPARG(3));
	/* R11 is a pointer to T_PAIR sob */
	MOV(R12, IMM(SOB_NIL));
	/* R12 will hold the reversed list */
	
 REVERSE_ARG_LIST:
	CMP(R11, IMM(SOB_NIL));
	JUMP_EQ(REVERSE_ARG_LIST_END);
	
	/* R11 is a pointer to T_PAIR sob */
	/* push new cdr */
	PUSH(R12);
	/* push new car */
	PUSH(INDD(R11, 1));
	CALL(MAKE_SOB_PAIR);
	DROP(2);
	
	MOV(R12, R0);
	/* set R11 to cdr */
	MOV(R11, INDD(R11, 2));
	JUMP(REVERSE_ARG_LIST);
	
 REVERSE_ARG_LIST_END:
	/* R12 is reversed list */
	/* push elements in R12 */
	MOV(R13, IMM(0));
	/* R13 is length of elements */
	
 PUSH_ARG_LIST_LOOP:
	CMP(R12, IMM(SOB_NIL));
	JUMP_EQ(PUSH_ARG_LIST_LOOP_END);
	
	PUSH(INDD(R12, 1));
	INCR(R13);
	/* set R12 to cdr */
	MOV(R12, INDD(R12, 2));
	JUMP(PUSH_ARG_LIST_LOOP);
	
 PUSH_ARG_LIST_LOOP_END:
	/* push num of args */
	PUSH(R13);
	/* put proc in R11 */
	MOV(R11, FPARG(2));
	/* push env */
	PUSH(INDD(R11, 1));
	/* runover apply's frame */
	/* push old ret address */
	PUSH(FPARG(-1));
	/* PUSH OLD FP */
	PUSH(FPARG(-2));
	
	/* R13 IS THE LOWEST ARG TO MOVE DOWN */
	ADD(R13, IMM(4));
	
RUNOVER_APPLY_LOOP:
	CMP(R13, IMM(0));
	JUMP_EQ(RUNOVER_APPLY_LOOP_END);
	
	MOV(R12, SP);
	SUB(R12, R13);
	MOV(R0, STACK(R12));
	SUB(R12, IMM(6));
	MOV(STACK(R12), R0);
	
	DECR(R13);
	JUMP(RUNOVER_APPLY_LOOP);
	
RUNOVER_APPLY_LOOP_END:
	SUB(SP, IMM(6));
	POP(FP);
	JUMPA(INDD(R11, 2));
	
	
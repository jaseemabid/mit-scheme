/* -*-C-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/interp.c,v 9.60 1990/11/27 19:13:48 cph Rel $

Copyright (c) 1988, 1989, 1990 Massachusetts Institute of Technology

This material was developed by the Scheme project at the Massachusetts
Institute of Technology, Department of Electrical Engineering and
Computer Science.  Permission to copy this software, to redistribute
it, and to use it for any purpose is granted, subject to the following
restrictions and understandings.

1. Any copy made of this software must include this copyright notice
in full.

2. Users of this software agree to make their best efforts (a) to
return to the MIT Scheme project any improvements or extensions that
they make, so that these may be included in future releases; and (b)
to inform MIT of noteworthy uses of this software.

3. All materials developed as a consequence of the use of this
software shall duly acknowledge such use, in accordance with the usual
standards of acknowledging credit in academic research.

4. MIT has made no warrantee or representation that the operation of
this software will be error-free, and MIT is under no obligation to
provide any services, by way of maintenance, update, or otherwise.

5. In conjunction with products arising from the use of this material,
there shall be no use of the name of the Massachusetts Institute of
Technology nor of any adaptation thereof in any advertising,
promotional, or sales literature without prior written consent from
MIT in each case. */

/* This file contains the heart of the SCode interpreter. */

#define In_Main_Interpreter true
#include "scheme.h"
#include "locks.h"
#include "trap.h"
#include "lookup.h"
#include "winder.h"
#include "history.h"
#include "cmpint.h"
#include "zones.h"
#include "prmcon.h"

extern PTR EXFUN (obstack_chunk_alloc, (unsigned int size));
extern void EXFUN (free, (PTR ptr));
#define obstack_chunk_free free

/* In order to make the interpreter tail recursive (i.e.
 * to avoid calling procedures and thus saving unnecessary
 * state information), the main body of the interpreter
 * is coded in a continuation passing style.
 *
 * Basically, this is done by dispatching on the type code
 * for an Scode item.  At each dispatch, some processing
 * is done which may include setting the return address
 * register, saving the current continuation (return address
 * and current expression) and jumping to the start of
 * the interpreter.
 *
 * It may be helpful to think of this program as being what
 * you would get if you wrote the straightforward Scheme
 * interpreter and then converted it into continuation
 * passing style as follows.  At every point where you would
 * call EVAL to handle a sub-form, you put a jump back to
 * Do_Expression.  Now, if there was code after the call to
 * EVAL you first push a "return code" (using Save_Cont) on
 * the stack and move the code that used to be after the
 * call down into the part of this file after the tag
 * Pop_Return.
 *
 * Notice that because of the caller saves convention used
 * here, all of the registers which are of interest have
 * been SAVEd on the racks by the time interpretation arrives
 * at Do_Expression (the top of EVAL).
 *
 * For notes on error handling and interrupts, see the file
 * utils.c.
 *
 * This file is divided into two parts. The first
 * corresponds is called the EVAL dispatch, and is ordered
 * alphabetically by the SCode item handled.  The second,
 * called the return dispatch, begins at Pop_Return and is
 * ordered alphabetically by return code name.
 */

#define Prepare_Pop_Return_Interrupt(Return_Code, Contents_of_Val)	\
{									\
  SCHEME_OBJECT temp;							\
									\
  temp = (Contents_of_Val);						\
  Store_Return(Return_Code);						\
  Save_Cont();								\
  Store_Return(RC_RESTORE_VALUE);					\
  Store_Expression(temp);						\
  Save_Cont();								\
}

#define Interrupt(Masked_Code)						\
{									\
  Export_Registers();							\
  Setup_Interrupt(Masked_Code);						\
  Import_Registers();							\
  goto Perform_Application;						\
}

#define Immediate_GC(N)							\
{									\
  Request_GC(N);							\
  Interrupt(PENDING_INTERRUPTS());					\
}

#define Eval_GC_Check(Amount)						\
if (GC_Check(Amount))							\
{									\
  Prepare_Eval_Repeat();						\
  Immediate_GC(Amount);							\
}

#define RESULT_OF_PURIFY(success)					\
{									\
  SCHEME_OBJECT words_free;						\
									\
  words_free = (LONG_TO_UNSIGNED_FIXNUM (MemTop - Free));		\
  Val = (MAKE_POINTER_OBJECT (TC_LIST, Free));				\
  (*Free++) = (success);						\
  (*Free++) = words_free;						\
}

#define Prepare_Eval_Repeat()						\
{									\
 Will_Push(CONTINUATION_SIZE+1);					\
  STACK_PUSH (Fetch_Env());						\
  Store_Return(RC_EVAL_ERROR);						\
  Save_Cont();								\
 Pushed();								\
}

#define Eval_Error(Err)							\
{									\
  Export_Registers();							\
  Do_Micro_Error(Err, false);						\
  Import_Registers();							\
  goto Internal_Apply;							\
}

#define Pop_Return_Error(Err)						\
{									\
  Export_Registers();							\
  Do_Micro_Error(Err, true);						\
  Import_Registers();							\
  goto Internal_Apply;							\
}

#define BACK_OUT_AFTER_PRIMITIVE()					\
{									\
  Export_Registers();							\
  Back_Out_Of_Primitive();						\
  Import_Registers();							\
}

#define Reduces_To(Expr)						\
	{ Store_Expression(Expr);					\
          New_Reduction(Fetch_Expression(), Fetch_Env());		\
          goto Do_Expression;						\
        }

#define Reduces_To_Nth(N)						\
        Reduces_To(FAST_MEMORY_REF (Fetch_Expression(), (N)))

#define Do_Nth_Then(Return_Code, N, Extra)				\
	{ Store_Return(Return_Code);					\
	  Save_Cont();							\
	  Store_Expression(FAST_MEMORY_REF (Fetch_Expression(), (N)));	\
	  New_Subproblem(Fetch_Expression(), Fetch_Env());		\
          Extra;							\
	  goto Do_Expression;						\
        }

#define Do_Another_Then(Return_Code, N)					\
	{ Store_Return(Return_Code);					\
          Save_Cont();							\
	  Store_Expression(FAST_MEMORY_REF (Fetch_Expression(), (N)));	\
	  Reuse_Subproblem(Fetch_Expression(), Fetch_Env());		\
	  goto Do_Expression;						\
        }

                      /***********************/
                      /* Macros for Stepping */
                      /***********************/

#define Fetch_Trapper(field)	\
  MEMORY_REF (Get_Fixed_Obj_Slot(Stepper_State), (field))

#define Fetch_Eval_Trapper() Fetch_Trapper(HUNK_CXR0)
#define Fetch_Apply_Trapper() Fetch_Trapper(HUNK_CXR1)
#define Fetch_Return_Trapper() Fetch_Trapper(HUNK_CXR2)

/* Macros for handling FUTUREs */

#ifdef COMPILE_FUTURES

/* ARG_TYPE_ERROR handles the error returns from primitives which type check
   their arguments and restarts them or suspends if the argument is a future.
 */

#define ARG_TYPE_ERROR(Arg_No, Err_No)					\
{									\
  fast SCHEME_OBJECT *Arg, Orig_Arg;					\
									\
  Arg = &(STACK_REF((Arg_No - 1) + STACK_ENV_FIRST_ARG));		\
  Orig_Arg = *Arg;							\
									\
  if (OBJECT_TYPE (*Arg) != TC_FUTURE)					\
  {									\
    Pop_Return_Error(Err_No);						\
  }									\
									\
  while ((OBJECT_TYPE (*Arg) == TC_FUTURE) && (Future_Has_Value(*Arg)))	\
  {									\
    if (Future_Is_Keep_Slot(*Arg))					\
    {									\
      Log_Touch_Of_Future(*Arg);					\
    }									\
    *Arg = Future_Value(*Arg);						\
  }									\
  if (OBJECT_TYPE (*Arg) != TC_FUTURE)					\
  {									\
    goto Apply_Non_Trapping;						\
  }									\
									\
  TOUCH_SETUP(*Arg);							\
  *Arg = Orig_Arg;							\
  goto Apply_Non_Trapping;						\
}

/* Apply_Future_Check is called at apply time to guarantee that certain
   objects (the procedure itself, and its LAMBDA components for user defined
   procedures) are not futures
*/

#define Apply_Future_Check(Name, Object)				\
{									\
  fast SCHEME_OBJECT *Arg, Orig_Answer;					\
									\
  Arg = &(Object);							\
  Orig_Answer = *Arg;							\
									\
  while (OBJECT_TYPE (*Arg) == TC_FUTURE)				\
  {									\
    if (Future_Has_Value(*Arg))						\
    {									\
      if (Future_Is_Keep_Slot(*Arg))					\
      {									\
	Log_Touch_Of_Future(*Arg);					\
      }									\
      *Arg = Future_Value(*Arg);					\
    }									\
    else								\
    {									\
      Prepare_Apply_Interrupt ();					\
      TOUCH_SETUP (*Arg);						\
      *Arg = Orig_Answer;						\
      goto Internal_Apply;						\
    }									\
  }									\
  Name = *Arg;								\
}

/* Future handling macros continue on the next page */

/* Future handling macros, continued */

/* Pop_Return_Val_Check suspends the process if the value calculated by
   a recursive call to EVAL is an undetermined future */

#define Pop_Return_Val_Check()						\
{									\
  fast SCHEME_OBJECT Orig_Val = Val;					\
									\
  while (OBJECT_TYPE (Val) == TC_FUTURE)				\
  {									\
    if (Future_Has_Value(Val))						\
    {									\
      if (Future_Is_Keep_Slot(Val))					\
      {									\
	Log_Touch_Of_Future(Val);					\
      }									\
      Val = Future_Value(Val);						\
    }									\
    else								\
    {									\
      Save_Cont();							\
     Will_Push(CONTINUATION_SIZE + (STACK_ENV_EXTRA_SLOTS + 2));	\
      Store_Return(RC_RESTORE_VALUE);					\
      Store_Expression(Orig_Val);					\
      Save_Cont();							\
      STACK_PUSH (Val);							\
      STACK_PUSH (Get_Fixed_Obj_Slot(System_Scheduler));		\
      STACK_PUSH (STACK_FRAME_HEADER + 1);				\
     Pushed();								\
      goto Internal_Apply;						\
    }									\
  }									\
}

/* This saves stuff unnecessarily in most cases.
   For example, when Which_Way is PRIM_APPLY, Val, Env, Expr,
   and Return_Code are undefined.
 */

#define LOG_FUTURES()							\
{									\
  if (Must_Report_References())						\
  {									\
    Save_Cont();							\
   Will_Push(CONTINUATION_SIZE + 2);					\
    STACK_PUSH (Val);							\
    Save_Env();								\
    Store_Return(RC_REPEAT_DISPATCH);					\
    Store_Expression(LONG_TO_FIXNUM(CODE_MAP(Which_Way)));		\
    Save_Cont();							\
   Pushed();								\
    Call_Future_Logging();						\
 }									\
}

#else /* not COMPILE_FUTURES */

#define Pop_Return_Val_Check()

#define Apply_Future_Check(Name, Object)	Name = (Object)

#define ARG_TYPE_ERROR(Arg_No, Err_No)					\
{									\
  Pop_Return_Error(Err_No)						\
}

#define LOG_FUTURES()

#endif /* COMPILE_FUTURES */

/* Notes on Repeat_Dispatch:

   The codes used (values of Which_Way) are divided into two groups:
   Those for which the primitive has already backed out, and those for
   which the back out code has not yet been executed, and is therefore
   executed below.

   Under most circumstances the distinction is moot, but if there are
   futures in the system, and future touches must be logged, the code
   must be set up to "interrupt" the dispatch, and proceed it later.
   The primitive back out code must be done before the furure is
   logged, so all of these codes are split into two versions: one set
   before doing the back out, and another afterwards.
 */

/* This is assumed to be larger (in absolute value) than any PRIM_<mumble>
   and ERR_<mumble>.
 */
#define PRIM_BIAS_AMOUNT 1000

#if (MAX_ERROR >= PRIM_BIAS_AMOUNT)
#include "Inconsistency: errors.h and interp.c"
#endif

#define CODE_MAP(code)							\
((code < 0) ?								\
 (code - PRIM_BIAS_AMOUNT) :						\
 (code + PRIM_BIAS_AMOUNT))

#define CODE_UNMAP(code)						\
((code < 0) ?								\
 (code + PRIM_BIAS_AMOUNT) :						\
 (code - PRIM_BIAS_AMOUNT))

#define CODE_MAPPED_P(code)						\
((code < (- PRIM_BIAS_AMOUNT)) ||					\
 (code >= PRIM_BIAS_AMOUNT))

#define PROCEED_AFTER_PRIMITIVE()					\
{									\
  (Regs [REGBLOCK_PRIMITIVE]) = SHARP_F;				\
  LOG_FUTURES ();							\
}

/*
  The EVAL/APPLY ying/yang
 */

static PTR interpreter_catch_dstack_position;
static jmp_buf interpreter_catch_env;
static int interpreter_throw_argument;

void
DEFUN (abort_to_interpreter, (argument), int argument)
{
  interpreter_throw_argument = argument;
  dstack_set_position (interpreter_catch_dstack_position);
  obstack_free ((&scratch_obstack), 0);
  obstack_init (&scratch_obstack);
  longjmp (interpreter_catch_env, argument);
}

int
DEFUN_VOID (abort_to_interpreter_argument)
{
  return (interpreter_throw_argument);
}

void
Interpret(dumped_p)
     Boolean dumped_p;
{
  long Which_Way;
  fast SCHEME_OBJECT *Reg_Block, *Reg_Stack_Pointer, *Reg_History;

  extern long enter_compiled_expression();
  extern long apply_compiled_procedure();
  extern long return_to_compiled_code();

  Reg_Block = &Registers[0];

  /* Primitives jump back here for errors, requests to evaluate an
   * expression, apply a function, or handle an interrupt request.  On
   * errors or interrupts they leave their arguments on the stack, the
   * primitive itself in Expression.  The code should do a primitive
   * backout in these cases, but not in others (apply, eval, etc.), since
   * the primitive itself will have left the state of the interpreter ready
   * for operation.
   */

  interpreter_catch_dstack_position = dstack_position;
  Which_Way = (setjmp (interpreter_catch_env));
  Set_Time_Zone (Zone_Working);
  Import_Registers ();

Repeat_Dispatch:
  switch (Which_Way)
  {
    case PRIM_APPLY:
      PROCEED_AFTER_PRIMITIVE();
    case CODE_MAP(PRIM_APPLY):
      goto Internal_Apply;

    case PRIM_NO_TRAP_APPLY:
      PROCEED_AFTER_PRIMITIVE();
    case CODE_MAP(PRIM_NO_TRAP_APPLY):
      goto Apply_Non_Trapping;

    case PRIM_DO_EXPRESSION:
      Val = Fetch_Expression();
      PROCEED_AFTER_PRIMITIVE();
    case CODE_MAP(PRIM_DO_EXPRESSION):
      Reduces_To(Val);

    case PRIM_NO_TRAP_EVAL:
      Val = Fetch_Expression();
      PROCEED_AFTER_PRIMITIVE();
    case CODE_MAP(PRIM_NO_TRAP_EVAL):
      New_Reduction(Val, Fetch_Env());
      goto Eval_Non_Trapping;

    case 0:			/* first time */
      if (dumped_p)
      {
	goto Pop_Return;
      }
      else
      {
	break;			/* fall into eval */
      }

    case PRIM_POP_RETURN:
      PROCEED_AFTER_PRIMITIVE();
    case CODE_MAP(PRIM_POP_RETURN):
      goto Pop_Return;

    case PRIM_REENTER:
      BACK_OUT_AFTER_PRIMITIVE();
      LOG_FUTURES();
    case CODE_MAP(PRIM_REENTER):
      goto Perform_Application;

    case PRIM_TOUCH:
    {
      SCHEME_OBJECT temp;

      temp = Val;
      BACK_OUT_AFTER_PRIMITIVE();
      Val = temp;
      LOG_FUTURES();
    }
    /* fall through */
    case CODE_MAP(PRIM_TOUCH):
      TOUCH_SETUP(Val);
      goto Internal_Apply;

    case PRIM_INTERRUPT:
      BACK_OUT_AFTER_PRIMITIVE();
      LOG_FUTURES();
      /* fall through */
    case CODE_MAP(PRIM_INTERRUPT):
      Save_Cont();
      Interrupt(PENDING_INTERRUPTS());

    case ERR_ARG_1_WRONG_TYPE:
      BACK_OUT_AFTER_PRIMITIVE();
      LOG_FUTURES();
      /* fall through */
    case CODE_MAP(ERR_ARG_1_WRONG_TYPE):
      ARG_TYPE_ERROR(1, ERR_ARG_1_WRONG_TYPE);

    case ERR_ARG_2_WRONG_TYPE:
      BACK_OUT_AFTER_PRIMITIVE();
      LOG_FUTURES();
      /* fall through */
    case CODE_MAP(ERR_ARG_2_WRONG_TYPE):
      ARG_TYPE_ERROR(2, ERR_ARG_2_WRONG_TYPE);

    case ERR_ARG_3_WRONG_TYPE:
      BACK_OUT_AFTER_PRIMITIVE();
      LOG_FUTURES();
      /* fall through */
    case CODE_MAP(ERR_ARG_3_WRONG_TYPE):
      ARG_TYPE_ERROR(3, ERR_ARG_3_WRONG_TYPE);

    default:
    {
      if (!CODE_MAPPED_P(Which_Way))
      {
	BACK_OUT_AFTER_PRIMITIVE();
	LOG_FUTURES();
      }
      else
      {
	Which_Way = CODE_UNMAP(Which_Way);
      }
      Pop_Return_Error(Which_Way);
    }
  }

Do_Expression:

  if (Eval_Debug)
  { Print_Expression(Fetch_Expression(), "Eval, expression");
    printf ("\n");
  }

/* The expression register has an Scode item in it which
 * should be evaluated and the result left in Val.
 *
 * A "break" after the code for any operation indicates that
 * all processing for this operation has been completed, and
 * the next step will be to pop a return code off the stack
 * and proceed at Pop_Return.  This is sometimes called
 * "executing the continuation" since the return code can be
 * considered the continuation to be performed after the
 * operation.
 *
 * An operation can terminate with a Reduces_To or
 * Reduces_To_Nth macro.  This indicates that the  value of
 * the current Scode item is the value returned when the
 * new expression is evaluated.  Therefore no new
 * continuation is created and processing continues at
 * Do_Expression with the new expression in the expression
 * register.
 *
 * Finally, an operation can terminate with a Do_Nth_Then
 * macro.  This indicates that another expression must be
 * evaluated and them some additional processing will be
 * performed before the value of this S-Code item available.
 * Thus a new continuation is created and placed on the
 * stack (using Save_Cont), the new expression is placed in
 * the Expression register, and processing continues at
 * Do_Expression.
 */

/* Handling of Eval Trapping.

   If we are handling traps and there is an Eval Trap set,
   turn off all trapping and then go to Internal_Apply to call the
   user supplied eval hook with the expression to be evaluated and the
   environment. */

  if (Microcode_Does_Stepping &&
      Trapping &&
      ((Fetch_Eval_Trapper ()) != SHARP_F))
  {
    Stop_Trapping ();
   Will_Push (4);
    STACK_PUSH (Fetch_Env ());
    STACK_PUSH (Fetch_Expression ());
    STACK_PUSH (Fetch_Eval_Trapper ());
    STACK_PUSH (STACK_FRAME_HEADER + 2);
   Pushed ();
    goto Apply_Non_Trapping;
  }

Eval_Non_Trapping:
  Eval_Ucode_Hook();
  switch (OBJECT_TYPE (Fetch_Expression()))
  {
    default:
#if false
      Eval_Error(ERR_UNDEFINED_USER_TYPE);
#else
      /* fall through to self evaluating. */
#endif

    case TC_BIG_FIXNUM:         /* The self evaluating items */
    case TC_BIG_FLONUM:
    case TC_CHARACTER_STRING:
    case TC_CHARACTER:
    case TC_COMPILED_CODE_BLOCK:
    case TC_COMPLEX:
    case TC_CONTROL_POINT:
    case TC_DELAYED:
    case TC_ENTITY:
    case TC_ENVIRONMENT:
    case TC_EXTENDED_PROCEDURE:
    case TC_FIXNUM:
    case TC_HUNK3_A:
    case TC_HUNK3_B:
    case TC_INTERNED_SYMBOL:
    case TC_LIST:
    case TC_NON_MARKED_VECTOR:
    case TC_NULL:
    case TC_PRIMITIVE:
    case TC_PROCEDURE:
    case TC_QUAD:
    case TC_RATNUM:
    case TC_REFERENCE_TRAP:
    case TC_RETURN_CODE:
    case TC_UNINTERNED_SYMBOL:
    case TC_TRUE:
    case TC_VECTOR:
    case TC_VECTOR_16B:
    case TC_VECTOR_1B:
      Val = Fetch_Expression();
      break;

    case TC_ACCESS:
     Will_Push(CONTINUATION_SIZE);
      Do_Nth_Then(RC_EXECUTE_ACCESS_FINISH, ACCESS_ENVIRONMENT, Pushed());

    case TC_ASSIGNMENT:
     Will_Push(CONTINUATION_SIZE + 1);
      Save_Env();
      Do_Nth_Then(RC_EXECUTE_ASSIGNMENT_FINISH, ASSIGN_VALUE, Pushed());

    case TC_BROKEN_HEART:
      Export_Registers();
      Microcode_Termination(TERM_BROKEN_HEART);

/* Interpret() continues on the next page */

/* Interpret(), continued */

    case TC_COMBINATION:
      {
	long Array_Length;

	Array_Length = (VECTOR_LENGTH (Fetch_Expression()) - 1);
#ifdef USE_STACKLETS
	/* Save_Env, Finger */
        Eval_GC_Check(New_Stacklet_Size(Array_Length + 1 + 1 + CONTINUATION_SIZE));
#endif /* USE_STACKLETS */
       Will_Push(Array_Length + 1 + 1 + CONTINUATION_SIZE);
	Stack_Pointer = (STACK_LOC (- Array_Length));
        STACK_PUSH (MAKE_OBJECT (TC_MANIFEST_NM_VECTOR, Array_Length));
	/* The finger: last argument number */
       Pushed();
        if (Array_Length == 0)
	{
	  STACK_PUSH (STACK_FRAME_HEADER);   /* Frame size */
          Do_Nth_Then(RC_COMB_APPLY_FUNCTION, COMB_FN_SLOT, {});
	}
	Save_Env();
	Do_Nth_Then(RC_COMB_SAVE_VALUE, Array_Length+1, {});
      }

    case TC_COMBINATION_1:
     Will_Eventually_Push(CONTINUATION_SIZE + STACK_ENV_FIRST_ARG + 1);
      Save_Env();
      Do_Nth_Then(RC_COMB_1_PROCEDURE, COMB_1_ARG_1, {});

    case TC_COMBINATION_2:
     Will_Eventually_Push(CONTINUATION_SIZE + STACK_ENV_FIRST_ARG + 2);
      Save_Env();
      Do_Nth_Then(RC_COMB_2_FIRST_OPERAND, COMB_2_ARG_2, {});

    case TC_COMMENT:
      Reduces_To_Nth(COMMENT_EXPRESSION);

    case TC_CONDITIONAL:
     Will_Push(CONTINUATION_SIZE + 1);
      Save_Env();
      Do_Nth_Then(RC_CONDITIONAL_DECIDE, COND_PREDICATE, Pushed());

    case TC_COMPILED_ENTRY:
      {
	SCHEME_OBJECT compiled_expression;

	compiled_expression = (Fetch_Expression ());
	execute_compiled_setup();
	Store_Expression (compiled_expression);
	Export_Registers();
	Which_Way = enter_compiled_expression();
	goto return_from_compiled_code;
      }

/* Interpret() continues on the next page */

/* Interpret(), continued */

    case TC_DEFINITION:
     Will_Push(CONTINUATION_SIZE + 1);
      Save_Env();
      Do_Nth_Then(RC_EXECUTE_DEFINITION_FINISH, DEFINE_VALUE, Pushed());

    case TC_DELAY:
      /* Deliberately omitted: Eval_GC_Check(2); */
      Val = MAKE_POINTER_OBJECT (TC_DELAYED, Free);
      Free[THUNK_ENVIRONMENT] = Fetch_Env();
      Free[THUNK_PROCEDURE] =
        FAST_MEMORY_REF (Fetch_Expression(), DELAY_OBJECT);
      Free += 2;
      break;

    case TC_DISJUNCTION:
     Will_Push(CONTINUATION_SIZE + 1);
      Save_Env();
      Do_Nth_Then(RC_DISJUNCTION_DECIDE, OR_PREDICATE, Pushed());

    case TC_EXTENDED_LAMBDA:	/* Close the procedure */
    /* Deliberately omitted: Eval_GC_Check(2); */
      Val = MAKE_POINTER_OBJECT (TC_EXTENDED_PROCEDURE, Free);
      Free[PROCEDURE_LAMBDA_EXPR] = Fetch_Expression();
      Free[PROCEDURE_ENVIRONMENT] = Fetch_Env();
      Free += 2;
      break;

/* Interpret() continues on the next page */

/* Interpret(), continued */

#ifdef COMPILE_FUTURES
    case TC_FUTURE:
      if (Future_Has_Value(Fetch_Expression()))
      { SCHEME_OBJECT Future = Fetch_Expression();
        if (Future_Is_Keep_Slot(Future)) Log_Touch_Of_Future(Future);
        Reduces_To_Nth(FUTURE_VALUE);
      }
      Prepare_Eval_Repeat();
     Will_Push(STACK_ENV_EXTRA_SLOTS+2);
      STACK_PUSH (Fetch_Expression());	/* Arg: FUTURE object */
      STACK_PUSH (Get_Fixed_Obj_Slot(System_Scheduler));
      STACK_PUSH (STACK_FRAME_HEADER+1);
     Pushed();
      goto Internal_Apply;
#endif

    case TC_IN_PACKAGE:
     Will_Push(CONTINUATION_SIZE);
      Do_Nth_Then(RC_EXECUTE_IN_PACKAGE_CONTINUE,
                  IN_PACKAGE_ENVIRONMENT, Pushed());

    case TC_LAMBDA:             /* Close the procedure */
    case TC_LEXPR:
    /* Deliberately omitted: Eval_GC_Check(2); */
      Val = MAKE_POINTER_OBJECT (TC_PROCEDURE, Free);
      Free[PROCEDURE_LAMBDA_EXPR] = Fetch_Expression();
      Free[PROCEDURE_ENVIRONMENT] = Fetch_Env();
      Free += 2;
      break;

    case TC_MANIFEST_NM_VECTOR:
    case TC_MANIFEST_SPECIAL_NM_VECTOR:
      Eval_Error(ERR_EXECUTE_MANIFEST_VECTOR);

/* Interpret() continues on the next page */

/* Interpret(), continued */

    /*
      The argument to Will_Eventually_Push is determined by how much
      will be on the stack if we back out of the primitive.
     */

    case TC_PCOMB0:
     Will_Eventually_Push(CONTINUATION_SIZE + STACK_ENV_FIRST_ARG);
     Finished_Eventual_Pushing(CONTINUATION_SIZE + STACK_ENV_FIRST_ARG);
      Store_Expression (OBJECT_NEW_TYPE (TC_PRIMITIVE, (Fetch_Expression ())));
      goto Primitive_Internal_Apply;

    case TC_PCOMB1:
     Will_Eventually_Push(CONTINUATION_SIZE + STACK_ENV_FIRST_ARG + 1);
      Do_Nth_Then(RC_PCOMB1_APPLY, PCOMB1_ARG_SLOT, {});

    case TC_PCOMB2:
     Will_Eventually_Push(CONTINUATION_SIZE + STACK_ENV_FIRST_ARG + 2);
      Save_Env();
      Do_Nth_Then(RC_PCOMB2_DO_1, PCOMB2_ARG_2_SLOT, {});

    case TC_PCOMB3:
     Will_Eventually_Push(CONTINUATION_SIZE + STACK_ENV_FIRST_ARG + 3);
      Save_Env();
      Do_Nth_Then(RC_PCOMB3_DO_2, PCOMB3_ARG_3_SLOT, {});

    case TC_SCODE_QUOTE:
      Val = FAST_MEMORY_REF (Fetch_Expression(), SCODE_QUOTE_OBJECT);
      break;

    case TC_SEQUENCE_2:
     Will_Push(CONTINUATION_SIZE + 1);
      Save_Env();
      Do_Nth_Then(RC_SEQ_2_DO_2, SEQUENCE_1, Pushed());

    case TC_SEQUENCE_3:
     Will_Push(CONTINUATION_SIZE + 1);
      Save_Env();
      Do_Nth_Then(RC_SEQ_3_DO_2, SEQUENCE_1, Pushed());

    case TC_THE_ENVIRONMENT:
      Val = Fetch_Env(); break;

/* Interpret() continues on the next page */

/* Interpret(), continued */

    case TC_VARIABLE:
    {
      long temp;

#ifndef No_In_Line_Lookup

      fast SCHEME_OBJECT *cell;

      Set_Time_Zone(Zone_Lookup);
      cell = OBJECT_ADDRESS (Fetch_Expression());
      lookup(cell, Fetch_Env(), cell, repeat_variable_lookup);

lookup_end_restart:

      Val = MEMORY_FETCH (cell[0]);
      if (OBJECT_TYPE (Val) != TC_REFERENCE_TRAP)
      {
	Set_Time_Zone(Zone_Working);
	goto Pop_Return;
      }

      get_trap_kind(temp, Val);
      switch(temp)
      {
	case TRAP_DANGEROUS:
	case TRAP_UNBOUND_DANGEROUS:
	case TRAP_UNASSIGNED_DANGEROUS:
	case TRAP_FLUID_DANGEROUS:
	case TRAP_COMPILER_CACHED_DANGEROUS:
	  cell = OBJECT_ADDRESS (Fetch_Expression());
	  temp =
	    deep_lookup_end(deep_lookup(Fetch_Env(),
					cell[VARIABLE_SYMBOL],
					cell),
			    cell);
	  Import_Val();
	  if (temp != PRIM_DONE)
	    break;
	  Set_Time_Zone(Zone_Working);
	  goto Pop_Return;

	case TRAP_COMPILER_CACHED:
	  cell = MEMORY_LOC (FAST_MEMORY_REF (Val, TRAP_EXTRA),
				TRAP_EXTENSION_CELL);
	  goto lookup_end_restart;

	case TRAP_FLUID:
	  cell = lookup_fluid(Val);
	  goto lookup_end_restart;

/* Interpret() continues on the next page */

/* Interpret(), continued */

	case TRAP_UNBOUND:
	  temp = ERR_UNBOUND_VARIABLE;
	  break;

	case TRAP_UNASSIGNED:
	  temp = ERR_UNASSIGNED_VARIABLE;
	  break;

	default:
	  temp = ERR_ILLEGAL_REFERENCE_TRAP;
	  break;
      }

#else /* No_In_Line_Lookup */

      Set_Time_Zone(Zone_Lookup);
      temp = Lex_Ref(Fetch_Env(), Fetch_Expression());
      Import_Val();
      if (temp == PRIM_DONE)
	goto Pop_Return;

#endif /* No_In_Line_Lookup */

      /* Back out of the evaluation. */

      Set_Time_Zone(Zone_Working);

      if (temp == PRIM_INTERRUPT)
      {
	Prepare_Eval_Repeat();
	Interrupt(PENDING_INTERRUPTS());
      }

      Eval_Error(temp);
    }

    SITE_EXPRESSION_DISPATCH_HOOK()
  };

/* Interpret() continues on the next page */

/* Interpret(), continued */

/* Now restore the continuation saved during an earlier part
 * of the EVAL cycle and continue as directed.
 */

Pop_Return:
  Pop_Return_Ucode_Hook();
  Restore_Cont();
  if (Consistency_Check &&
      (OBJECT_TYPE (Fetch_Return()) != TC_RETURN_CODE))
  { STACK_PUSH (Val);			/* For possible stack trace */
    Save_Cont();
    Export_Registers();
    Microcode_Termination(TERM_BAD_STACK);
  }
  if (Eval_Debug)
  { Print_Return("Pop_Return, return code");
    Print_Expression(Val, "Pop_Return, value");
    printf ("\n");
  };

  /* Dispatch on the return code.  A BREAK here will cause
   * a "goto Pop_Return" to occur, since this is the most
   * common occurrence.
   */

  switch (OBJECT_DATUM (Fetch_Return()))
  {
    case RC_COMB_1_PROCEDURE:
      Restore_Env();
      STACK_PUSH (Val);                /* Arg. 1 */
      STACK_PUSH (SHARP_F);                /* Operator */
      STACK_PUSH (STACK_FRAME_HEADER + 1);
     Finished_Eventual_Pushing(CONTINUATION_SIZE);
      Do_Another_Then(RC_COMB_APPLY_FUNCTION, COMB_1_FN);

    case RC_COMB_2_FIRST_OPERAND:
      Restore_Env();
      STACK_PUSH (Val);
      Save_Env();
      Do_Another_Then(RC_COMB_2_PROCEDURE, COMB_2_ARG_1);

/* Interpret() continues on the next page */

/* Interpret(), continued */

    case RC_COMB_2_PROCEDURE:
      Restore_Env();
      STACK_PUSH (Val);                /* Arg 1, just calculated */
      STACK_PUSH (SHARP_F);		/* Function */
      STACK_PUSH (STACK_FRAME_HEADER + 2);
     Finished_Eventual_Pushing(CONTINUATION_SIZE);
      Do_Another_Then(RC_COMB_APPLY_FUNCTION, COMB_2_FN);

    case RC_COMB_APPLY_FUNCTION:
       End_Subproblem();
       goto Internal_Apply_Val;

    case RC_COMB_SAVE_VALUE:
      {	long Arg_Number;

        Restore_Env();
        Arg_Number = OBJECT_DATUM (STACK_REF(STACK_COMB_FINGER))-1;
        STACK_REF(STACK_COMB_FIRST_ARG+Arg_Number) = Val;
        STACK_REF(STACK_COMB_FINGER) =
          MAKE_OBJECT (TC_MANIFEST_NM_VECTOR, Arg_Number);
	/* DO NOT count on the type code being NMVector here, since
	   the stack parser may create them with #F here! */
        if (Arg_Number > 0)
        { Save_Env();
          Do_Another_Then(RC_COMB_SAVE_VALUE,
                          (COMB_ARG_1_SLOT - 1) + Arg_Number);
        }
	STACK_PUSH (FAST_MEMORY_REF (Fetch_Expression(), 0)); /* Frame Size */
        Do_Another_Then(RC_COMB_APPLY_FUNCTION, COMB_FN_SLOT);
      }

/* Interpret() continues on the next page */

/* Interpret(), continued */

#define define_compiler_restart(return_code, entry)			\
    case return_code:							\
      {									\
	extern long entry();						\
	compiled_code_restart();					\
	Export_Registers();						\
	Which_Way = entry();						\
	goto return_from_compiled_code;					\
      }

      define_compiler_restart (RC_COMP_INTERRUPT_RESTART,
			       comp_interrupt_restart)

      define_compiler_restart (RC_COMP_LOOKUP_APPLY_RESTART,
			       comp_lookup_apply_restart)

      define_compiler_restart (RC_COMP_REFERENCE_RESTART,
			       comp_reference_restart)

      define_compiler_restart (RC_COMP_ACCESS_RESTART,
			       comp_access_restart)

      define_compiler_restart (RC_COMP_UNASSIGNED_P_RESTART,
			       comp_unassigned_p_restart)

      define_compiler_restart (RC_COMP_UNBOUND_P_RESTART,
			       comp_unbound_p_restart)

      define_compiler_restart (RC_COMP_ASSIGNMENT_RESTART,
			       comp_assignment_restart)

      define_compiler_restart (RC_COMP_DEFINITION_RESTART,
			       comp_definition_restart)

      define_compiler_restart (RC_COMP_SAFE_REFERENCE_RESTART,
			       comp_safe_reference_restart)

      define_compiler_restart (RC_COMP_LOOKUP_TRAP_RESTART,
			       comp_lookup_trap_restart)

      define_compiler_restart (RC_COMP_ASSIGNMENT_TRAP_RESTART,
			       comp_assignment_trap_restart)

      define_compiler_restart (RC_COMP_OP_REF_TRAP_RESTART,
			       comp_op_lookup_trap_restart)

      define_compiler_restart (RC_COMP_CACHE_REF_APPLY_RESTART,
			       comp_cache_lookup_apply_restart)

      define_compiler_restart (RC_COMP_SAFE_REF_TRAP_RESTART,
			       comp_safe_lookup_trap_restart)

      define_compiler_restart (RC_COMP_UNASSIGNED_TRAP_RESTART,
			       comp_unassigned_p_trap_restart)

      define_compiler_restart (RC_COMP_LINK_CACHES_RESTART,
			       comp_link_caches_restart)

      define_compiler_restart (RC_COMP_ERROR_RESTART,
			       comp_error_restart)

    case RC_REENTER_COMPILED_CODE:
      compiled_code_restart();
      Export_Registers();
      Which_Way = return_to_compiled_code();
      goto return_from_compiled_code;

    case RC_CONDITIONAL_DECIDE:
      Pop_Return_Val_Check();
      End_Subproblem();
      Restore_Env();
      Reduces_To_Nth ((Val == SHARP_F) ? COND_ALTERNATIVE : COND_CONSEQUENT);

    case RC_DISJUNCTION_DECIDE:
      /* Return predicate if it isn't #F; else do ALTERNATIVE */
      Pop_Return_Val_Check();
      End_Subproblem();
      Restore_Env();
      if (Val != SHARP_F) goto Pop_Return;
      Reduces_To_Nth(OR_ALTERNATIVE);

    case RC_END_OF_COMPUTATION:
      /* Signals bottom of stack */
      Export_Registers();
      termination_end_of_computation ();

    case RC_EVAL_ERROR:
      /* Should be called RC_REDO_EVALUATION. */
      Store_Env(STACK_POP ());
      Reduces_To(Fetch_Expression());

    case RC_EXECUTE_ACCESS_FINISH:
    {
      long Result;
      SCHEME_OBJECT value;

      Pop_Return_Val_Check();
      value = Val;

      if (ENVIRONMENT_P (Val))
      { Result = Symbol_Lex_Ref(value,
				FAST_MEMORY_REF (Fetch_Expression(),
						ACCESS_NAME));
	Import_Val();
	if (Result == PRIM_DONE)
	{
	  End_Subproblem();
	  break;
	}
	if (Result != PRIM_INTERRUPT)
	{
	  Val = value;
	  Pop_Return_Error(Result);
	}
	Prepare_Pop_Return_Interrupt(RC_EXECUTE_ACCESS_FINISH, value);
	Interrupt(PENDING_INTERRUPTS());
      }
      Val = value;
      Pop_Return_Error(ERR_BAD_FRAME);
    }

/* Interpret() continues on the next page */

/* Interpret(), continued */

    case RC_EXECUTE_ASSIGNMENT_FINISH:
    {
      long temp;
      SCHEME_OBJECT value;
      Lock_Handle set_serializer;

#ifndef No_In_Line_Lookup

      SCHEME_OBJECT bogus_unassigned;
      fast SCHEME_OBJECT *cell;

      Set_Time_Zone(Zone_Lookup);
      Restore_Env();
      cell = OBJECT_ADDRESS (MEMORY_REF (Fetch_Expression(), ASSIGN_NAME));
      lookup(cell, Fetch_Env(), cell, repeat_assignment_lookup);

      value = Val;
      bogus_unassigned = Get_Fixed_Obj_Slot(Non_Object);
      if (value == bogus_unassigned)
	value = UNASSIGNED_OBJECT;

assignment_end_before_lock:

      setup_lock(set_serializer, cell);

assignment_end_after_lock:

      Val = *cell;

      if (OBJECT_TYPE (*cell) != TC_REFERENCE_TRAP)
      {
normal_assignment_done:
	*cell = value;
	remove_lock(set_serializer);
	Set_Time_Zone(Zone_Working);
	End_Subproblem();
	goto Pop_Return;
      }

/* Interpret() continues on the next page */

/* Interpret(), continued */

      get_trap_kind(temp, *cell);
      switch(temp)
      {
	case TRAP_DANGEROUS:
	case TRAP_UNBOUND_DANGEROUS:
	case TRAP_UNASSIGNED_DANGEROUS:
	case TRAP_FLUID_DANGEROUS:
	case TRAP_COMPILER_CACHED_DANGEROUS:
	  remove_lock(set_serializer);
	  cell = OBJECT_ADDRESS (MEMORY_REF (Fetch_Expression(), ASSIGN_NAME));
	  temp =
	    deep_assignment_end(deep_lookup(Fetch_Env(),
					    cell[VARIABLE_SYMBOL],
					    cell),
				cell,
				value,
				false);
external_assignment_return:
	  Import_Val();
	  if (temp != PRIM_DONE)
	    break;
	  Set_Time_Zone(Zone_Working);
	  End_Subproblem();
	  goto Pop_Return;

	case TRAP_COMPILER_CACHED:
	{
	  SCHEME_OBJECT extension, references;

	  extension = FAST_MEMORY_REF (Val, TRAP_EXTRA);
	  references = FAST_MEMORY_REF (extension, TRAP_EXTENSION_REFERENCES);

	  if ((FAST_MEMORY_REF (references, TRAP_REFERENCES_OPERATOR))
	      != SHARP_F)
	  {

	    /* There are uuo links.
	       wimp out and let deep_assignment_end handle it.
	     */

	    remove_lock(set_serializer);
	    temp = deep_assignment_end(cell,
				       fake_variable_object,
				       value,
				       false);
	    goto external_assignment_return;
	  }
	  cell = MEMORY_LOC (extension, TRAP_EXTENSION_CELL);
	  update_lock(set_serializer, cell);
	  goto assignment_end_after_lock;
	}

/* Interpret() continues on the next page */

/* Interpret(), continued */

	case TRAP_FLUID:
	  remove_lock(set_serializer);
	  cell = lookup_fluid(Val);
	  goto assignment_end_before_lock;

	case TRAP_UNBOUND:
	  remove_lock(set_serializer);
	  temp = ERR_UNBOUND_VARIABLE;
	  break;

	case TRAP_UNASSIGNED:
	  Val = bogus_unassigned;
	  goto normal_assignment_done;

	default:
	  remove_lock(set_serializer);
	  temp = ERR_ILLEGAL_REFERENCE_TRAP;
	  break;
      }

      if (value == UNASSIGNED_OBJECT)
	value = bogus_unassigned;

/* Interpret() continues on the next page */

/* Interpret(), continued */

#else /* No_In_Line_Lookup */

      value = Val;
      Set_Time_Zone(Zone_Lookup);
      Restore_Env();
      temp = Lex_Set(Fetch_Env(),
		     MEMORY_REF (Fetch_Expression(), ASSIGN_NAME),
		     value);
      Import_Val();
      if (temp == PRIM_DONE)
      {
	End_Subproblem();
	Set_Time_Zone(Zone_Working);
	break;
      }

#endif /* No_In_Line_Lookup */

      Set_Time_Zone(Zone_Working);
      Save_Env();
      if (temp != PRIM_INTERRUPT)
      {
	Val = value;
	Pop_Return_Error(temp);
      }

      Prepare_Pop_Return_Interrupt(RC_EXECUTE_ASSIGNMENT_FINISH,
				   value);
      Interrupt(PENDING_INTERRUPTS());
    }

/* Interpret() continues on the next page */

/* Interpret(), continued */

    case RC_EXECUTE_DEFINITION_FINISH:
      {
	SCHEME_OBJECT value;
        long result;

	value = Val;
        Restore_Env();
	Export_Registers();
        result = Local_Set(Fetch_Env(),
			   FAST_MEMORY_REF (Fetch_Expression(), DEFINE_NAME),
			   Val);
        Import_Registers();
        if (result == PRIM_DONE)
        {
	  End_Subproblem();
          break;
	}
	Save_Env();
	if (result == PRIM_INTERRUPT)
	{
	  Prepare_Pop_Return_Interrupt(RC_EXECUTE_DEFINITION_FINISH,
				       value);
	  Interrupt(PENDING_INTERRUPTS());
	}
	Val = value;
        Pop_Return_Error(result);
      }

    case RC_EXECUTE_IN_PACKAGE_CONTINUE:
      Pop_Return_Val_Check();
      if (ENVIRONMENT_P (Val))
      {
	End_Subproblem();
        Store_Env(Val);
        Reduces_To_Nth(IN_PACKAGE_EXPRESSION);
      }
      Pop_Return_Error(ERR_BAD_FRAME);

#ifdef COMPILE_FUTURES
    case RC_FINISH_GLOBAL_INT:
      Export_Registers();
      Val = Global_Int_Part_2(Fetch_Expression(), Val);
      Import_Registers_Except_Val();
      break;
#endif

    case RC_HALT:
      Export_Registers();
      Microcode_Termination(TERM_TERM_HANDLER);

    case RC_HARDWARE_TRAP:
    {
      /* This just reinvokes the handler */

      SCHEME_OBJECT info, handler;
      info = (STACK_REF (0));

      Save_Cont();
      if ((! (Valid_Fixed_Obj_Vector())) ||
	  ((handler = (Get_Fixed_Obj_Slot(Trap_Handler))) == SHARP_F))
      {
	fprintf(stderr, "There is no trap handler for recovery!\n");
	termination_trap ();
	/*NOTREACHED*/
      }
     Will_Push(STACK_ENV_EXTRA_SLOTS + 2);
      STACK_PUSH (info);
      STACK_PUSH (handler);
      STACK_PUSH (STACK_FRAME_HEADER + 1);
     Pushed();
      goto Internal_Apply;
    }

/* Internal_Apply, the core of the application mechanism.

   Branch here to perform a function application.

   At this point the top of the stack contains an application frame
   which consists of the following elements (see sdata.h):
   - A header specifying the frame length.
   - A procedure.
   - The actual (evaluated) arguments.

   No registers (except the stack pointer) are meaning full at this point.
   Before interrupts or errors are processed, some registers are cleared
   to avoid holding onto garbage if a garbage collection occurs.
*/

#define Prepare_Apply_Interrupt()					\
{									\
  Store_Expression (SHARP_F);						\
  Prepare_Pop_Return_Interrupt (RC_INTERNAL_APPLY_VAL,			\
				(STACK_REF (STACK_ENV_FUNCTION)));	\
}

#define Apply_Error(N)							\
{									\
  Store_Expression (SHARP_F);						\
  Store_Return (RC_INTERNAL_APPLY_VAL);					\
  Val = (STACK_REF (STACK_ENV_FUNCTION));				\
  Pop_Return_Error (N);							\
}

/* Interpret() continues on the next page */

/* Interpret(), continued */

    case RC_INTERNAL_APPLY_VAL:
Internal_Apply_Val:

       STACK_REF (STACK_ENV_FUNCTION) = Val;

    case RC_INTERNAL_APPLY:
Internal_Apply:

      if (Microcode_Does_Stepping &&
	  Trapping &&
	  ((Fetch_Apply_Trapper ()) != SHARP_F))
      {
	long Count;

	Count = (OBJECT_DATUM (STACK_REF (STACK_ENV_HEADER)));
        (* (STACK_LOC (0))) = (Fetch_Apply_Trapper ());
        STACK_PUSH (STACK_FRAME_HEADER + Count);
        Stop_Trapping ();
      }

Apply_Non_Trapping:

      if ((PENDING_INTERRUPTS()) != 0)
      {
	long Interrupts;

	Interrupts = (PENDING_INTERRUPTS());
	Prepare_Apply_Interrupt ();
	Interrupt(Interrupts);
      }

Perform_Application:

      Apply_Ucode_Hook();

      {
        fast SCHEME_OBJECT Function;

	Apply_Future_Check(Function, STACK_REF(STACK_ENV_FUNCTION));

        switch(OBJECT_TYPE (Function))
        {

	  case TC_ENTITY:
	  {
	    fast long nargs;

	    /* Will_Pushed ommited since frame must be contiguous.
	       combination code must ensure one more slot.
	     */

	    /* This code assumes that adding 1 to nargs takes care
	       of everything, including type code, etc.
	     */

	    nargs = (STACK_POP ());
	    STACK_PUSH (FAST_MEMORY_REF (Function, ENTITY_OPERATOR));
	    STACK_PUSH (nargs + 1);
	    /* This must be done to prevent an infinite push loop by
	       an entity whose handler is the entity itself or some
	       other such loop.  Of course, it will die if stack overflow
	       interrupts are disabled.
	       This will not work in fscheme!  It has to be thought out
	       carefully.
	     */
	    Stack_Check(Stack_Pointer);
	    goto Internal_Apply;
	  }

/* Interpret() continues on the next page */

/* Interpret(), continued */

	  case TC_PROCEDURE:
	  {
	    fast long nargs;

            nargs = OBJECT_DATUM (STACK_POP ());
	    Function = FAST_MEMORY_REF (Function, PROCEDURE_LAMBDA_EXPR);

	    {
	      fast SCHEME_OBJECT formals;

	      Apply_Future_Check(formals,
				 FAST_MEMORY_REF (Function, LAMBDA_FORMALS));

	      if ((nargs != VECTOR_LENGTH (formals)) &&
		  ((OBJECT_TYPE (Function) != TC_LEXPR) ||
		  (nargs < VECTOR_LENGTH (formals))))
	      {
		STACK_PUSH (STACK_FRAME_HEADER + nargs - 1);
		Apply_Error(ERR_WRONG_NUMBER_OF_ARGUMENTS);
	      }
	    }

	    if (Eval_Debug)
	    {
	      Print_Expression(LONG_TO_UNSIGNED_FIXNUM(nargs),
			       "APPLY: Number of arguments");
	    }

            if (GC_Check(nargs + 1))
            {
	      STACK_PUSH (STACK_FRAME_HEADER + nargs - 1);
              Prepare_Apply_Interrupt ();
              Immediate_GC(nargs + 1);
            }

	    {
	      fast SCHEME_OBJECT *scan;

	      scan = Free;
	      Store_Env(MAKE_POINTER_OBJECT (TC_ENVIRONMENT, scan));
	      *scan++ = MAKE_OBJECT (TC_MANIFEST_VECTOR, nargs);
	      while(--nargs >= 0)
		*scan++ = (STACK_POP ());
	      Free = scan;
	      Reduces_To(FAST_MEMORY_REF (Function, LAMBDA_SCODE));
	    }
          }

/* Interpret() continues on the next page */

/* Interpret(), continued */

          case TC_CONTROL_POINT:
	  {
            if (OBJECT_DATUM (STACK_REF (STACK_ENV_HEADER)) !=
                STACK_ENV_FIRST_ARG)
	    {
              Apply_Error(ERR_WRONG_NUMBER_OF_ARGUMENTS);
	    }
            Val = (STACK_REF (STACK_ENV_FIRST_ARG));
            Our_Throw(false, Function);
	    Apply_Stacklet_Backout();
	    Our_Throw_Part_2();
            goto Pop_Return;
	  }

/* Interpret() continues on the next page */

/* Interpret(), continued */

	  /*
	     After checking the number of arguments, remove the
	     frame header since primitives do not expect it.

	     NOTE: This code must match the application code which
	     follows Primitive_Internal_Apply.
	   */

          case TC_PRIMITIVE:
          {
	    fast long nargs;

	    if (!IMPLEMENTED_PRIMITIVE_P(Function))
	    {
	      Apply_Error(ERR_UNIMPLEMENTED_PRIMITIVE);
	    }

	    /* Note that the first test below will fail for lexpr primitives. */

	    nargs = ((OBJECT_DATUM (STACK_REF(STACK_ENV_HEADER))) -
		     (STACK_ENV_FIRST_ARG - 1));
            if (nargs != PRIMITIVE_ARITY(Function))
	    {
	      if (PRIMITIVE_ARITY(Function) != LEXPR_PRIMITIVE_ARITY)
	      {
		Apply_Error(ERR_WRONG_NUMBER_OF_ARGUMENTS);
	      }
	      Regs[REGBLOCK_LEXPR_ACTUALS] = ((SCHEME_OBJECT) nargs);
	    }

            Stack_Pointer = (STACK_LOC (STACK_ENV_FIRST_ARG));
            Store_Expression (Function);
	    EXPORT_REGS_BEFORE_PRIMITIVE ();
	    PRIMITIVE_APPLY (Val, Function);
	    IMPORT_REGS_AFTER_PRIMITIVE ();
	    POP_PRIMITIVE_FRAME (nargs);
	    if (Must_Report_References())
	    {
	      Store_Expression(Val);
	      Store_Return(RC_RESTORE_VALUE);
	      Save_Cont();
	      Call_Future_Logging();
	    }
	    goto Pop_Return;
	  }

/* Interpret() continues on the next page */

/* Interpret(), continued */

          case TC_EXTENDED_PROCEDURE:
          {
	    SCHEME_OBJECT lambda;
            long nargs, nparams, formals, params, auxes,
                 rest_flag, size;

	    fast long i;
	    fast SCHEME_OBJECT *scan;

            nargs = OBJECT_DATUM (STACK_POP ()) - STACK_FRAME_HEADER;

	    if (Eval_Debug)
	    {
	      Print_Expression(LONG_TO_UNSIGNED_FIXNUM(nargs+STACK_FRAME_HEADER),
			       "APPLY: Number of arguments");
	    }

            lambda = FAST_MEMORY_REF (Function, PROCEDURE_LAMBDA_EXPR);
	    Apply_Future_Check(Function,
			       FAST_MEMORY_REF (lambda, ELAMBDA_NAMES));
            nparams = VECTOR_LENGTH (Function) - 1;

	    Apply_Future_Check(Function, Get_Count_Elambda(lambda));
            formals = Elambda_Formals_Count(Function);
            params = Elambda_Opts_Count(Function) + formals;
            rest_flag = Elambda_Rest_Flag(Function);
            auxes = nparams - (params + rest_flag);

            if ((nargs < formals) || (!rest_flag && (nargs > params)))
            {
	      STACK_PUSH (STACK_FRAME_HEADER + nargs);
              Apply_Error(ERR_WRONG_NUMBER_OF_ARGUMENTS);
            }

	    /* size includes the procedure slot, but not the header. */
            size = params + rest_flag + auxes + 1;
            if (GC_Check(size + 1 + ((nargs > params) ?
				     (2 * (nargs - params)) :
				     0)))
            {
	      STACK_PUSH (STACK_FRAME_HEADER + nargs);
              Prepare_Apply_Interrupt ();
              Immediate_GC(size + 1 + ((nargs > params) ?
				       (2 * (nargs - params)) :
				       0));
            }

/* Interpret() continues on the next page */

/* Interpret(), continued */

	    scan = Free;
            Store_Env(MAKE_POINTER_OBJECT (TC_ENVIRONMENT, scan));
	    *scan++ = MAKE_OBJECT (TC_MANIFEST_VECTOR, size);

	    if (nargs <= params)
	    {
	      for (i = (nargs + 1); --i >= 0; )
		*scan++ = (STACK_POP ());
	      for (i = (params - nargs); --i >= 0; )
		*scan++ = UNASSIGNED_OBJECT;
	      if (rest_flag)
		*scan++ = EMPTY_LIST;
	      for (i = auxes; --i >= 0; )
		*scan++ = UNASSIGNED_OBJECT;
	    }
	    else
	    {
	      /* rest_flag must be true. */
	      SCHEME_OBJECT list;

	      list = MAKE_POINTER_OBJECT (TC_LIST, (scan + size));
	      for (i = (params + 1); --i >= 0; )
		*scan++ = (STACK_POP ());
	      *scan++ = list;
	      for (i = auxes; --i >= 0; )
		*scan++ = UNASSIGNED_OBJECT;
	      /* Now scan == OBJECT_ADDRESS (list) */
	      for (i = (nargs - params); --i >= 0; )
	      {
		*scan++ = (STACK_POP ());
		*scan = MAKE_POINTER_OBJECT (TC_LIST, (scan + 1));
		scan += 1;
	      }
	      scan[-1] = EMPTY_LIST;
	    }

	    Free = scan;
            Reduces_To(Get_Body_Elambda(lambda));
          }

/* Interpret() continues on the next page */

/* Interpret(), continued */

          case TC_COMPILED_ENTRY:
	  {
	    apply_compiled_setup (STACK_ENV_EXTRA_SLOTS +
				  (OBJECT_DATUM (STACK_REF (STACK_ENV_HEADER))));
	    Export_Registers ();
	    Which_Way = apply_compiled_procedure();

return_from_compiled_code:
	    Import_Registers ();
            switch (Which_Way)
            {
	    case PRIM_DONE:
	    {
	      compiled_code_done ();
	      goto Pop_Return;
	    }

	    case PRIM_APPLY:
	    {
	      compiler_apply_procedure
		(STACK_ENV_EXTRA_SLOTS +
		 OBJECT_DATUM (STACK_REF (STACK_ENV_HEADER)));
	      goto Internal_Apply;
	    }

	    case PRIM_INTERRUPT:
	    {
	      compiled_error_backout ();
	      Save_Cont ();
	      Interrupt (PENDING_INTERRUPTS ());
	    }

	    case PRIM_APPLY_INTERRUPT:
	    {
	      apply_compiled_backout ();
	      Prepare_Apply_Interrupt ();
	      Interrupt (PENDING_INTERRUPTS ());
	    }

	    case ERR_INAPPLICABLE_OBJECT:
	    /* This error code means that apply_compiled_procedure
	       was called on an object which is not a compiled procedure,
	       or it was called in a system without compiler support.

	       Fall through...
	     */

	    case ERR_WRONG_NUMBER_OF_ARGUMENTS:
	    {
	      apply_compiled_backout ();
	      Apply_Error (Which_Way);
	    }

	    case ERR_EXECUTE_MANIFEST_VECTOR:
	    {
	      /* This error code means that enter_compiled_expression
		 was called in a system without compiler support.
		 This is a kludge!
	       */

	      execute_compiled_backout ();
	      Val =
		(OBJECT_NEW_TYPE (TC_COMPILED_ENTRY, (Fetch_Expression ())));
	      Pop_Return_Error (Which_Way);
	    }

	    case ERR_INAPPLICABLE_CONTINUATION:
	    {
	      /* This error code means that return_to_compiled_code
		 saw a non-continuation on the stack, or was called
		 in a system without compiler support.
	       */

	      Store_Expression (SHARP_F);
	      Store_Return (RC_REENTER_COMPILED_CODE);
	      Pop_Return_Error (Which_Way);
	    }

	    default:
	      compiled_error_backout ();
	      Pop_Return_Error (Which_Way);
            }
          }

          default:
            Apply_Error (ERR_INAPPLICABLE_OBJECT);
        }       /* End of switch in RC_INTERNAL_APPLY */
      }         /* End of RC_INTERNAL_APPLY case */

/* Interpret() continues on the next page */

/* Interpret(), continued */

    case RC_MOVE_TO_ADJACENT_POINT:
    /* Expression contains the space in which we are moving */
    {
      long From_Count;
      SCHEME_OBJECT Thunk, New_Location;

      From_Count =
	(UNSIGNED_FIXNUM_TO_LONG (STACK_REF (TRANSLATE_FROM_DISTANCE)));
      if (From_Count != 0)
      { SCHEME_OBJECT Current = STACK_REF(TRANSLATE_FROM_POINT);
	STACK_REF(TRANSLATE_FROM_DISTANCE) =
	  (LONG_TO_UNSIGNED_FIXNUM (From_Count - 1));
	Thunk = FAST_MEMORY_REF (Current, STATE_POINT_AFTER_THUNK);
	New_Location = FAST_MEMORY_REF (Current, STATE_POINT_NEARER_POINT);
	STACK_REF(TRANSLATE_FROM_POINT) = New_Location;
	if ((From_Count == 1) &&
	    (STACK_REF(TRANSLATE_TO_DISTANCE) == LONG_TO_UNSIGNED_FIXNUM(0)))
	  Stack_Pointer = (STACK_LOC (4));
	else Save_Cont();
      }
      else
      {
	long To_Count;
	fast SCHEME_OBJECT To_Location;
	fast long i;

	To_Count =
	  (UNSIGNED_FIXNUM_TO_LONG (STACK_REF (TRANSLATE_TO_DISTANCE)) -  1);
	To_Location = STACK_REF(TRANSLATE_TO_POINT);
	for (i = 0; i < To_Count; i++)
	{
	  To_Location =
	    (FAST_MEMORY_REF (To_Location, STATE_POINT_NEARER_POINT));
	}
	Thunk = FAST_MEMORY_REF (To_Location, STATE_POINT_BEFORE_THUNK);
	New_Location = To_Location;
	STACK_REF(TRANSLATE_TO_DISTANCE) = LONG_TO_UNSIGNED_FIXNUM(To_Count);
	if (To_Count == 0)
	{
	  Stack_Pointer = (STACK_LOC (4));
	}
	else
	{
	  Save_Cont();
	}
      }
      if ((Fetch_Expression ()) != SHARP_F)
      {
        MEMORY_SET
	  ((Fetch_Expression ()), STATE_SPACE_NEAREST_POINT, New_Location);
      }
      else
      {
	Current_State_Point = New_Location;
      }
     Will_Push(2);
      STACK_PUSH (Thunk);
      STACK_PUSH (STACK_FRAME_HEADER);
     Pushed();
      goto Internal_Apply;
    }

/* Interpret() continues on the next page */

/* Interpret(), continued */

    case RC_INVOKE_STACK_THREAD:
      /* Used for WITH_THREADED_STACK primitive */
     Will_Push(3);
      STACK_PUSH (Val);        /* Value calculated by thunk */
      STACK_PUSH (Fetch_Expression());
      STACK_PUSH (STACK_FRAME_HEADER+1);
     Pushed();
      goto Internal_Apply;

    case RC_JOIN_STACKLETS:
      Our_Throw(true, Fetch_Expression());
      Join_Stacklet_Backout();
      Our_Throw_Part_2();
      break;

    case RC_NORMAL_GC_DONE:
      Val = Fetch_Expression();
      if (GC_Space_Needed < 0)
      {
	/* Paranoia */

	GC_Space_Needed = 0;
      }
      if (GC_Check(GC_Space_Needed))
	termination_gc_out_of_space ();
      GC_Space_Needed = 0;
      EXIT_CRITICAL_SECTION ({ Save_Cont(); Export_Registers(); });
      End_GC_Hook();
      break;

    case RC_PCOMB1_APPLY:
      End_Subproblem();
      STACK_PUSH (Val);		/* Argument value */
     Finished_Eventual_Pushing(CONTINUATION_SIZE + STACK_ENV_FIRST_ARG);
      Store_Expression(FAST_MEMORY_REF (Fetch_Expression(), PCOMB1_FN_SLOT));

Primitive_Internal_Apply:
      if (Microcode_Does_Stepping &&
	  Trapping &&
	  ((Fetch_Apply_Trapper ()) != SHARP_F))
      {
	/* Does this work in the stacklet case?
	   We may have a non-contiguous frame. -- Jinx
	 */
       Will_Push(3);
        STACK_PUSH (Fetch_Expression());
        STACK_PUSH (Fetch_Apply_Trapper());
        STACK_PUSH (STACK_FRAME_HEADER + 1 +
	     PRIMITIVE_N_PARAMETERS(Fetch_Expression()));
       Pushed();
        Stop_Trapping();
	goto Apply_Non_Trapping;
      }

      /* NOTE: This code must match the code in the TC_PRIMITIVE
	 case of Internal_Apply.
	 This code is simpler because:
	 1) The arity was checked at syntax time.
	 2) We don't have to deal with "lexpr" primitives.
	 3) We don't need to worry about unimplemented primitives because
	    unimplemented primitives will cause an error at invocation.
       */

      {
	fast SCHEME_OBJECT primitive = (Fetch_Expression ());
	EXPORT_REGS_BEFORE_PRIMITIVE ();
	PRIMITIVE_APPLY (Val, primitive);
	IMPORT_REGS_AFTER_PRIMITIVE ();
	POP_PRIMITIVE_FRAME (PRIMITIVE_ARITY (primitive));
	if (Must_Report_References ())
	  {
	    Store_Expression (Val);
	    Store_Return (RC_RESTORE_VALUE);
	    Save_Cont ();
	    Call_Future_Logging ();
	  }
	break;
      }

    case RC_PCOMB2_APPLY:
      End_Subproblem();
      STACK_PUSH (Val);		/* Value of arg. 1 */
     Finished_Eventual_Pushing(CONTINUATION_SIZE + STACK_ENV_FIRST_ARG);
      Store_Expression(FAST_MEMORY_REF (Fetch_Expression(), PCOMB2_FN_SLOT));
      goto Primitive_Internal_Apply;

    case RC_PCOMB2_DO_1:
      Restore_Env();
      STACK_PUSH (Val);		/* Save value of arg. 2 */
      Do_Another_Then(RC_PCOMB2_APPLY, PCOMB2_ARG_1_SLOT);

    case RC_PCOMB3_APPLY:
      End_Subproblem();
      STACK_PUSH (Val);		/* Save value of arg. 1 */
     Finished_Eventual_Pushing(CONTINUATION_SIZE + STACK_ENV_FIRST_ARG);
      Store_Expression(FAST_MEMORY_REF (Fetch_Expression(), PCOMB3_FN_SLOT));
      goto Primitive_Internal_Apply;

/* Interpret() continues on the next page */

/* Interpret(), continued */

    case RC_PCOMB3_DO_1:
    {
      SCHEME_OBJECT Temp;

      Temp = (STACK_POP ());		/* Value of arg. 3 */
      Restore_Env();
      STACK_PUSH (Temp);		/* Save arg. 3 again */
      STACK_PUSH (Val);		/* Save arg. 2 */
      Do_Another_Then(RC_PCOMB3_APPLY, PCOMB3_ARG_1_SLOT);
    }

    case RC_PCOMB3_DO_2:
      Restore_Then_Save_Env();
      STACK_PUSH (Val);		/* Save value of arg. 3 */
      Do_Another_Then(RC_PCOMB3_DO_1, PCOMB3_ARG_2_SLOT);

    case RC_POP_RETURN_ERROR:
    case RC_RESTORE_VALUE:
      Val = Fetch_Expression();
      break;

    case RC_PRIMITIVE_CONTINUE:
      Export_Registers ();
      Val = (continue_primitive ());
      Import_Registers ();
      break;

/* Interpret() continues on the next page */

/* Interpret(), continued */

    case RC_PURIFY_GC_1:
    {
      SCHEME_OBJECT GC_Daemon_Proc, Result;

      RENAME_CRITICAL_SECTION ("purify pass 2");
      Export_Registers();
      Result = Purify_Pass_2(Fetch_Expression());
      Import_Registers();
      if (Result == SHARP_F)
	{
	  /* The object does not fit in Constant space.
	     There is no need to run the daemons, and we should let
	     the runtime system know what happened.  */
	  RESULT_OF_PURIFY (SHARP_F);
	  EXIT_CRITICAL_SECTION ({ Export_Registers(); });
	  break;
	}
      GC_Daemon_Proc = Get_Fixed_Obj_Slot(GC_Daemon);
      if (GC_Daemon_Proc == SHARP_F)
	{
	  RESULT_OF_PURIFY (SHARP_T);
	  EXIT_CRITICAL_SECTION ({ Export_Registers(); });
	  break;
	}
      RENAME_CRITICAL_SECTION( "purify daemon 2");
      Store_Expression(SHARP_F);
      Store_Return(RC_PURIFY_GC_2);
      Save_Cont();
     Will_Push(2);
      STACK_PUSH (GC_Daemon_Proc);
      STACK_PUSH (STACK_FRAME_HEADER);
     Pushed();
      goto Internal_Apply;
    }

    case RC_PURIFY_GC_2:
      RESULT_OF_PURIFY (SHARP_T);
      EXIT_CRITICAL_SECTION ({ Export_Registers(); });
      break;

    case RC_REPEAT_DISPATCH:
      Which_Way = (FIXNUM_TO_LONG (Fetch_Expression ()));
      Restore_Env();
      Val = (STACK_POP ());
      Restore_Cont();
      goto Repeat_Dispatch;

/* Interpret() continues on the next page */

/* Interpret(), continued */

/* The following two return codes are both used to restore
   a saved history object.  The difference is that the first
   does not copy the history object while the second does.
   In both cases, the Expression register contains the history
   object and the next item to be popped off the stack contains
   the offset back to the previous restore history return code.

   ASSUMPTION: History objects are never created using futures.
*/

    case RC_RESTORE_DONT_COPY_HISTORY:
    {
      SCHEME_OBJECT Stacklet;

      Prev_Restore_History_Offset = OBJECT_DATUM (STACK_POP ());
      Stacklet = (STACK_POP ());
      History = OBJECT_ADDRESS (Fetch_Expression());
      if (Prev_Restore_History_Offset == 0)
      {
	Prev_Restore_History_Stacklet = NULL;
      }
      else if (Stacklet == SHARP_F)
      {
        Prev_Restore_History_Stacklet = NULL;
      }
      else
      {
	Prev_Restore_History_Stacklet = OBJECT_ADDRESS (Stacklet);
      }
      break;
    }

/* Interpret() continues on the next page */

/* Interpret(), continued */

    case RC_RESTORE_HISTORY:
    {
      SCHEME_OBJECT Stacklet;

      Export_Registers();
      if (! Restore_History(Fetch_Expression()))
      {
	Import_Registers();
        Save_Cont();
       Will_Push(CONTINUATION_SIZE);
        Store_Expression(Val);
        Store_Return(RC_RESTORE_VALUE);
        Save_Cont();
       Pushed();
        Immediate_GC((Free > MemTop) ? 0 : ((MemTop-Free)+1));
      }
      Import_Registers();
      Prev_Restore_History_Offset = OBJECT_DATUM (STACK_POP ());
      Stacklet = (STACK_POP ());
      if (Prev_Restore_History_Offset == 0)
	Prev_Restore_History_Stacklet = NULL;
      else
      { if (Stacklet == SHARP_F)
        { Prev_Restore_History_Stacklet = NULL;
	  Get_End_Of_Stacklet()[-Prev_Restore_History_Offset] =
            MAKE_OBJECT (TC_RETURN_CODE, RC_RESTORE_HISTORY);
        }
        else
	{ Prev_Restore_History_Stacklet = OBJECT_ADDRESS (Stacklet);
	  Prev_Restore_History_Stacklet[-Prev_Restore_History_Offset] =
            MAKE_OBJECT (TC_RETURN_CODE, RC_RESTORE_HISTORY);
        }
      }
      break;
    }

    case RC_RESTORE_FLUIDS:
      Fluid_Bindings = Fetch_Expression();
      /* Why is this here? -- Jinx */
      COMPILER_SETUP_INTERRUPT();
      break;

    case RC_RESTORE_INT_MASK:
      SET_INTERRUPT_MASK (UNSIGNED_FIXNUM_TO_LONG (Fetch_Expression()));
      break;

/* Interpret() continues on the next page */

/* Interpret(), continued */

    case RC_RESTORE_TO_STATE_POINT:
    { SCHEME_OBJECT Where_To_Go = Fetch_Expression();
     Will_Push(CONTINUATION_SIZE);
      /* Restore the contents of Val after moving to point */
      Store_Expression(Val);
      Store_Return(RC_RESTORE_VALUE);
      Save_Cont();
     Pushed();
      Export_Registers();
      Translate_To_Point(Where_To_Go);
      break;			/* We never get here.... */
    }

    case RC_RETURN_TRAP_POINT:
      Store_Return(Old_Return_Code);
     Will_Push(CONTINUATION_SIZE+3);
      Save_Cont();
      Return_Hook_Address = NULL;
      Stop_Trapping();
      STACK_PUSH (Val);
      STACK_PUSH (Fetch_Return_Trapper());
      STACK_PUSH (STACK_FRAME_HEADER+1);
     Pushed();
      goto Apply_Non_Trapping;

    case RC_SEQ_2_DO_2:
      End_Subproblem();
      Restore_Env();
      Reduces_To_Nth(SEQUENCE_2);

    case RC_SEQ_3_DO_2:
      Restore_Then_Save_Env();
      Do_Another_Then(RC_SEQ_3_DO_3, SEQUENCE_2);

    case RC_SEQ_3_DO_3:
      End_Subproblem();
      Restore_Env();
      Reduces_To_Nth(SEQUENCE_3);

/* Interpret() continues on the next page */

/* Interpret(), continued */

    case RC_SNAP_NEED_THUNK:
      MEMORY_SET (Fetch_Expression(), THUNK_SNAPPED, SHARP_T);
      MEMORY_SET (Fetch_Expression(), THUNK_VALUE, Val);
      break;

    case RC_AFTER_MEMORY_UPDATE:
    case RC_BAD_INTERRUPT_CONTINUE:
    case RC_COMPLETE_GC_DONE:
    case RC_RESTARTABLE_EXIT:
    case RC_RESTART_EXECUTION:
    case RC_RESTORE_CONTINUATION:
    case RC_RESTORE_STEPPER:
    case RC_POP_FROM_COMPILED_CODE:
      Pop_Return_Error (ERR_INAPPLICABLE_CONTINUATION);

    SITE_RETURN_DISPATCH_HOOK()

    default:
      Pop_Return_Error (ERR_INAPPLICABLE_CONTINUATION);
  };
  goto Pop_Return;
}

/* -*-C-*-

$Id: dostrap.c,v 1.3 1992/11/23 04:01:58 gjr Exp $

Copyright (c) 1992 Massachusetts Institute of Technology

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

#include "scheme.h"
#include "os.h"
#include "msdos.h"
#include "dostrap.h"
#include "dosexcp.h"

extern void EXFUN (DOS_initialize_trap_recovery, (void));
CONST char * EXFUN (find_trap_name, (int trapno));
extern PTR initial_C_stack_pointer;

static enum trap_state trap_state;
static enum trap_state user_trap_state;

static enum trap_state saved_trap_state;
static int saved_trapno;
static SIGINFO_T saved_info;
static struct FULL_SIGCONTEXT * saved_scp;

static unsigned short
  initial_C_ss = 0,
  initial_C_ds = 0,
  initial_C_cs = 0;

static void EXFUN (initialize_dos_trap_codes, (void));
static void EXFUN
  (continue_from_trap,
   (int trapno, SIGINFO_T info, struct FULL_SIGCONTEXT * scp));

void
DEFUN_VOID (DOS_initialize_trap_recovery)
{
  extern unsigned short getSS (void);

  initial_C_ss = (getSS ());
  initial_C_ds = (getDS ());
  initial_C_cs = (getCS ());
  trap_state = trap_state_recover;
  user_trap_state = trap_state_recover;
  initialize_dos_trap_codes ();
}

enum trap_state
DEFUN (OS_set_trap_state, (state), enum trap_state state)
{
  enum trap_state old_trap_state = user_trap_state;
  user_trap_state = state;
  trap_state = state;
  return (old_trap_state);
}

static void
DEFUN_VOID (trap_normal_termination)
{
  trap_state = trap_state_exitting_soft;
  termination_trap ();
}

static void
DEFUN_VOID (trap_immediate_termination)
{
  trap_state = trap_state_exitting_hard;
  OS_restore_external_state ();
  exit (1);
}

static void
DEFUN_VOID (trap_recover)
{
  if (WITHIN_CRITICAL_SECTION_P ())
    {
      CLEAR_CRITICAL_SECTION_HOOK ();
      EXIT_CRITICAL_SECTION ({});
    }
  reset_interruptable_extent ();
  continue_from_trap (saved_trapno, saved_info, saved_scp);
}

void
DEFUN (trap_handler, (message, trapno, info, scp),
       CONST char * message AND
       int trapno AND
       SIGINFO_T info AND
       struct FULL_SIGCONTEXT * scp)
{
  int code = ((SIGINFO_VALID_P (info)) ? (SIGINFO_CODE (info)) : 0);
  Boolean constant_space_broken = (!(CONSTANT_SPACE_SEALED ()));
  enum trap_state old_trap_state = trap_state;

  if (old_trap_state == trap_state_exitting_hard)
  {
    _exit (1);
  }
  else if (old_trap_state == trap_state_exitting_soft)
  {
    trap_immediate_termination ();
  }
  trap_state = trap_state_trapped;
  if (WITHIN_CRITICAL_SECTION_P ())
  {
    fprintf (stdout,
	     "\n>> A %s has occurred within critical section \"%s\".\n",
	     message, (CRITICAL_SECTION_NAME ()));
    fprintf (stdout, ">> [exception %d (%s), code %d = 0x%x]\n",
	     trapno, (find_trap_name (trapno)), code, code);
  }
  else if (constant_space_broken || (old_trap_state != trap_state_recover))
  {
    fprintf (stdout, "\n>> A %s (%d) has occurred.\n", message, trapno);
    fprintf (stdout, ">> [exception %d (%s), code %d = 0x%x]\n",
	     trapno, (find_trap_name (trapno)), code, code);
  }
  if (constant_space_broken)
  {
    fputs (">> Constant space has been overwritten.\n", stdout);
    fputs (">> Probably a runaway recursion has overflowed the stack.\n",
	   stdout);
  }
  fflush (stdout);

  switch (old_trap_state)
  {
  case trap_state_trapped:
    if ((saved_trap_state == trap_state_recover) ||
	(saved_trap_state == trap_state_query))
    {
      fputs (">> The trap occurred while processing an earlier trap.\n",
	     stdout);
      fprintf (stdout,
	       ">> [The earlier trap raised exception %d (%s), code %d.]\n",
	       saved_trapno,
	       (find_trap_name (saved_trapno)),
	       ((SIGINFO_VALID_P (saved_info))
		? (SIGINFO_CODE (saved_info))
		: 0));
      fputs (((WITHIN_CRITICAL_SECTION_P ())
	      ? ">> Successful recovery is extremely unlikely.\n"
	      : ">> Successful recovery is unlikely.\n"),
	     stdout);
      break;
    }
    else
      trap_immediate_termination ();
  case trap_state_recover:
    if ((WITHIN_CRITICAL_SECTION_P ()) || constant_space_broken)
    {
      fputs (">> Successful recovery is unlikely.\n", stdout);
      break;
    }
    else
    {
      saved_trap_state = old_trap_state;
      saved_trapno = trapno;
      saved_info = info;
      saved_scp = scp;
      trap_recover ();
    }
  case trap_state_exit:
    termination_trap ();
  }

  fflush (stdout);
  saved_trap_state = old_trap_state;
  saved_trapno = trapno;
  saved_info = info;
  saved_scp = scp;
    
  while (1)
  {
    char option;
    static CONST char * trap_query_choices[] =
    {
      "I = terminate immediately",
      "N = terminate normally",
      "R = attempt recovery",
      "Q = terminate normally",
      0
      };
    option = (userio_choose_option
	      ("Choose one of the following actions:",
	       "Action -> ",
	       trap_query_choices));
    switch (option)
    {
      case 'I':
        trap_immediate_termination ();
      case '\0':
        /* Error in IO. Assume everything scrod. */
      case 'N':
      case 'Q':
        trap_normal_termination ();
      case 'R':
        trap_recover ();
    }
  }
}

#define STATE_UNKNOWN		(LONG_TO_UNSIGNED_FIXNUM (0))
#define STATE_PRIMITIVE		(LONG_TO_UNSIGNED_FIXNUM (1))
#define STATE_COMPILED_CODE	(LONG_TO_UNSIGNED_FIXNUM (2))
#define STATE_PROBABLY_COMPILED	(LONG_TO_UNSIGNED_FIXNUM (3))

struct trap_recovery_info
{
  SCHEME_OBJECT state;
  SCHEME_OBJECT pc_info_1;
  SCHEME_OBJECT pc_info_2;
  SCHEME_OBJECT extra_trap_info;
};

static struct trap_recovery_info dummy_recovery_info =
{
  STATE_UNKNOWN,
  SHARP_F,
  SHARP_F,
  SHARP_F
};

struct dos_trap_code_desc
{
  int trapno;
  unsigned long code_mask;
  unsigned long code_value;
  char *name;
};

static struct dos_trap_code_desc dos_trap_codes [64];

#define DECLARE_DOS_TRAP_CODE(s, m, v, n)				\
{									\
  ((dos_trap_codes [i]) . trapno) = (s);				\
  ((dos_trap_codes [i]) . code_mask) = (m);				\
  ((dos_trap_codes [i]) . code_value) = (v);				\
  ((dos_trap_codes [i]) . name) = (n);					\
  i += 1;								\
}

static SCHEME_OBJECT
DEFUN (find_trap_code_name, (trapno, info, scp),
       int trapno AND
       SIGINFO_T info AND
       struct FULL_SIGCONTEXT * scp)
{
  unsigned long code = 0;
  char * name = 0;
  if (SIGINFO_VALID_P (info))
    {
      code = (SIGINFO_CODE (info));
      {
	struct dos_trap_code_desc * entry = (& (dos_trap_codes [0]));
	while ((entry -> trapno) != DOS_INVALID_TRAP)
	  if (((entry -> trapno) == trapno)
	      && (((entry -> code_mask) & code) == (entry -> code_value)))
	  {
	    name = (entry -> name);
	    break;
	  }
	  else
	    entry += 1;
      }
    }
  return (cons ((long_to_integer ((long) code)),
		((name == 0) ? SHARP_F
		 : (char_pointer_to_string ((unsigned char *) name)))));
}

static void
DEFUN_VOID (initialize_dos_trap_codes)
{
  unsigned int i = 0;

  DECLARE_DOS_TRAP_CODE (DOS_EXCP_Integer_divide_by_zero,
			 0, 0,
			 "Integer divide by zero");
  DECLARE_DOS_TRAP_CODE (DOS_EXCP_Debug_exception,
			 0, 0,
			 "Debug exception");
  DECLARE_DOS_TRAP_CODE (DOS_EXCP_Non_maskable_interrupt,
			 0, 0,
			 "Non-maskable interrupt");
  DECLARE_DOS_TRAP_CODE (DOS_EXCP_Breakpoint,
			 0, 0,
			 "Breakpoint");
  DECLARE_DOS_TRAP_CODE (DOS_EXCP_Integer_overflow,
			 0, 0,
			 "Integer overflow");
  DECLARE_DOS_TRAP_CODE (DOS_EXCP_Bounds_check,
			 0, 0,
			 "Bounds check");
  DECLARE_DOS_TRAP_CODE (DOS_EXCP_Invalid_opcode,
			 0, 0,
			 "Invalid opcode");
  DECLARE_DOS_TRAP_CODE (DOS_EXCP_Numeric_co_processor_not_available,
			 0, 0,
			 "Numeric co-processor not available");
  DECLARE_DOS_TRAP_CODE (DOS_EXCP_Double_fault,
			 0, 0,
			 "Double fault");
  DECLARE_DOS_TRAP_CODE (DOS_EXCP_Numeric_co_processor_segment_overrun,
			 0, 0,
			 "Numeric co-processor segment overrun");
  DECLARE_DOS_TRAP_CODE (DOS_EXCP_Invalid_TSS,
			 0, 0,
			 "Invalid TSS");
  DECLARE_DOS_TRAP_CODE (DOS_EXCP_Segment_not_present,
			 0, 0,
			 "Segment not present");
  DECLARE_DOS_TRAP_CODE (DOS_EXCP_Stack_exception,
			 0, 0,
			 "Stack exception");
  DECLARE_DOS_TRAP_CODE (DOS_EXCP_General_protection,
			 0, 0,
			 "General protection");
  DECLARE_DOS_TRAP_CODE (DOS_EXCP_Page_Fault,
			 0, 0,
			 "Page Fault");
  DECLARE_DOS_TRAP_CODE (DOS_EXCP_Floating_point_exception,
			 0, 0,
			 "Floating-point exception");
  DECLARE_DOS_TRAP_CODE (DOS_EXCP_Alignment_check,
			 0, 0,
			 "Alignment check");
  DECLARE_DOS_TRAP_CODE (DOS_INVALID_TRAP, 0, 0, ((char *) 0));
  return;
}

static CONST char *
trap_names[NUM_DOS_EXCP] =
{
  "Integer divide by zero",
  "Debugging trap",
  "NMI interrupt",
  "Breakpoint exception",
  "INTO -- integer overflow",
  "BOUND -- range exceeded",
  "UD -- invalid opcode",
  "NM -- 387 not available",
  "DF -- double fault",
  "387 segment overrun",
  "TS -- invalid TSS",
  "NP -- segment not present",
  "SS -- stack fault",
  "GP -- general protection",
  "PF -- page fault",
  ((CONST char *) NULL),
  "MF -- floating-point error",
  "AC -- alignment check"
};

CONST char *
DEFUN (find_trap_name, (trapno), int trapno)
{
  static char buffer [64], * name;
  if ((trapno >= 0) &&
      (trapno < ((sizeof (trap_names)) / (sizeof (char *)))))
  {
    name = trap_names[trapno];
    if ((name != ((char *) NULL))
        && (name[0] != '\0'))
      return ((CONST char *) name);
  }
  sprintf (buffer, "unknown exception %d", trapno);
  return ((CONST char *) buffer);
}

static void
DEFUN (setup_trap_frame, (trapno, info, scp, trinfo, new_stack_pointer),
       int trapno AND
       SIGINFO_T info AND
       struct FULL_SIGCONTEXT * scp AND
       struct trap_recovery_info * trinfo AND
       SCHEME_OBJECT * new_stack_pointer)
{
  SCHEME_OBJECT handler;
  SCHEME_OBJECT trap_name, trap_code;
  int stack_recovered_p = (new_stack_pointer != 0);
  long saved_mask = (FETCH_INTERRUPT_MASK ());
  SET_INTERRUPT_MASK (0);	/* To prevent GC for now. */
  if ((! (Valid_Fixed_Obj_Vector ())) ||
      ((handler = (Get_Fixed_Obj_Slot (Trap_Handler))) == SHARP_F))
    {
      fprintf (stderr, "There is no trap handler for recovery!\n");
      fprintf (stderr, "Trap = %s.\n", (find_trap_name (trapno)));
      fprintf (stderr, "pc = %04x:%08lx; sp = %04x:%08lx.\n",
	       scp->sc_cs, scp->sc_eip, scp->sc_ss, scp->sc_esp);
      fflush (stderr);
      termination_trap ();
    }
  if (Free > MemTop)
    Request_GC (0);

  trap_name =
    ((trapno <= 0)
     ? SHARP_F
     : (char_pointer_to_string
	((unsigned char *) (find_trap_name (trapno)))));
  trap_code = (find_trap_code_name (trapno, info, scp));
  if (!stack_recovered_p)
    {
      Initialize_Stack ();
     Will_Push (CONTINUATION_SIZE);
      Store_Return (RC_END_OF_COMPUTATION);
      Store_Expression (SHARP_F);
      Save_Cont ();
     Pushed ();
    }
  else
    Stack_Pointer = new_stack_pointer;
 Will_Push (7 + CONTINUATION_SIZE);
  STACK_PUSH (trinfo -> extra_trap_info);
  STACK_PUSH (trinfo -> pc_info_2);
  STACK_PUSH (trinfo -> pc_info_1);
  STACK_PUSH (trinfo -> state);
  STACK_PUSH (BOOLEAN_TO_OBJECT (stack_recovered_p));
  STACK_PUSH (trap_code);
  STACK_PUSH (trap_name);
  Store_Return (RC_HARDWARE_TRAP);
  Store_Expression (long_to_integer (trapno));
  Save_Cont ();
 Pushed ();
  if (stack_recovered_p
      /* This may want to do it in other cases, but this may be enough. */
      && (trinfo->state == STATE_COMPILED_CODE))
    Stop_History ();

  History = (Make_Dummy_History ());
 Will_Push (STACK_ENV_EXTRA_SLOTS + 2);
  STACK_PUSH (trap_name);
  STACK_PUSH (handler);
  STACK_PUSH (STACK_FRAME_HEADER + 1);
 Pushed ();
  SET_INTERRUPT_MASK (saved_mask);
  abort_to_interpreter (PRIM_APPLY);
}

/* DOS_INVALID_TRAP is an invalid trap, it means a user requested reset. */

void
DEFUN (hard_reset, (scp), struct FULL_SIGCONTEXT * scp)
{
  continue_from_trap (DOS_INVALID_TRAP, 0, scp);
}

/* Called synchronously. */

void
DEFUN_VOID (soft_reset)
{
  struct trap_recovery_info trinfo;
  SCHEME_OBJECT * new_stack_pointer =
    (((Stack_Pointer <= Stack_Top) && (Stack_Pointer > Stack_Guard))
     ? Stack_Pointer
     : 0);
  if ((Regs[REGBLOCK_PRIMITIVE]) != SHARP_F)
    {
      (trinfo . state) = STATE_PRIMITIVE;
      (trinfo . pc_info_1) = (Regs[REGBLOCK_PRIMITIVE]);
      (trinfo . pc_info_2) =
	(LONG_TO_UNSIGNED_FIXNUM (Regs[REGBLOCK_LEXPR_ACTUALS]));
      (trinfo . extra_trap_info) = SHARP_F;
    }
  else
    {
      (trinfo . state) = STATE_UNKNOWN;
      (trinfo . pc_info_1) = SHARP_F;
      (trinfo . pc_info_2) = SHARP_F;
      (trinfo . extra_trap_info) = SHARP_F;
    }
  if ((Free >= Heap_Top) || (Free < Heap_Bottom))
    /* Let's hope this works. */
    Free = MemTop;
  setup_trap_frame (DOS_INVALID_TRAP, 0, 0, (&trinfo), new_stack_pointer);
}

#if !defined(HAVE_SIGCONTEXT) || !defined(HAS_COMPILER_SUPPORT) || defined(USE_STACKLETS)

static void
DEFUN (continue_from_trap, (trapno, info, scp),
       int trapno AND
       SIGINFO_T info AND
       struct FULL_SIGCONTEXT * scp)
{
  if (Free < MemTop)
    Free = MemTop;
  setup_trap_frame (trapno, info, scp, (&dummy_recovery_info), 0);
}

#else /* HAVE_SIGCONTEXT and HAS_COMPILER_SUPPORT and not USE_STACKLETS */

/* Heuristic recovery from processor traps/exceptions.

   continue_from_trap attempts to:

   1) validate the trap information (pc and sp);
   2) determine whether compiled code was executing, a primitive was
      executing, or execution was in the interpreter;
   3) guess what C global state is still valid; and
   4) set up a recovery frame for the interpreter so that debuggers can
      display more information. */

#include "gccode.h"

#define SCHEME_ALIGNMENT_MASK		((sizeof (long)) - 1)
#define STACK_ALIGNMENT_MASK		SCHEME_ALIGNMENT_MASK
#define FREE_PARANOIA_MARGIN		0x100

/* PCs must be aligned according to this. */

#define PC_ALIGNMENT_MASK		((1 << PC_ZERO_BITS) - 1)

/* But they may have bits that can be masked by this. */

#ifndef PC_VALUE_MASK
#define PC_VALUE_MASK			(~0)
#endif

#define C_STACK_SIZE			0x01000000

#ifdef HAS_COMPILER_SUPPORT
#define ALLOW_ONLY_C 0
#else
#define ALLOW_ONLY_C 1
#define PLAUSIBLE_CC_BLOCK_P(block)	0
#endif

static SCHEME_OBJECT * EXFUN
  (find_block_address, (char * pc_value, SCHEME_OBJECT * area_start));

#if 0
#define get_etext() (&etext)
#else
/* For now */
#define get_etext() (Heap_Bottom)
#endif

static void
DEFUN (continue_from_trap, (trapno, info, scp),
       int trapno AND
       SIGINFO_T info AND
       struct FULL_SIGCONTEXT * scp)
{
  extern unsigned short scheme_ss;
  int pc_in_C;
  int pc_in_heap;
  int pc_in_constant_space;
  int pc_in_scheme;
  int pc_in_hyper_space;
  int scheme_sp_valid;
  long C_sp;
  long scheme_sp;
  long the_pc;
  SCHEME_OBJECT * new_stack_pointer;
  SCHEME_OBJECT * xtra_info;
  struct trap_recovery_info trinfo;

  if (scp == ((struct FULL_SIGCONTEXT *) NULL))
  {
    if (Free < MemTop)
      Free = MemTop;
    setup_trap_frame (trapno, info, scp, (&dummy_recovery_info), 0);
    /*NOTREACHED*/
  }

  C_sp = (FULL_SIGCONTEXT_SP (scp));
  scheme_sp = (FULL_SIGCONTEXT_SCHSP (scp));
  the_pc = ((FULL_SIGCONTEXT_PC (scp)) & PC_VALUE_MASK);

#if FALSE
  fprintf (stderr, "\ncontinue_from_trap:");
  fprintf (stderr, "\tpc = 0x%08lx\n", the_pc);
  fprintf (stderr, "\tCsp = 0x%08lx\n", C_sp);
  fprintf (stderr, "\tssp = 0x%08lx\n", scheme_sp);
  fprintf (stderr, "\tesp = 0x%08lx\n", Ext_Stack_Pointer);
#endif

  if (((the_pc & PC_ALIGNMENT_MASK) != 0)
      || (scp->sc_cs != initial_C_cs))
  {
    pc_in_C = 0;
    pc_in_heap = 0;
    pc_in_constant_space = 0;
    pc_in_scheme = 0;
    pc_in_hyper_space = 1;
  }
  else
  {
    pc_in_C = (the_pc <= ((long) (get_etext ())));
    pc_in_heap =
      ((the_pc < ((long) Heap_Top)) && (the_pc >= ((long) Heap_Bottom)));
    pc_in_constant_space =
      ((the_pc < ((long) Constant_Top)) &&
       (the_pc >= ((long) Constant_Space)));
    pc_in_scheme = (pc_in_heap || pc_in_constant_space);
    pc_in_hyper_space = ((!pc_in_C) && (!pc_in_scheme));
  }

  scheme_sp_valid =
    (pc_in_scheme
     && (((scp->sc_ss & 0xffff) == (scp->sc_ds & 0xffff))
	 || ((scheme_ss != 0)
	     && ((scp->sc_ss & 0xffff) == scheme_ss)))
     && ((scp->sc_ds & 0xffff) == (initial_C_ds & 0xffff))
     && ((scheme_sp < ((long) Stack_Top)) &&
	 (scheme_sp >= ((long) Absolute_Stack_Base)) &&
	 ((scheme_sp & STACK_ALIGNMENT_MASK) == 0)));

  new_stack_pointer =
    (scheme_sp_valid
     ? ((SCHEME_OBJECT *) scheme_sp)
     : ((pc_in_C
	&& ((scp->sc_ss & 0xffff) == (initial_C_ss & 0xffff))
	&& (Stack_Pointer < Stack_Top)
	&& (Stack_Pointer > Absolute_Stack_Base))
        ? Stack_Pointer
        : ((SCHEME_OBJECT *) 0)));

  if (pc_in_hyper_space || (pc_in_scheme && ALLOW_ONLY_C))
  {
    /* In hyper space. */
    (trinfo . state) = STATE_UNKNOWN;
    (trinfo . pc_info_1) = SHARP_F;
    (trinfo . pc_info_2) = SHARP_F;
    new_stack_pointer = 0;
    if ((Free < MemTop) ||
	(Free >= Heap_Top) ||
	((((unsigned long) Free) & SCHEME_ALIGNMENT_MASK) != 0))
      Free = MemTop;
  }
  else if (pc_in_scheme)
  {
    /* In compiled code. */
    SCHEME_OBJECT * block_addr;
    SCHEME_OBJECT * maybe_free;
    block_addr =
      (find_block_address (((PTR) the_pc),
			   (pc_in_heap ? Heap_Bottom : Constant_Space)));
    if (block_addr == 0)
    {
      (trinfo . state) = STATE_PROBABLY_COMPILED;
      (trinfo . pc_info_1) = (LONG_TO_UNSIGNED_FIXNUM (the_pc));
      (trinfo . pc_info_2) = SHARP_F;
      if ((Free < MemTop) ||
	  (Free >= Heap_Top) ||
	  ((((unsigned long) Free) & SCHEME_ALIGNMENT_MASK) != 0))
	Free = MemTop;
    }
    else
    {
      (trinfo . state) = STATE_COMPILED_CODE;
      (trinfo . pc_info_1) =
	(MAKE_POINTER_OBJECT (TC_COMPILED_CODE_BLOCK, block_addr));
      (trinfo . pc_info_2) =
	(LONG_TO_UNSIGNED_FIXNUM (the_pc - ((long) block_addr)));
#ifdef HAVE_FULL_SIGCONTEXT
      maybe_free = ((SCHEME_OBJECT *) (FULL_SIGCONTEXT_RFREE (scp)));
      if (((((unsigned long) maybe_free) & SCHEME_ALIGNMENT_MASK) == 0)
	  && (maybe_free >= Heap_Bottom) && (maybe_free < Heap_Top))
	Free = (maybe_free + FREE_PARANOIA_MARGIN);
      else
#endif
      {
	if ((Free < MemTop) || (Free >= Heap_Top)
	    || ((((unsigned long) Free) & SCHEME_ALIGNMENT_MASK) != 0))
	  Free = MemTop;
      }
    }
  }
  else
  {
    /* In the interpreter, a primitive, or a compiled code utility. */

    SCHEME_OBJECT primitive = (Regs[REGBLOCK_PRIMITIVE]);

    if ((OBJECT_TYPE (primitive)) != TC_PRIMITIVE)
    {
      (trinfo . state) = STATE_UNKNOWN;
      (trinfo . pc_info_1) = SHARP_F;
      (trinfo . pc_info_2) = SHARP_F;
      new_stack_pointer = 0;
    }
    else
    {
      long primitive_address =
	((long) (Primitive_Procedure_Table[OBJECT_DATUM (primitive)]));
      (trinfo . state) = STATE_PRIMITIVE;
      (trinfo . pc_info_1) = primitive;
      (trinfo . pc_info_2) =
	(LONG_TO_UNSIGNED_FIXNUM (Regs[REGBLOCK_LEXPR_ACTUALS]));
    }
    if ((new_stack_pointer == 0)
	|| ((((unsigned long) Free) & SCHEME_ALIGNMENT_MASK) != 0)
	|| ((Free < Heap_Bottom) || (Free >= Heap_Top))
	|| ((Free < MemTop) && ((Free + FREE_PARANOIA_MARGIN) >= MemTop)))
      Free = MemTop;
    else if ((Free + FREE_PARANOIA_MARGIN) < MemTop)
      Free +=  FREE_PARANOIA_MARGIN;
  }
  xtra_info = Free;
  Free += (1 + 2 + PROCESSOR_NREGS);
  (trinfo . extra_trap_info) =
    (MAKE_POINTER_OBJECT (TC_NON_MARKED_VECTOR, xtra_info));
  (*xtra_info++) =
    (MAKE_OBJECT (TC_MANIFEST_NM_VECTOR, (2 + PROCESSOR_NREGS)));
  (*xtra_info++) = ((SCHEME_OBJECT) the_pc);
  (*xtra_info++) = ((SCHEME_OBJECT) C_sp);
  {
    int counter = FULL_SIGCONTEXT_NREGS;
    int * regs = (FULL_SIGCONTEXT_FIRST_REG (scp));
    while ((counter--) > 0)
      (*xtra_info++) = ((SCHEME_OBJECT) (*regs++));
  }
  /* We assume that regs,sp,pc is the order in the processor.
     Scheme can always fix this. */
  if ((PROCESSOR_NREGS - FULL_SIGCONTEXT_NREGS) > 0)
    (*xtra_info++) = ((SCHEME_OBJECT) C_sp);
  if ((PROCESSOR_NREGS - FULL_SIGCONTEXT_NREGS) > 1)
    (*xtra_info++) = ((SCHEME_OBJECT) the_pc);
  setup_trap_frame (trapno, info, scp, (&trinfo), new_stack_pointer);
}

/* Find the compiled code block in area which contains `pc_value'.
   This attempts to be more efficient than `find_block_address_in_area'.
   If the pointer is in the heap, it can actually do twice as
   much work, but it is expected to pay off on the average. */

static SCHEME_OBJECT * EXFUN
  (find_block_address_in_area, (char * pc_value, SCHEME_OBJECT * area_start));

#define MINIMUM_SCAN_RANGE		2048

static SCHEME_OBJECT *
DEFUN (find_block_address, (pc_value, area_start),
       char * pc_value AND
       SCHEME_OBJECT * area_start)
{
  if (area_start == Constant_Space)
    {
      extern SCHEME_OBJECT * EXFUN
	(find_constant_space_block, (SCHEME_OBJECT *));
      SCHEME_OBJECT * constant_block =
	(find_constant_space_block
	 ((SCHEME_OBJECT *)
	  (((unsigned long) pc_value) &~ SCHEME_ALIGNMENT_MASK)));
      return
	((constant_block == 0)
	 ? 0
	 : (find_block_address_in_area (pc_value, constant_block)));
    }
  {
    SCHEME_OBJECT * nearest_word =
      ((SCHEME_OBJECT *)
       (((unsigned long) pc_value) &~ SCHEME_ALIGNMENT_MASK));
    long maximum_distance = (nearest_word - area_start);
    long distance = maximum_distance;
    while ((distance / 2) > MINIMUM_SCAN_RANGE)
      distance = (distance / 2);
    while ((distance * 2) < maximum_distance)
      {
	SCHEME_OBJECT * block =
	  (find_block_address_in_area (pc_value, (nearest_word - distance)));
	if (block != 0)
	  return (block);
	distance *= 2;
      }
  }
  return (find_block_address_in_area (pc_value, area_start));
}

/*
  Find the compiled code block in area which contains `pc_value',
  by scanning sequentially the complete area.
  For the time being, skip over manifest closures and linkage sections. */

static SCHEME_OBJECT *
DEFUN (find_block_address_in_area, (pc_value, area_start),
       char * pc_value AND
       SCHEME_OBJECT * area_start)
{
  SCHEME_OBJECT * first_valid = area_start;
  SCHEME_OBJECT * area = area_start;
  while (((char *) area) < pc_value)
    {
      SCHEME_OBJECT object = (*area);
      switch (OBJECT_TYPE (object))
	{
	case TC_LINKAGE_SECTION:
	  {
	    switch (READ_LINKAGE_KIND (object))
	    {
	      case OPERATOR_LINKAGE_KIND:
	      case GLOBAL_OPERATOR_LINKAGE_KIND:
	      {
		long count = (READ_OPERATOR_LINKAGE_COUNT (object));
		area = ((END_OPERATOR_LINKAGE_AREA (area, count)) + 1);
		break;
	      }

	      default:
#if FALSE
	      {
		gc_death (TERM_EXIT,
			  "find_block_address: Unknown compiler linkage kind.",
			  area, NULL);
		/*NOTREACHED*/
	      }
#else
	      /* Fall through, no reason to crash here. */
#endif
	      case REFERENCE_LINKAGE_KIND:
	      case ASSIGNMENT_LINKAGE_KIND:
	        area += ((READ_CACHE_LINKAGE_COUNT (object)) + 1);
		break;

	    }
	    break;
	  }
	case TC_MANIFEST_CLOSURE:
	  {
	    area += 1;
	    {
	      long count = (MANIFEST_CLOSURE_COUNT (area));
	      area = ((MANIFEST_CLOSURE_END (area, count)) + 1);
	    }
	    break;
	  }
	case TC_MANIFEST_NM_VECTOR:
	  {
	    long count = (OBJECT_DATUM (object));
	    if (((char *) (area + (count + 1))) < pc_value)
	      {
		area += (count + 1);
		first_valid = area;
		break;
	      }
	    {
	      SCHEME_OBJECT * block = (area - 1);
	      return
		(((area == first_valid) ||
		  ((OBJECT_TYPE (*block)) != TC_MANIFEST_VECTOR) ||
		  ((OBJECT_DATUM (*block)) < (count + 1)) ||
		  (! (PLAUSIBLE_CC_BLOCK_P (block))))
		 ? 0
		 : block);
	    }
	  }
	default:
	  {
	    area += 1;
	    break;
	  }
	}
    }
  return (0);
}

#endif /* HAVE_SIGCONTEXT and HAS_COMPILER_SUPPORT and not USE_STACKLETS */


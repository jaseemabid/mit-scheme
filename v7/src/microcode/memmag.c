/* -*-C-*-

$Id: memmag.c,v 9.71.2.4 2006/08/30 03:00:08 cph Exp $

Copyright 1986,1987,1988,1989,1990,1991 Massachusetts Institute of Technology
Copyright 1992,1993,1994,1995,1996,1997 Massachusetts Institute of Technology
Copyright 2000,2002,2005,2006 Massachusetts Institute of Technology

This file is part of MIT/GNU Scheme.

MIT/GNU Scheme is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

MIT/GNU Scheme is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with MIT/GNU Scheme; if not, write to the Free Software
Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301,
USA.

*/

/* Memory management top level  */

#include "scheme.h"
#include "prims.h"
#include "history.h"
#include "memmag.h"
#include "gccode.h"
#include "osscheme.h"

#ifndef DEFAULT_ACTIVE_HEAP_RESERVED
#  define DEFAULT_ACTIVE_HEAP_RESERVED 4500
#endif

/* buffer for impurify, etc. */
#ifndef CONSTANT_SPACE_FUDGE
#  define CONSTANT_SPACE_FUDGE 128
#endif

static unsigned long saved_heap_size;
static unsigned long saved_constant_size;
static unsigned long saved_stack_size;

#ifdef __WIN32__
   static bool win32_flush_old_halfspace_p = false;
   static void win32_advise_end_GC (void);
   static void win32_flush_old_halfspace (void);
#  define ADVISE_END_GC win32_advise_end_GC
#endif

/* Memory Allocation, sequential processor:

oo
   ------------------------------------------ <- fixed boundary (currently)
   |           Heap 2			    |
   |                                        |
   ------------------------------------------ <- boundary moved by purify
   |           Heap 1			    |
   |                                        |
   ------------------------------------------ <- boundary moved by purify
   |     Constant + Pure Space    /\        |
   |                              ||        |
   ------------------------------------------ <- fixed boundary (currently)
   |         Control Stack        ||        |
   |                              \/        |
   ------------------------------------------ <- fixed boundary (currently)
0

   Each area has a pointer to its starting address and a pointer to
   the next free cell (for the stack, it is a pointer to the last cell
   in use).  In addition, there is a pointer to the top of the
   useable area of the heap (the heap is subdivided into two areas for
   the purposes of GC, and this pointer indicates the top of the half
   currently in use).  */

void
setup_memory (unsigned long heap_size,
	      unsigned long stack_size,
	      unsigned long constant_size)
{
  ALLOCATE_REGISTERS ();

  /* Consistency check 1 */
  if ((heap_size == 0) || (stack_size == 0) || (constant_size == 0))
    {
      outf_fatal ("Configuration won't hold initial data.\n");
      outf_flush_fatal ();
      exit (1);
    }

  /* Allocate */
  ALLOCATE_HEAP_SPACE ((stack_size
			+ (2 * heap_size)
			+ constant_size),
		       memory_block_start,
		       memory_block_end);

  /* Consistency check 2 */
  if (memory_block_start == 0)
    {
      outf_fatal ("Not enough memory for this configuration.\n");
      outf_flush_fatal ();
      exit (1);
    }

  /* Consistency check 3 */
  if ((ADDRESS_TO_DATUM (memory_block_end)) > DATUM_MASK)
    {
      outf_fatal ("Requested allocation is too large.\n");
      outf_fatal ("Try again with a smaller argument to '--heap'.\n");
      outf_flush_fatal ();
      reset_memory ();
      exit (1);
    }

  saved_heap_size = heap_size;
  saved_constant_size = constant_size;
  saved_stack_size = stack_size;
  reset_allocator_parameters ();
}

void
reset_memory (void)
{
  HEAP_FREE (memory_block_start);
  DEALLOCATE_REGISTERS ();
}

void
reset_allocator_parameters (void)
{
  active_heap_reserved = DEFAULT_ACTIVE_HEAP_RESERVED;
  gc_space_needed = 0;
  SET_STACK_LIMITS (memory_block_start, saved_stack_size);
  constant_start = (memory_block_start + saved_stack_size);
  constant_alloc_next = constant_start;
  update_allocator_parameters (0);
  INITIALIZE_STACK ();
  STACK_RESET ();
}

bool
update_allocator_parameters (unsigned long n_reserved)
{
  SCHEME_OBJECT * nctop
    = (constant_alloc_next + n_reserved + CONSTANT_SPACE_FUDGE);

  if (nctop >= memory_block_end)
    return (false);

  constant_end = nctop;
  if (((memory_block_end - constant_end) % 2) == 1)
    /* Guarantee that both heaps are exactly the same size.  */
    constant_end += 1;

  active_heap_start = constant_end;
  active_heap_end
    = (active_heap_start + ((memory_block_end - constant_end) / 2));
  inactive_heap_start = active_heap_end;
  inactive_heap_end = memory_block_end;

  Free = active_heap_start;
  RESET_HEAP_ALLOC_LIMIT ();

  return (true);
}

bool
object_in_active_heap_p (SCHEME_OBJECT object)
{
  SCHEME_OBJECT * address = (get_object_address (object));
  return ((address != 0) && (ADDRESS_IN_ACTIVE_HEAP_P (address)));
}

DEFINE_PRIMITIVE ("GARBAGE-COLLECT", Prim_garbage_collect, 1, 1,
		  "(SAFETY-MARGIN)\n\
Performs a garbage collection and returns the number of words\n\
available for further allocation.  Also sets the "safety margin",\n\
which is the number of reserved words at the top of the heap, to\n\
SAFETY-MARGIN, which must be a non-negative integer.  Finally, runs\n\
the primitive GC daemons before returning.")
{
  PRIMITIVE_HEADER (1);
  SCHEME_OBJECT * from_start;
  SCHEME_OBJECT * from_end;
  canonicalize_primitive_context ();

  STACK_CHECK_FATAL ("GC");
  if (Free > active_heap_end)
    {
      outf_fatal ("\nGC has been delayed too long!\n");
      outf_fatal
	("Free = %#lx; heap_alloc_limit = %#lx; active_heap_end = %#lx\n",
	 ((unsigned long) Free),
	 ((unsigned long) heap_alloc_limit),
	 ((unsigned long) active_heap_end));
      Microcode_Termination (TERM_NO_SPACE);
    }

  active_heap_reserved = (ARG_HEAP_RESERVED (1));
  POP_PRIMITIVE_FRAME (1);

  ENTER_CRITICAL_SECTION ("garbage collector");

  from_start = active_heap_start;
  from_end = Free;
  if (((constant_end - constant_alloc_next) < CONSTANT_SPACE_FUDGE)
      && (inactive_heap_start < active_heap_start)
      && (update_allocator_parameters (0)))
    ;
  else
    {
      SCHEME_OBJECT * new_start = inactive_heap_start;
      SCHEME_OBJECT * new_end = inactive_heap_end;
      inactive_heap_start = active_heap_start;
      inactive_heap_end = active_heap_end;
      active_heap_start = new_start;
      active_heap_end = new_end;
      Free = active_heap_start;
      RESET_HEAP_ALLOC_LIMIT ();
    }
  initialize_weak_chain ();

  garbage_collect (from_start, from_end);

  Will_Push (CONTINUATION_SIZE);
  SET_RC (RC_NORMAL_GC_DONE);
  SET_EXP (ULONG_TO_FIXNUM (HEAP_AVAILABLE));
  SAVE_CONT ();
  Pushed ();

  RENAME_CRITICAL_SECTION ("garbage collector daemon");
  {
    SCHEME_OBJECT daemon = (VECTOR_REF (fixed_objects, GC_DAEMON));
    if (daemon == SHARP_F)
      PRIMITIVE_ABORT (PRIM_POP_RETURN);

    Will_Push (2);
    STACK_PUSH (daemon);
    PUSH_APPLY_FRAME_HEADER (0);
    Pushed ();
    PRIMITIVE_ABORT (PRIM_APPLY);
    /*NOTREACHED*/
  }
  PRIMITIVE_RETURN (UNSPECIFIC);
}

void
garbage_collect (SCHEME_OBJECT * from_start, SCHEME_OBJECT * from_end)
{
  SCHEME_OBJECT * saved_values = Free;

  (*Free++) = fixed_objects;
  (*Free++) = (MAKE_POINTER_OBJECT (UNMARKED_HISTORY_TYPE, history_register));
  (*Free++) = current_state_point;

#ifdef ENABLE_GC_DEBUGGING_TOOLS
  initialize_gc_objects_referencing ();
#endif

  std_gc_loop (stack_pointer, (&stack_end),
	       (&Free), (&active_heap_end),
	       from_start, from_end);
  std_gc_loop (constant_start, (&constant_alloc_next),
	       (&Free), (&active_heap_end),
	       from_start, from_end);
  std_gc_loop (saved_values, (&Free),
	       (&Free), (&active_heap_end),
	       from_start, from_end);

#ifdef ENABLE_GC_DEBUGGING_TOOLS
  scan_gc_objects_referencing (from_start, from_end);
#endif

  update_weak_pointers ();

  fixed_objects = (*saved_values++);
  history_register = (OBJECT_ADDRESS (*saved_values++));
  current_state_point = (*saved_values++);

  CC_TRANSPORT_END ();
#ifdef ADVISE_END_GC
  ADVISE_END_GC ();
#endif

  CLEAR_INTERRUPT (INT_GC);
}

void
stack_death (const char * name)
{
  outf_fatal
    ("\n%s: The stack has overflowed and overwritten adjacent memory.\n",
     name);
  outf_fatal ("This was probably caused by a runaway recursion.\n");
  Microcode_Termination (TERM_STACK_OVERFLOW);
  /*NOTREACHED*/
}

DEFINE_PRIMITIVE ("GC-TRACE-REFERENCES", Prim_gc_trace_references, 2, 2, 0)
{
  PRIMITIVE_HEADER (2);
  {
    SCHEME_OBJECT collector = (ARG_REF (2));
    if (! ((collector == SHARP_F)
	   || ((VECTOR_P (collector))
	       && ((VECTOR_LENGTH (collector)) >= 1))))
      error_wrong_type_arg (2);
#ifdef ENABLE_GC_DEBUGGING_TOOLS
    collect_gc_objects_referencing ((ARG_REF (1)), collector);
#else
    error_external_return ();
#endif
  }
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("WIN32-FLUSH-OLD-HALFSPACE-AFTER-GC?!",
		  Prim_win32_flush_old_halfspace_after_gc, 1, 1,
		  "(BOOLEAN)")
{
  PRIMITIVE_HEADER (1);
#ifdef __WIN32__
  {
    bool old = win32_flush_old_halfspace_p;
    win32_flush_old_halfspace_p = (OBJECT_TO_BOOLEAN (ARG_REF (1)));
    PRIMITIVE_RETURN (old ? SHARP_T : SHARP_F);
  }
#else
  error_unimplemented_primitive ();
  PRIMITIVE_RETURN (UNSPECIFIC);
#endif
}

DEFINE_PRIMITIVE ("WIN32-FLUSH-OLD-HALFSPACE!",
		  Prim_win32_flush_old_halfspace, 0, 0, "()")
{
  PRIMITIVE_HEADER (0);
#ifdef __WIN32__
  win32_flush_old_halfspace ();
#else
  error_unimplemented_primitive ();
#endif
  PRIMITIVE_RETURN (UNSPECIFIC);
}

#ifdef __WIN32__

static void
win32_advise_end_GC (void)
{
  if (win32_flush_old_halfspace_p)
    win32_flush_old_halfspace ();
}

static void
win32_flush_old_halfspace (void)
{
  /* Since we allocated the heap with VirtualAlloc, we can decommit
     the old half-space to tell the VM system that it contains trash.
     Immediately recommitting the region allows the old half-space to
     be used for temporary storage (e.g. by fasdump).  Note that this
     is only a win when it prevents paging.  When no paging would have
     happened, we incur the cost of zero-filling the recommitted
     pages.  This can be significant - up to 50% of the time taken to
     GC, but usually somewhat less.

     We are careful to play with pages that are strictly within the old
     half-space, hence the `pagesize' arithmetic.  */

  unsigned long pagesize = 4096;
  /* round up to page boundary */
  char * start
    = ((char *)
       ((((unsigned long) inactive_heap_start)
	 &~ (pagesize - 1))
	+ pagesize));
  /* round down to page boundary */
  char * end
    = ((char *)
       (((unsigned long) inactive_heap_end) &~ (pagesize - 1)));
  VirtualFree (start, (end - start), MEM_DECOMMIT);
  VirtualAlloc (start, (end - start), MEM_COMMIT, PAGE_READWRITE);
}

#endif /* __WIN32__ */

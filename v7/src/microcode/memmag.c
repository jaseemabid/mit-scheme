/* -*-C-*-

$Id: memmag.c,v 9.71.2.1 2005/08/22 18:05:59 cph Exp $

Copyright 1986,1987,1988,1989,1990,1991 Massachusetts Institute of Technology
Copyright 1992,1993,1994,1995,1996,1997 Massachusetts Institute of Technology
Copyright 2000,2002,2005 Massachusetts Institute of Technology

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
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
USA.

*/

/* Memory management top level  */

#include "scheme.h"
#include "prims.h"
#include "history.h"
#include "memmag.h"
#include "gccode.h"
#include "osscheme.h"

/* These are the values of active_heap_start and active_heap_end just
   prior to switching the heaps for GC.  This is needed because the
   heap might be resized when switching, in which case
   ADDRESS_IN_INACTIVE_HEAP_P won't do the right thing.  Instead, use
   ADDRESS_IN_OLD_SPACE_P, which refers to these variables.  */
SCHEME_OBJECT * old_space_start;
SCHEME_OBJECT * old_space_end;

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
static SCHEME_OBJECT * weak_chain;

#ifdef ENABLE_GC_DEBUGGING_TOOLS

#  ifndef GC_SCAN_HISTORY_SIZE
#    define GC_SCAN_HISTORY_SIZE 1024
#  endif

   static unsigned int gc_scan_history_index;
   static SCHEME_OBJECT * gc_scan_history [GC_SCAN_HISTORY_SIZE];
   static SCHEME_OBJECT * gc_to_history [GC_SCAN_HISTORY_SIZE];

   static SCHEME_OBJECT gc_trap
     = (MAKE_OBJECT (TC_REFERENCE_TRAP, TRAP_MAX_IMMEDIATE));
   static SCHEME_OBJECT * gc_scan_trap = 0;
   static SCHEME_OBJECT * gc_free_trap = 0;

   static SCHEME_OBJECT gc_object_referenced = SHARP_F;
   static SCHEME_OBJECT gc_objects_referencing = SHARP_F;
   static unsigned long gc_objects_referencing_count;
   static SCHEME_OBJECT * gc_objects_referencing_scan;
   static SCHEME_OBJECT * gc_objects_referencing_end;

   static void initialize_gc_objects_referencing (void);
   static void update_gc_objects_referencing (void);
   static void scan_gc_objects_referencing (void);
#endif

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
  canonicalize_primitive_context ();

  if (STACK_OVERFLOWED_P ())
    stack_death ("GC");
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
  gc_counter += 1;

  PRESERVE_OLD_SPACE_LIMITS ();
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

  garbage_collect ();

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
garbage_collect (void)
{
  SCHEME_OBJECT * saved_values = Free;

  (*Free++) = fixed_objects;
  (*Free++) = (MAKE_POINTER_OBJECT (UNMARKED_HISTORY_TYPE, history_register));
  (*Free++) = current_state_point;

#ifdef ENABLE_GC_DEBUGGING_TOOLS
  initialize_gc_objects_referencing ();
#endif

  gc_loop (stack_pointer, (&stack_end), (&Free));
  gc_loop (constant_start, (&constant_alloc_next), (&Free));
  gc_loop (saved_values, (&Free), (&Free));

#ifdef ENABLE_GC_DEBUGGING_TOOLS
  scan_gc_objects_referencing ();
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

/* Weak pairs are supported by adding an extra pass to the GC.  During
   the normal pass, a weak pair is transported to new space, but the
   car of the pair is marked as a non-pointer so that won't be traced.
   Then the original weak pair in old space is chained into a list.
   This work is performed by 'note_weak_pair'.

   At the end of this pass, we have a list of all of the old weak
   pairs.  Since each weak pair in old space has a broken-heart
   pointer to the corresponding weak pair in new space, we also have a
   list of all of the new weak pairs.

   The extra pass then traverses this list, restoring the original
   type of the object in the car of each pair.  Then, if the car is a
   pointer that hasn't been copied to new space, it is replaced by #F.
   This work is performed by 'update_weak_pointers'.

   Here is a diagram showing the layout of a weak pair immediately
   after it is transported to new space.  After the normal pass is
   complete, the only thing that will have changed is that the "old
   CDR object" will have been updated to point to new space, if it is
   a pointer object.


   weak_chain       old space           |         new space
       |      _______________________   |   _______________________
       |      |broken |     new     |   |   |      |              |
       +=====>|heart  |  location ======|==>| NULL | old CAR data |
	      |_______|_____________|   |   |______|______________|
	      |old car|   next in   |   |   |                     |
	      | type  |    chain    |   |   |   old CDR object    |
	      |_______|_____________|   |   |_____________________|

 */

void
initialize_weak_chain (void)
{
  weak_chain = 0;
}

void
note_weak_pair (SCHEME_OBJECT pair, SCHEME_OBJECT * new_addr)
{
  SCHEME_OBJECT * old_addr = (OBJECT_ADDRESS (pair));
  SCHEME_OBJECT old_car = (new_addr[0]);
  SCHEME_OBJECT * caddr;

  /* Don't add pair to chain unless old_car is a pointer into old
     space.  */

  switch (gc_ptr_type (old_car))
    {
    case GC_POINTER_NORMAL:
      caddr = (OBJECT_ADDRESS (old_car));
      break;

    case GC_POINTER_COMPILED:
      caddr = (cc_entry_address_to_block_address (CC_ENTRY_ADDRESS (old_car)));
      break;

    default:
      caddr = 0;
      break;
    }
  if ((caddr != 0) && (ADDRESS_IN_OLD_SPACE_P (caddr)))
    {
      (old_addr[1])
	= (MAKE_POINTER_OBJECT ((OBJECT_TYPE (old_car)), weak_chain));
      (new_addr[0]) = (OBJECT_NEW_TYPE (TC_NULL, old_car));
      weak_chain = old_addr;
    }
}

void
update_weak_pointers (void)
{
  while (weak_chain != 0)
    {
      SCHEME_OBJECT * new_addr = (OBJECT_ADDRESS (weak_chain[0]));
      SCHEME_OBJECT old_car
	= (OBJECT_NEW_TYPE ((OBJECT_TYPE (weak_chain[1])), (new_addr[0])));

      switch (gc_ptr_type (old_car))
	{
	case GC_POINTER_NORMAL:
	  {
	    SCHEME_OBJECT * addr = (OBJECT_ADDRESS (old_car));
	    (*new_addr)
	      = ((BROKEN_HEART_P (*addr))
		 ? (MAKE_OBJECT_FROM_OBJECTS (old_car, (*addr)))
		 : SHARP_F);
	  }
	  break;

	case GC_POINTER_COMPILED:
	  {
	    SCHEME_OBJECT * addr
	      = (cc_entry_address_to_block_address
		 (CC_ENTRY_ADDRESS (old_car)));
	    (*new_addr)
	      = ((BROKEN_HEART_P (*addr))
		 ? (CC_ENTRY_NEW_BLOCK (old_car,
					(OBJECT_ADDRESS (*addr)),
					addr))
		 : SHARP_F);
	  }
	  break;

	case GC_POINTER_NOT:
	  /* Shouldn't happen -- filtered out in 'note_weak_pair'.  */
	  (*new_addr) = old_car;
	  break;
	}
      weak_chain = (OBJECT_ADDRESS (weak_chain[1]));
    }
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

DEFINE_PRIMITIVE ("GC-TRACE-REFERENCES", Prim_gc_trace_references, 2, 2, 0)
{
  PRIMITIVE_HEADER (2);
  {
    SCHEME_OBJECT objects_referencing = (ARG_REF (2));
    if (! ((objects_referencing == SHARP_F)
	   || ((VECTOR_P (objects_referencing))
	       && ((VECTOR_LENGTH (objects_referencing)) >= 1))))
      error_wrong_type_arg (2);
#ifdef ENABLE_GC_DEBUGGING_TOOLS
    gc_object_referenced = (ARG_REF (1));
    gc_objects_referencing = objects_referencing;
#else
    error_external_return ();
#endif
  }
  PRIMITIVE_RETURN (UNSPECIFIC);
}

#ifdef ENABLE_GC_DEBUGGING_TOOLS

void
initialize_gc_history (void)
{
  gc_scan_history_index = 0;
  memset (gc_scan_history, 0, (sizeof (gc_scan_history)));
  memset (gc_to_history, 0, (sizeof (gc_to_history)));
}

void
handle_gc_trap (SCHEME_OBJECT * scan,
		SCHEME_OBJECT ** pto,
		SCHEME_OBJECT object)
{
  (gc_scan_history[gc_scan_history_index]) = scan;
  (gc_to_history[gc_scan_history_index]) = ((pto == 0) ? 0 : (*pto));
  gc_scan_history_index += 1;
  if (gc_scan_history_index == GC_SCAN_HISTORY_SIZE)
    gc_scan_history_index = 0;
  if ((object == gc_trap)
      || ((gc_scan_trap != 0) && (scan >= gc_scan_trap))
      || ((gc_free_trap != 0) && (pto != 0) && ((*pto) >= gc_free_trap)))
    {
      outf_error ("\ngc_loop: trap.\n");
      abort ();
    }
}

void
debug_transport_one_word (SCHEME_OBJECT object, SCHEME_OBJECT * from)
{
  if ((gc_object_referenced == (*from))
      && (gc_objects_referencing != SHARP_F))
    {
      gc_objects_referencing_count += 1;
      if (gc_objects_referencing_scan != gc_objects_referencing_end)
	{
	  update_gc_objects_referencing ();
	  (*gc_objects_referencing_scan++) = object;
	}
    }
}

static void
initialize_gc_objects_referencing (void)
{
  if (gc_objects_referencing != SHARP_F)
    {
      /* Temporarily change to non-marked vector.  */
      MEMORY_SET
	(gc_objects_referencing, 0,
	 (MAKE_OBJECT
	  (TC_MANIFEST_NM_VECTOR,
	   (OBJECT_DATUM (MEMORY_REF (gc_objects_referencing, 0))))));
      /* Wipe the table.  */
      {
	SCHEME_OBJECT * scan = (VECTOR_LOC (gc_objects_referencing, 0));
	SCHEME_OBJECT * end
	  = (VECTOR_LOC (gc_objects_referencing,
			 (VECTOR_LENGTH (gc_objects_referencing))));
	while (scan < end)
	  (*scan++) = SHARP_F;
      }
      gc_objects_referencing_count = 0;
      gc_objects_referencing_scan = (VECTOR_LOC (gc_objects_referencing, 1));
      gc_objects_referencing_end
	= (VECTOR_LOC (gc_objects_referencing,
		       (VECTOR_LENGTH (gc_objects_referencing))));
      (*Free++) = gc_objects_referencing;
    }
}

static void
scan_gc_objects_referencing (void)
{
  if (gc_objects_referencing != SHARP_F)
    {
      update_gc_objects_referencing ();
      /* Change back to marked vector.  */
      MEMORY_SET
	(gc_objects_referencing, 0,
	 (MAKE_OBJECT
	  (TC_MANIFEST_VECTOR,
	   (OBJECT_DATUM (MEMORY_REF (gc_objects_referencing, 0))))));
      /* Store the count in the table.  */
      VECTOR_SET (gc_objects_referencing, 0,
		  (ULONG_TO_FIXNUM (gc_objects_referencing_count)));

      {
	SCHEME_OBJECT * end = gc_objects_referencing_scan;
	gc_loop ((VECTOR_LOC (gc_objects_referencing, 1)), (&end), (&end));
	if (end != gc_objects_referencing_scan)
	  gc_death (TERM_BROKEN_HEART, 0, 0,
		    "scan of gc_objects_referencing performed transport");
      }
      gc_objects_referencing = SHARP_F;
      gc_object_referenced = SHARP_F;
    }
}

static void
update_gc_objects_referencing (void)
{
  SCHEME_OBJECT header = (MEMORY_REF (gc_objects_referencing, 0));
  if (BROKEN_HEART_P (header))
    {
      SCHEME_OBJECT new
	= (MAKE_OBJECT_FROM_OBJECTS (gc_objects_referencing, header));
      gc_objects_referencing_scan =
	(VECTOR_LOC (new,
		     (gc_objects_referencing_scan
		      - (VECTOR_LOC (gc_objects_referencing, 0)))));
      gc_objects_referencing_end = (VECTOR_LOC (new, (VECTOR_LENGTH (new))));
      gc_objects_referencing = new;
    }
}

#endif /* ENABLE_GC_DEBUGGING_TOOLS */

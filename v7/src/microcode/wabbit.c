/* -*-C-*-

$Id: wabbit.c,v 1.1 1994/02/15 04:37:44 gjr Exp $

Copyright (c) 1994 Massachusetts Institute of Technology

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

/* 
 *
 * What's opera, doc?!
 * This file contains the wabbit-hunting garbage collector,
 * by Ziggy and GJR.
 *
 */

#include "scheme.h"
#include "gccode.h"

extern SCHEME_OBJECT Weak_Chain;

extern SCHEME_OBJECT *
  EXFUN (wabbit_hunt_gcloop, (SCHEME_OBJECT *, SCHEME_OBJECT **));

extern void
  EXFUN (wabbit_season, (SCHEME_OBJECT));

extern void
  EXFUN (duck_season, (SCHEME_OBJECT));

extern void
  EXFUN (fix_weak_chain_and_hunt_wabbits, (void));

/* Wabbit hunting code */
/* Be wary, wary, quiet... */

Boolean
  wabbit_holes_discarded_p,
  wabbit_holes_overwritten_p,
  wabbit_all_dead_p;  

SCHEME_OBJECT
  hare_marker,
  * wabbit_holes,
  * wabbit_holes_hi,
  * wabbit_lo_address,
  * wabbit_hi_address,
  * wabbit_of_Seville,
  * wabbit_buffer_lo,
  * wabbit_buffer_ptr,
  * wabbit_buffer_hi,
  * hares_lo,
  * hares_hi;

#define ELMER_FUDGE_FACTOR	 4	/* Size of QUAD */
#define ELMER_HUNG_FACTOR	20	/* 1 / (Sales tax in MA in 1994) */
#define RAJIV_SURATI_FACTOR     -20     /* -1 * ELMER_HUNG_FACTOR */

void EXFUN (kill_da_wabbit, (SCHEME_OBJECT *, SCHEME_OBJECT *));
Boolean EXFUN (discard_wabbit_holes_p, (SCHEME_OBJECT *, SCHEME_OBJECT *));

/* We need not check wabbit_lo_address by construction:
   wabbit_lo_address is Free at the beginning of the GC, and
   all forwarded objects will point above that, except for
   the wabbit of Seville, a.k.a. the wabbit vector.
 */

#define WABBIT_P(addr) (((addr) < wabbit_hi_address)			\
			&& ((addr) != wabbit_of_Seville))

#define HARE_P(addr) ((* addr) == hare_marker)

#define RECORD_WABBIT_HOLE(tag, address) do				\
{									\
  if ((wabbit_holes > (new_space_free + ELMER_FUDGE_FACTOR))		\
      || (discard_wabbit_holes_p (scan, new_space_free)))		\
    *--wabbit_holes = (MAKE_POINTER_OBJECT (tag, address));		\
} while (0)

#define KILL_DA_WABBIT(where, last_block) do				\
{									\
  if ((wabbit_buffer_ptr + 2) <= wabbit_buffer_hi)			\
    kill_da_wabbit (where, last_block);					\
  else									\
    wabbit_all_dead_p = false;						\
} while (0)

/* Oh, what have I done!  I've killed the little bunny wabbit... */

#define COPY_CELL()							\
{									\
  *new_space_free++ = *old_space_addr;					\
}

#define COPY_PAIR()							\
{									\
  *new_space_free++ = *old_space_addr++;				\
  *new_space_free++ = *old_space_addr;					\
}

#define COPY_TRIPLE()							\
{									\
  *new_space_free++ = *old_space_addr++;				\
  *new_space_free++ = *old_space_addr++;				\
  *new_space_free++ = *old_space_addr;					\
}

#define COPY_QUADRUPLE()						\
{									\
  *new_space_free++ = *old_space_addr++;				\
  *new_space_free++ = *old_space_addr++;				\
  *new_space_free++ = *old_space_addr++;				\
  *new_space_free++ = *old_space_addr;					\
}

#define COPY_VECTOR()							\
{									\
  long veclen = (1 + (OBJECT_DATUM (* old_space_addr)));		\
  SCHEME_OBJECT * vecend = (new_space_free + veclen);			\
									\
  if (vecend > wabbit_holes)						\
    discard_wabbit_holes_p (scan, new_space_free);			\
  while (new_space_free != vecend)					\
    *new_space_free++ = *old_space_addr++;				\
}

#define COPY_WEAK_PAIR()						\
{									\
  long car_tag = (OBJECT_TYPE (* old_space_addr));			\
  (*new_space_free++)							\
    = (OBJECT_NEW_TYPE (TC_NULL, (* old_space_addr)));			\
  *new_space_free++ = *++old_space_addr;				\
  * old_space_addr = (OBJECT_NEW_TYPE (car_tag, Weak_Chain));		\
  Weak_Chain = this_object;						\
}

#define RELOCATE_NORMAL_SETUP()						\
{									\
  old_space_addr = (OBJECT_ADDRESS (this_object));			\
  if (old_space_addr < low_heap)					\
  {									\
    if (HARE_P (old_space_addr))					\
      KILL_DA_WABBIT (scan, ((SCHEME_OBJECT *) NULL));			\
    continue;								\
  }									\
  if ((OBJECT_TYPE (* old_space_addr)) == TC_BROKEN_HEART)		\
  {									\
    new_space_addr = (OBJECT_ADDRESS (* old_space_addr));		\
    if (WABBIT_P (new_space_addr))					\
      KILL_DA_WABBIT (scan, ((SCHEME_OBJECT *) NULL));			\
    * scan = (MAKE_OBJECT_FROM_OBJECTS (this_object,			\
					(* old_space_addr)));		\
    continue;								\
  }									\
}

#define RELOCATE_NORMAL_END()						\
{									\
  (* (OBJECT_ADDRESS (this_object)))					\
    = (MAKE_BROKEN_HEART (new_space_addr));				\
  (* scan) = (MAKE_POINTER_OBJECT ((OBJECT_TYPE (this_object)),		\
				   new_space_addr));			\
  continue;								\
}

#define RELOCATE_NORMAL_POINTER(copy_code)				\
{									\
  RELOCATE_NORMAL_SETUP ();						\
  new_space_addr = new_space_free;					\
  copy_code;								\
  RECORD_WABBIT_HOLE ((OBJECT_TYPE (this_object)), new_space_addr);	\
  RELOCATE_NORMAL_END ();						\
}

#define RELOCATE_ALIGNED_POINTER(copy_code)				\
{									\
  RELOCATE_NORMAL_SETUP ();						\
  ALIGN_FLOAT (new_space_free);						\
  new_space_addr = new_space_free;					\
  copy_code;								\
  RECORD_WABBIT_HOLE ((OBJECT_TYPE (this_object)), new_space_addr);	\
  RELOCATE_NORMAL_END ();						\
}

#define RELOCATE_RAW_POINTER(tag, copy_code, last_block)		\
{									\
  old_space_addr = ((SCHEME_OBJECT *) this_object);			\
  if (old_space_addr < low_heap)					\
  {									\
    if (HARE_P (old_space_addr))					\
      KILL_DA_WABBIT (scan, last_block);				\
    continue;								\
  }									\
  if ((OBJECT_TYPE (* old_space_addr)) == TC_BROKEN_HEART)		\
  {									\
    new_space_addr = (OBJECT_ADDRESS (* old_space_addr));		\
    if (WABBIT_P (new_space_addr))					\
      KILL_DA_WABBIT (scan, last_block);				\
    * scan = ((SCHEME_OBJECT) new_space_addr);				\
    continue;								\
  }									\
  {									\
    SCHEME_OBJECT * saved_old_addr = old_space_addr;			\
									\
    new_space_addr = new_space_free;					\
    copy_code;								\
    RECORD_WABBIT_HOLE (tag, new_space_addr);				\
    (* saved_old_addr) = (MAKE_BROKEN_HEART (new_space_addr));		\
    (* scan) = ((SCHEME_OBJECT) new_space_addr);			\
    continue;								\
  }									\
}

#define RELOCATE_COMPILED_ENTRY(last_block)				\
{									\
  Get_Compiled_Block (old_space_addr,					\
		      ((SCHEME_OBJECT *) this_entry));			\
  if (old_space_addr < low_heap)					\
  {									\
    if (HARE_P (old_space_addr))					\
      KILL_DA_WABBIT (scan, last_block);				\
    new_entry = this_entry;						\
  }									\
  else if ((OBJECT_TYPE (* old_space_addr)) == TC_BROKEN_HEART)		\
  {									\
    new_space_addr = (OBJECT_ADDRESS (* old_space_addr));		\
    if (WABBIT_P (new_space_addr))					\
      KILL_DA_WABBIT (scan, last_block);				\
    new_entry =								\
      ((SCHEME_OBJECT)							\
       (RELOCATE_COMPILED_INTERNAL (this_entry,				\
				    new_space_addr,			\
				    old_space_addr)));			\
  }									\
  else									\
  {									\
    SCHEME_OBJECT * saved_old_addr = old_space_addr;			\
									\
    ALIGN_FLOAT (new_space_free);					\
    new_space_addr = new_space_free;					\
    new_entry =								\
      ((SCHEME_OBJECT)							\
       (RELOCATE_COMPILED_INTERNAL (this_entry,				\
				    new_space_addr,			\
				    old_space_addr)));			\
    COPY_VECTOR ();							\
    RECORD_WABBIT_HOLE (TC_COMPILED_CODE_BLOCK, new_space_addr);	\
    (* saved_old_addr) = (MAKE_BROKEN_HEART (new_space_addr));		\
  }									\
}

SCHEME_OBJECT *
DEFUN (wabbit_hunt_gcloop, (scan, new_space_free_loc),
       fast SCHEME_OBJECT * scan
       AND SCHEME_OBJECT ** new_space_free_loc)
{
  fast SCHEME_OBJECT
    * new_space_free, * old_space_addr, this_object,
    * low_heap, * new_space_addr, this_entry, new_entry,
    * last_cc_block_start, * last_nmv;

  last_cc_block_start = ((SCHEME_OBJECT *) NULL);
  last_nmv = ((SCHEME_OBJECT *) NULL);
  new_space_free = * new_space_free_loc;
  low_heap = Constant_Top;
  for ( ; scan != new_space_free; scan++)
  {
    this_object = * scan;

    Switch_by_GC_Type (this_object)
    {
      case TC_BROKEN_HEART:
        if (scan == (OBJECT_ADDRESS (this_object)))
	{
	  * new_space_free_loc = new_space_free;
	  return (scan);
	}
	else if (this_object != hare_marker)
	{
	  sprintf (gc_death_message_buffer,
		   "wabbit_hunt_gcloop: broken heart (0x%lx) in scan",
		   this_object);
	  gc_death (TERM_BROKEN_HEART, gc_death_message_buffer,
		    scan, new_space_free);
	  /*NOTREACHED*/
	}
	break;

      case TC_MANIFEST_NM_VECTOR:
      case TC_MANIFEST_SPECIAL_NM_VECTOR:
	if (((OBJECT_TYPE (scan[-1])) != TC_MANIFEST_VECTOR)
	    || ((last_nmv != ((SCHEME_OBJECT *) NULL))
		&& ((last_nmv + (1 + (OBJECT_DATUM (* last_nmv))))
		    == scan)))
	  last_cc_block_start = ((SCHEME_OBJECT *) NULL);
	else
	  last_cc_block_start = (scan - 1);

	last_nmv = scan;
	scan += (OBJECT_DATUM (this_object));
	break;

      /* Compiled code relocation. */

      case TC_LINKAGE_SECTION:
      {
	if ((last_cc_block_start == ((SCHEME_OBJECT *) NULL))
	    || ((last_cc_block_start
		 + (1 + (OBJECT_DATUM (* last_cc_block_start))))
		< scan))
	  last_cc_block_start = scan;
	  
	switch (READ_LINKAGE_KIND (this_object))
	{
	  case REFERENCE_LINKAGE_KIND:
	  case ASSIGNMENT_LINKAGE_KIND:
	  {
	    /* Assumes that all others are objects of type TC_QUAD without
	       their type codes.
	       */

	    fast long count;

	    scan++;
	    for (count = (READ_CACHE_LINKAGE_COUNT (this_object));
		 --count >= 0;
		 scan += 1)
	    {
	      this_object = (* scan);
	      RELOCATE_RAW_POINTER (TC_QUAD, COPY_QUADRUPLE (), last_cc_block);
	    }
	    scan -= 1;
	    break;
	  }

	  case OPERATOR_LINKAGE_KIND:
	  case GLOBAL_OPERATOR_LINKAGE_KIND:
	  {
	    fast long count;
	    fast char * word_ptr;
	    SCHEME_OBJECT * end_scan;

	    START_OPERATOR_RELOCATION (scan);
	    count = (READ_OPERATOR_LINKAGE_COUNT (this_object));
	    word_ptr = (FIRST_OPERATOR_LINKAGE_ENTRY (scan));
	    end_scan = (END_OPERATOR_LINKAGE_AREA (scan, count));

	    while (--count >= 0)
	    {
	      scan = ((SCHEME_OBJECT *) word_ptr);
	      word_ptr = (NEXT_LINKAGE_OPERATOR_ENTRY (word_ptr));
	      EXTRACT_OPERATOR_LINKAGE_ADDRESS (this_entry, scan);
	      RELOCATE_COMPILED_ENTRY (last_cc_block);
	      STORE_OPERATOR_LINKAGE_ADDRESS (new_entry, scan);
	    }
	    scan = end_scan;
	    END_OPERATOR_RELOCATION (scan);
	    break;
	  }

	  case CLOSURE_PATTERN_LINKAGE_KIND:
	    scan += (READ_CACHE_LINKAGE_COUNT (this_object));
	    break;

	  default:
	  {
	    gc_death (TERM_EXIT,
		      "GC: Unknown compiler linkage kind.",
		      scan, Free);
	    /*NOTREACHED*/
	  }
	}
	break;
      }

      case TC_MANIFEST_CLOSURE:
      {
	fast long count;
	fast char * word_ptr;
	SCHEME_OBJECT * area_end;

	last_cc_block_start = scan;
	START_CLOSURE_RELOCATION (scan);
	scan += 1;
	count = (MANIFEST_CLOSURE_COUNT (scan));
	word_ptr = (FIRST_MANIFEST_CLOSURE_ENTRY (scan));
	area_end = (MANIFEST_CLOSURE_END (scan, count));

	while ((--count) >= 0)
	{
	  scan = ((SCHEME_OBJECT *) (word_ptr));
	  word_ptr = (NEXT_MANIFEST_CLOSURE_ENTRY (word_ptr));
	  EXTRACT_CLOSURE_ENTRY_ADDRESS (this_entry, scan);
	  RELOCATE_COMPILED_ENTRY (last_cc_block);
	  STORE_CLOSURE_ENTRY_ADDRESS (new_entry, scan);
	}

	scan = area_end;
	END_CLOSURE_RELOCATION (scan);
	break;
      }

      case_compiled_entry_point:
      {
	this_entry = ((SCHEME_OBJECT) (OBJECT_ADDRESS (this_object)));
	RELOCATE_COMPILED_ENTRY ((SCHEME_OBJECT *) NULL);
	(* scan) = (MAKE_POINTER_OBJECT ((OBJECT_TYPE (this_object)),
					 ((SCHEME_OBJECT *) new_entry)));
	continue;
      }

      case_Cell:
	RELOCATE_NORMAL_POINTER (COPY_CELL ());
	break;

      case TC_REFERENCE_TRAP:
	if ((OBJECT_DATUM (this_object)) <= TRAP_MAX_IMMEDIATE)
	{
	  /* It is a non pointer. */
	  break;
	}
	/* Fall Through. */

      case_Pair:
	RELOCATE_NORMAL_POINTER (COPY_PAIR ());
	break;

      case TC_VARIABLE:
      case_Triple:
	RELOCATE_NORMAL_POINTER (COPY_TRIPLE ());
	break;

      case_Quadruple:
	RELOCATE_NORMAL_POINTER (COPY_QUADRUPLE ());
	break;

      case_Aligned_Vector:
	RELOCATE_ALIGNED_POINTER (COPY_VECTOR ());
	break;

      case TC_FUTURE:
	if (Future_Spliceable (this_object))
	{
	  * scan = (Future_Value (this_object));
	  scan -= 1;
	  continue;
	}
	/* fall through */

      case_Vector:
	RELOCATE_NORMAL_POINTER (COPY_VECTOR ());
	break;

      case TC_WEAK_CONS:
	RELOCATE_NORMAL_POINTER (COPY_WEAK_PAIR ());
	break;

      default:
	sprintf (gc_death_message_buffer,
		 "wabbit_hunt_gcloop: bad type code (0x%02x)",
		 (OBJECT_TYPE (this_object)));
	gc_death (TERM_INVALID_TYPE_CODE,
		  gc_death_message_buffer,
		  scan, new_space_free);
	/*NOTREACHED*/

      case_Non_Pointer:
	break;

      }	/* Switch_by_GC_Type */
  } /* For loop */

  * new_space_free_loc = new_space_free;
  return (new_space_free);

} /* wabbit_hunt_gcloop */

void
DEFUN (wabbit_season, (wabbit_descriptor),
       SCHEME_OBJECT wabbit_descriptor)
{
  long n_wabbits, buf_len;
  SCHEME_OBJECT
    * result, * area, * saved_area, * wabbit_ptr,
    wabbit_buffer, wabbit_vector, * wabbit_vector_ptr;

  wabbit_vector = (VECTOR_REF (wabbit_descriptor, 1));
  wabbit_buffer = (VECTOR_REF (wabbit_descriptor, 2));
    
  buf_len = (VECTOR_LENGTH (wabbit_buffer));
  n_wabbits = (VECTOR_LENGTH (wabbit_vector));

  wabbit_all_dead_p = true;
  wabbit_holes_overwritten_p = false;
  wabbit_holes_discarded_p = false;
  wabbit_holes_hi = Heap_Top;
  wabbit_holes = wabbit_holes_hi;

  saved_area = area = Free;
  wabbit_lo_address = saved_area;
  wabbit_hi_address = saved_area;
  wabbit_of_Seville = saved_area;
  hare_marker = (MAKE_BROKEN_HEART (wabbit_of_Seville));

  wabbit_vector_ptr = (MEMORY_LOC (wabbit_vector, 0));
  while (n_wabbits >= 0)
  {
    *area++ = *wabbit_vector_ptr++;
    n_wabbits -= 1;
  }
  MEMORY_SET (wabbit_vector, 0, (MAKE_BROKEN_HEART (saved_area)));
  *area = (MAKE_BROKEN_HEART (area));
  Free = (area + 1);
  
  result = (wabbit_hunt_gcloop (saved_area, &Free));
  if (result != area)
  {
    outf_fatal ("\nwabbit_hunt Wabbit scan ended too early.\n");
    Microcode_Termination (TERM_BROKEN_HEART);
  }

  /* Check whether any wabbits are hares, and if so, mark them so. */
  /* *** HERE *** */


  *area = SHARP_F;		/* Remove broken heart on Valentine's day */
  wabbit_lo_address = (area + 1);
  wabbit_hi_address = Free;

  if (BROKEN_HEART_P (MEMORY_REF (wabbit_buffer, 0)))
    /* One of the wabbits is the wabbit buffer itself! */
    wabbit_buffer_lo = (OBJECT_ADDRESS (MEMORY_REF (wabbit_buffer, 0)));
  else
  {
    wabbit_buffer_lo = Free;
    MEMORY_SET (wabbit_buffer, 0, (MAKE_BROKEN_HEART (wabbit_buffer_lo)));
    Free += (1 + buf_len);
  }
  wabbit_buffer_hi = (wabbit_buffer_lo + (1 + buf_len));
  * wabbit_buffer_lo = (MAKE_OBJECT (TC_MANIFEST_NM_VECTOR, buf_len));
  wabbit_buffer_ptr = (wabbit_buffer_lo + 3);

  result = (wabbit_hunt_gcloop (wabbit_lo_address, &Free));
  if (Free != result)
  {
    outf_fatal ("\nwabbit_hunt: heap scan ended too early.\n");
    Microcode_Termination (TERM_BROKEN_HEART);
  }
  return;
}

void
DEFUN (duck_season, (wabbit_descriptor),
       SCHEME_OBJECT wabbit_descriptor)
{
  wabbit_buffer_lo[2] =
    (LONG_TO_UNSIGNED_FIXNUM (wabbit_buffer_ptr - (wabbit_buffer_lo + 1)));
  while (wabbit_buffer_ptr < wabbit_buffer_hi)
    *wabbit_buffer_ptr++ = SHARP_F;
  wabbit_buffer_lo[1] = (BOOLEAN_TO_OBJECT (wabbit_all_dead_p));
  wabbit_buffer_lo[0]
    = (MAKE_OBJECT (TC_MANIFEST_VECTOR,
		    (wabbit_buffer_hi - (wabbit_buffer_lo + 1))));

  if ((VECTOR_REF (wabbit_descriptor, 3)) == SHARP_T)
  {
    SCHEME_OBJECT * guaranteed_free = (Free + (GC_Reserve + 2));
    SCHEME_OBJECT * source, * dest, result;
    long len;

    if (guaranteed_free > wabbit_holes)
    {
      wabbit_holes_discarded_p = true;
      wabbit_holes = guaranteed_free;
    }
    dest = Free;
    result = (MAKE_POINTER_OBJECT (TC_VECTOR, dest));
    source = wabbit_holes;
    len = (wabbit_holes_hi - source);
    *dest++ = (MAKE_OBJECT (TC_MANIFEST_VECTOR, (len + 1)));
    *dest++ = (BOOLEAN_TO_OBJECT (! (wabbit_holes_discarded_p
				     || wabbit_holes_overwritten_p)));
    while (--len >= 0)
      *dest++ = *source++;
    Free = dest;
    VECTOR_SET (wabbit_descriptor, 3, result);
  }

  VECTOR_SET (wabbit_descriptor, 0, SHARP_T);
  return;
}

SCHEME_OBJECT *
DEFUN (hunt_wabbit, (where), SCHEME_OBJECT * where)
{
  SCHEME_OBJECT * ptr_lo, * ptr_hi, * ptr_mid, * hole;

  ptr_lo = wabbit_holes;
  ptr_hi = (wabbit_holes_hi - 1);

  while (ptr_lo < ptr_hi)
  {
    ptr_mid = (ptr_lo + ((ptr_hi - ptr_lo) / 2));
    hole = (OBJECT_ADDRESS (* ptr_mid));
    if (where < hole)
      ptr_lo = (ptr_mid + 1);
    else if (where > hole)
      ptr_hi = ptr_mid;
    else
    {
      ptr_hi = ptr_mid;
      ptr_lo = ptr_mid;
      break;
    }
  }
  return (ptr_lo);
}

Boolean
DEFUN (discard_wabbit_holes_p, (scan, free),
       SCHEME_OBJECT * scan AND SCHEME_OBJECT * free)
{
  SCHEME_OBJECT * hole, * new_hole;
  long keep_index;

  if (free > wabbit_holes)
  {
    wabbit_holes_overwritten_p = true;
    wabbit_holes = free;    
  }
  if (scan < Constant_Top)
    return (free < wabbit_holes);

  hole = ((hunt_wabbit (scan)) + 1);

  /* This guarantees that we don't get into quadratic copying:
     We discard only if the fraction of holes being discarded
     is at least 1/ELMER_HUNG_FACTOR of the total number of holes.
   */

  if ((ELMER_HUNG_FACTOR * (wabbit_holes_hi - hole))
      < (wabbit_holes_hi - wabbit_holes))
    return (free < wabbit_holes);

  keep_index = (hole - wabbit_holes);
  new_hole = wabbit_holes_hi;

  while (--keep_index >= 0)
    *--new_hole = *--hole;

  wabbit_holes = new_hole;
  wabbit_holes_discarded_p = true;
  return (free < wabbit_holes);
}

#define TC_HEADLESS_REFERENCE		TC_NULL
#define TC_REFERENCE_TO_STACK		TC_STACK_ENVIRONMENT
#define TC_REFERENCE_TO_CONSTANT_SPACE	TC_CHARACTER

void
DEFUN (kill_da_wabbit, (where, last_block),
       SCHEME_OBJECT * where AND SCHEME_OBJECT * last_block)
{
  SCHEME_OBJECT * hole, wabbit, * wabbit_addr;
  long offset, max_offset;

  /* With my sword and magic helmet... */

  if (where < Constant_Top)
  {
    SCHEME_OBJECT head;

    if (last_block != ((SCHEME_OBJECT *) NULL))
    {
      offset = (where - last_block);
      if (((OBJECT_TYPE (* last_block)) == TC_MANIFEST_VECTOR)
	  || ((OBJECT_TYPE (* last_block)) == TC_MANIFEST_CLOSURE))
	head = (MAKE_POINTER_OBJECT (TC_COMPILED_CODE_BLOCK, last_block));
      else
	head = (MAKE_POINTER_OBJECT (TC_HEADLESS_REFERENCE, last_block));
    }
    else
    {
      /* If we do cwcc before calling the special garbage collector,
	 there should be no references to the stack.
       */
      offset = 0;
      if (where < Stack_Top)
	head = (MAKE_POINTER_OBJECT (TC_REFERENCE_TO_STACK, where));
      else
	head = (MAKE_POINTER_OBJECT (TC_REFERENCE_TO_CONSTANT_SPACE, where));
    }

    *wabbit_buffer_ptr++ = head;
    *wabbit_buffer_ptr++ = (LONG_TO_UNSIGNED_FIXNUM (offset));
    return;
  }
  if (wabbit_holes >= wabbit_holes_hi)
    return;

  hole = (hunt_wabbit (where));
  wabbit = (* hole);
  wabbit_addr = (OBJECT_ADDRESS (wabbit));
  offset = (where - wabbit_addr);
  *wabbit_buffer_ptr++ = wabbit;
  *wabbit_buffer_ptr++ = (LONG_TO_UNSIGNED_FIXNUM (offset));

  if ((hole == wabbit_holes)
      && wabbit_holes_overwritten_p && (where != wabbit_addr))
  {
    switch (GC_Type_Map[(OBJECT_TYPE (wabbit))])
    {
      case GC_Pair:
        max_offset = 2;
	break;

      case GC_Triple:
        max_offset = 3;
	break;

      case GC_Quadruple:
        max_offset = 4;
	break;

      case GC_Vector:
	max_offset = (1 + (OBJECT_DATUM (* wabbit_addr)));
	break;
	
      case GC_Special:
        if ((OBJECT_TYPE (* hole)) == TC_REFERENCE_TRAP)
	{
	  max_offset = 2;
	  break;
	}
	/* fall through */

      case GC_Cell:	/* => (where == wabbit_addr), already tested */
      default:
	max_offset = -1;
    }
    if ((max_offset == -1) || (where > (wabbit_addr + max_offset)))
    {
      wabbit_buffer_ptr -= 2;
      wabbit_all_dead_p = false;
    }
  }
  return;
}

/* Alternate version of Fix_Weak_Chain that hunts wabbits. */

void
DEFUN_VOID (fix_weak_chain_and_hunt_wabbits)
{
  fast SCHEME_OBJECT
    * old_weak_pair, * scan, nulled_car, * new_space_addr,
    this_object, * old_space_addr, * low_heap;

  low_heap = Constant_Top;
  while (Weak_Chain != EMPTY_LIST)
  {
    old_weak_pair = (OBJECT_ADDRESS (Weak_Chain));
    scan = (OBJECT_ADDRESS (*old_weak_pair++));
    Weak_Chain = * old_weak_pair;
    nulled_car = * scan;
    this_object = (MAKE_OBJECT_FROM_OBJECTS (Weak_Chain, nulled_car));
    Weak_Chain = (OBJECT_NEW_TYPE (TC_NULL, Weak_Chain));

    switch (GC_Type (this_object))
    {
      case GC_Non_Pointer:
        *scan = this_object;
	continue;

      case GC_Special:
	if ((OBJECT_TYPE (this_object)) != TC_REFERENCE_TRAP)
	{
	  /* No other special type makes sense here. */
	  goto fail;
	}
	if ((OBJECT_DATUM (this_object)) <= TRAP_MAX_IMMEDIATE)
	{
	  * scan = this_object;
	  continue;
	}
	/* Otherwise, it is a pointer.  Fall through */

      /* Normal pointer types, the broken heart is in the first word.
         Note that most special types are treated normally here.
       */
      case GC_Cell:
      case GC_Pair:
      case GC_Triple:
      case GC_Quadruple:
      case GC_Vector:
	* scan = this_object;	/* In case it points to constant space */
	RELOCATE_NORMAL_SETUP ();
	* scan = SHARP_F;
	continue;

      case GC_Compiled:
	* scan = this_object;
	old_space_addr = (OBJECT_ADDRESS (this_object));
	if (old_space_addr < low_heap)
	  continue;
	Get_Compiled_Block (old_space_addr, old_space_addr);
	if ((OBJECT_TYPE (* old_space_addr)) == TC_BROKEN_HEART)
	{
	  new_space_addr = (OBJECT_ADDRESS (* old_space_addr));
	  if (WABBIT_P (new_space_addr))
	    KILL_DA_WABBIT (scan);
	    
	  * scan = (RELOCATE_COMPILED (this_object,
				       new_space_addr,
				       old_space_addr));
	  continue;
	}
	* scan = SHARP_F;
	continue;

      case GC_Undefined:
	outf_error
	  ("\nfix_weak_chain_and_hunt_wabbits: Clearing bad object 0x%08lx.\n",
	   this_object);
	* scan = SHARP_F;
	continue;

      default:			/* Non Marked Headers and Broken Hearts */
      fail:
        outf_fatal
	  ("\nfix_weak_chain_and_hunt_wabbits: Bad Object: 0x%08lx.\n",
	   this_object);
	Microcode_Termination (TERM_INVALID_TYPE_CODE);
	/*NOTREACHED*/
    }
  }
  return;
}

/* What did you expect from opera, a happy ending? */

/* -*-C-*-

$Id: purutl.c,v 9.54.2.7 2006/08/30 19:20:55 cph Exp $

Copyright 1987,1988,1989,1990,1991,1992 Massachusetts Institute of Technology
Copyright 1993,1996,2000,2001,2005,2006 Massachusetts Institute of Technology

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

/* Pure/constant space utilities. */

#include "scheme.h"
#include "prims.h"
#include "gccode.h"

#define ADDRESS_IN_BLOCK_PURE_P(addr, block)				\
  (((block) != 0)							\
   && ((addr) >= (constant_space_block_pure_start (block)))		\
   && ((addr) < (constant_space_block_pure_end (block))))

typedef struct
{
  gc_ctx_t gc_ctx;
  SCHEME_OBJECT * old_start;
  SCHEME_OBJECT * old_end;
  SCHEME_OBJECT * new_start;
} ud_ctx_t;

#define CTX_OLD_START(ctx) (((ud_ctx_t *) (ctx))->old_start)
#define CTX_OLD_END(ctx) (((ud_ctx_t *) (ctx))->old_end)
#define CTX_NEW_START(ctx) (((ud_ctx_t *) (ctx))->new_start)

static gc_table_t update_table;
static gc_tuple_handler_t update_tuple;
static gc_vector_handler_t update_vector;
static gc_object_handler_t update_cc_entry;
static gc_object_handler_t update_weak_pair;

static unsigned long object_length (SCHEME_OBJECT);
static void update_pointers
  (SCHEME_OBJECT *, SCHEME_OBJECT *, SCHEME_OBJECT *, SCHEME_OBJECT *,
   SCHEME_OBJECT *);

DEFINE_PRIMITIVE ("PRIMITIVE-IMPURIFY", Prim_impurify, 1, 1,
		  "(OBJECT)\n\
Remove OBJECT from pure space, allowing it to be modified.")
{
  PRIMITIVE_HEADER (1);
  {
    SCHEME_OBJECT object = (ARG_REF (1));
    SCHEME_OBJECT normalized;
    SCHEME_OBJECT * old_start;
    SCHEME_OBJECT * old_block;
    unsigned long n_words;
    SCHEME_OBJECT * old_end;
    SCHEME_OBJECT * new_block;
    SCHEME_OBJECT * new_start;

    switch (gc_ptr_type (object))
      {
      case GC_POINTER_NORMAL:
	normalized = object;
	break;

      case GC_POINTER_COMPILED:
#ifdef CC_SUPPORT_P
	normalized = (cc_entry_to_block (object));
	break;
#endif

      default:
	PRIMITIVE_RETURN (object);
      }
    old_start = (OBJECT_ADDRESS (normalized));
    old_block = (find_constant_space_block (old_start));
    if (!ADDRESS_IN_BLOCK_PURE_P (old_start, old_block))
      PRIMITIVE_RETURN (object);

    n_words = (object_length (normalized));
    old_end = (old_start + n_words);

    /* If there's nothing else in the block's pure area, or if there's
       no room to copy the object, make the entire block impure.  */
    if (((old_start == (constant_space_block_pure_start (old_block)))
	 && (old_end == (constant_space_block_pure_end (old_block))))
	|| ((constant_alloc_next + n_words) > constant_end))
      {
	(*old_block) = (MAKE_OBJECT (TC_MANIFEST_SPECIAL_NM_VECTOR, 1));
	PRIMITIVE_RETURN (object);
      }

    /* Append a copy of the object to the last constant block.  */
    new_block = (constant_alloc_next - 1);
    new_block -= (OBJECT_DATUM (*new_block));
    new_start = (constant_space_block_constant_end (new_block));
    {
      SCHEME_OBJECT * new_scan = new_start;
      SCHEME_OBJECT * old_scan = old_start;
      unsigned long m = ((OBJECT_DATUM (new_block[1])) + n_words);
      while (old_scan < old_end)
	(*new_scan++) = (*old_scan++);
      (new_block[1]) = (MAKE_OBJECT (PURE_PART, m));
      (*new_scan++) = (MAKE_OBJECT (TC_MANIFEST_SPECIAL_NM_VECTOR, 1));
      (*new_scan++) = (MAKE_OBJECT (END_OF_BLOCK, m));
      constant_alloc_next = new_scan;
    }
    (*old_start) = (MAKE_OBJECT (TC_MANIFEST_NM_VECTOR, (n_words - 1)));

    /* Relocate all pointers to the object.  */
    ENTER_CRITICAL_SECTION ("impurify");
    update_pointers (active_heap_start, Free, old_start, old_end, new_start);
    update_pointers (stack_pointer, stack_end, old_start, old_end, new_start);
    update_pointers
      (constant_start, constant_alloc_next, old_start, old_end, new_start);
    EXIT_CRITICAL_SECTION ({});

#ifdef CC_SUPPORT_P
    if (normalized != object)
      PRIMITIVE_RETURN (CC_ENTRY_NEW_BLOCK (object, new_start, old_start));
#endif

    PRIMITIVE_RETURN (OBJECT_NEW_ADDRESS (object, new_start));
  }
}

static unsigned long
object_length (SCHEME_OBJECT object)
{
  /* Calculate size of object to be "impurified".  */
  switch (GC_TYPE (object))
    {
    case GC_NON_POINTER: return (0);
    case GC_CELL: return (1);
    case GC_PAIR: return (2);
    case GC_TRIPLE: return (3);
    case GC_QUADRUPLE: return (4);

    case GC_VECTOR:
      return (1 + (VECTOR_LENGTH (object)));

#ifdef CC_SUPPORT_P
    case GC_COMPILED:
      return (1 + (VECTOR_LENGTH (cc_entry_to_block (object))));
#endif

    case GC_SPECIAL:
      if (REFERENCE_TRAP_P (object))
	return (((OBJECT_DATUM (object)) <= TRAP_MAX_IMMEDIATE) ? 0 : 2);
      else
	{
	  error_wrong_type_arg (1);
	  /*NOTREACHED*/
	  return (0);
	}

    default:
      gc_bad_type (object, 0);
      error_wrong_type_arg (1);
      /*NOTREACHED*/
      return (0);
    }
}

static void
update_pointers (SCHEME_OBJECT * scan,
		 SCHEME_OBJECT * to,
		 SCHEME_OBJECT * old_start,
		 SCHEME_OBJECT * old_end,
		 SCHEME_OBJECT * new_start)
{
  static bool initialized_p = false;
  ud_ctx_t ctx0;
  gc_ctx_t * ctx = ((gc_ctx_t *) (&ctx0));

  if (!initialized_p)
    {
      initialize_gc_table ((&update_table),
			   update_tuple,
			   update_vector,
			   update_cc_entry,
			   update_weak_pair,
			   gc_precheck_from);
      initialized_p = true;
    }

  (GCTX_TABLE (ctx)) = (&update_table);
  (GCTX_PTO (ctx)) = 0;
  (GCTX_PTO_END (ctx)) = 0;
  (GCTX_FROM_START (ctx)) = 0;
  (GCTX_FROM_END (ctx)) = 0;
  (CTX_OLD_START (ctx)) = old_start;
  (CTX_OLD_END (ctx)) = old_end;
  (CTX_NEW_START (ctx)) = new_start;
  run_gc_loop (scan, (&to), ctx);
}

static
DEFINE_GC_TUPLE_HANDLER (update_tuple)
{
  return
    (((OBJECT_ADDRESS (tuple)) == (CTX_OLD_START (ctx)))
     ? (OBJECT_NEW_ADDRESS (tuple, (CTX_NEW_START (ctx))))
     : tuple);
}

static
DEFINE_GC_VECTOR_HANDLER (update_vector)
{
  return
    (((OBJECT_ADDRESS (vector)) == (CTX_OLD_START (ctx)))
     ? (OBJECT_NEW_ADDRESS (vector, (CTX_NEW_START (ctx))))
     : vector);
}

static
DEFINE_GC_OBJECT_HANDLER (update_cc_entry)
{
#ifdef CC_SUPPORT_P
  insn_t * addr = (CC_ENTRY_ADDRESS (object));
  return
    (((addr >= ((insn_t *) (CTX_OLD_START (ctx))))
      && (addr < ((insn_t *) (CTX_OLD_END (ctx)))))
     ? (CC_ENTRY_NEW_BLOCK (object,
			    (CTX_NEW_START (ctx)),
			    (CTX_OLD_START (ctx))))
     : object);
#else
  gc_no_cc_support (ctx);
  return (object);
#endif
}

static
DEFINE_GC_OBJECT_HANDLER (update_weak_pair)
{
  return (update_tuple (object, 2, ctx));
}

DEFINE_PRIMITIVE ("CONSTANT?", Prim_constant_p, 1, 1, "(OBJECT)\n\
Returns #T iff OBJECT is in constant space.")
{
  PRIMITIVE_HEADER (1);
  PRIMITIVE_RETURN
    (BOOLEAN_TO_OBJECT (object_in_constant_space_p (ARG_REF (1))));
}

DEFINE_PRIMITIVE ("PURE?", Prim_pure_p, 1, 1, "(OBJECT)\n\
Returns #T iff OBJECT is in constant space and is 'pure'.")
{
  PRIMITIVE_HEADER (1);
  PRIMITIVE_RETURN (BOOLEAN_TO_OBJECT (object_pure_p (ARG_REF (1))));
}

bool
object_in_constant_space_p (SCHEME_OBJECT object)
{
  SCHEME_OBJECT * address = (get_object_address (object));
  return ((address != 0) && (ADDRESS_IN_CONSTANT_P (address)));
}

bool
object_pure_p (SCHEME_OBJECT object)
{
  SCHEME_OBJECT * address = (get_object_address (object));
  return ((address != 0)
	  && (ADDRESS_IN_CONSTANT_P (address))
	  && (pure_test (address)));
}

bool
pure_test (SCHEME_OBJECT * addr)
{
  SCHEME_OBJECT * block = (find_constant_space_block (addr));
  return (ADDRESS_IN_BLOCK_PURE_P (addr, block));
}

SCHEME_OBJECT *
find_constant_space_block (SCHEME_OBJECT * addr)
{
  if ((addr >= constant_start) && (addr < constant_alloc_next))
    {
      SCHEME_OBJECT * p = constant_alloc_next;
      while (p > addr)
	p -= (1 + (OBJECT_DATUM (p[-1])));
      return (p);
    }
  return (0);
}

SCHEME_OBJECT *
constant_space_block_pure_start (SCHEME_OBJECT * block)
{
  return (block + 2);
}

SCHEME_OBJECT *
constant_space_block_pure_end (SCHEME_OBJECT * block)
{
  unsigned long n = (OBJECT_DATUM (block[0]));
  return ((n >= 3) ? (block + (n - 1)) : (block + 2));
}

SCHEME_OBJECT *
constant_space_block_constant_start (SCHEME_OBJECT * block)
{
  unsigned long n = (OBJECT_DATUM (block[0]));
  return ((n >= 3) ? (block + (n + 1)) : (block + 2));
}

SCHEME_OBJECT *
constant_space_block_constant_end (SCHEME_OBJECT * block)
{
  unsigned long m = (OBJECT_DATUM (block[1]));
  return (block + (m - 1));
}

/* copy_to_constant_space takes care of making legal constant space
   blocks.  */

SCHEME_OBJECT *
copy_to_constant_space (SCHEME_OBJECT * source, unsigned long n_words)
{
  SCHEME_OBJECT * result;

  if ((n_words + 6) > (constant_end - constant_alloc_next))
    {
      outf_fatal ("\nInsufficient constant space!\n");
      Microcode_Termination (TERM_NO_SPACE);
    }
  (*constant_alloc_next++) = (MAKE_OBJECT (TC_MANIFEST_SPECIAL_NM_VECTOR, 3));
  (*constant_alloc_next++) = (MAKE_OBJECT (PURE_PART, (n_words + 5)));
  (*constant_alloc_next++) = (MAKE_OBJECT (TC_MANIFEST_SPECIAL_NM_VECTOR, 1));
  (*constant_alloc_next++) = (MAKE_OBJECT (CONSTANT_PART, 3));
  result = constant_alloc_next;
  {
    SCHEME_OBJECT * limit = (constant_alloc_next + n_words);
    while (constant_alloc_next < limit)
      (*constant_alloc_next++) = (*source++);
  }
  (*constant_alloc_next++) = (MAKE_OBJECT (TC_MANIFEST_SPECIAL_NM_VECTOR, 1));
  (*constant_alloc_next++) = (MAKE_OBJECT (END_OF_BLOCK, (n_words + 5)));
  return (result);
}

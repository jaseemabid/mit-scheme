/* -*-C-*-

$Id: purify.c,v 9.65.2.6 2006/08/29 04:44:32 cph Exp $

Copyright 1986,1987,1988,1989,1990,1991 Massachusetts Institute of Technology
Copyright 1992,1993,1995,1997,2000,2001 Massachusetts Institute of Technology
Copyright 2002,2005,2006 Massachusetts Institute of Technology

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

/* Copy objects into constant/pure space.  */

#include "scheme.h"
#include "prims.h"
#include "gccode.h"

typedef struct
{
  gc_ctx_t gc_ctx;
  bool pure_p;
} pl_ctx_t;

#define CTX_PURE_P(ctx) (((pl_ctx_t *) (ctx))->pure_p)

static gc_table_t purify_table;

static void purify (SCHEME_OBJECT, bool);
static void purify_loop (SCHEME_OBJECT *, SCHEME_OBJECT **, bool);
static gc_handler_t handle_linkage_section;
static gc_handler_t handle_manifest_closure;
static gc_handler_t handle_reference_trap;
static gc_handler_t handle_symbol;
static gc_handler_t handle_cc_entry;
static gc_handler_t handle_cc_block;
static gc_handler_t handle_environment;
static gc_tuple_handler_t purify_tuple;
static gc_vector_handler_t purify_vector;
static gc_object_handler_t purify_cc_entry;
static gc_object_handler_t purify_weak_pair;
static void purify_symbol_name (SCHEME_OBJECT, gc_ctx_t *);
static gc_precheck_from_t precheck_from;

/* Description of the algorithm for PURIFY:

   Purify increases the size of constant space at the expense of both
   heaps.  A GC-like relocation is performed with the object being
   purified as the root.  The object is copied and relocated from the
   high heap to the area adjacent to constant space.  Then the GC is
   finished after changing the end of constant-space marker.

   In order to make a pure object, the copy process proceeds in two
   halves.  During the first half (which collects the pure part)
   Compiled Code, Environments, Symbols, and Variables (i.e.  things
   whose contents change) are NOT copied.  Then a header is put down
   indicating constant (not pure) area, and then they ARE copied.

   The constant area contains a contiguous set of blocks of the
   following format:

  >>Heap above here<<

                   . (direction of growth)
                   .  ^
                   . / \
                   .  |
                   .  |
        |----------------------|---
        | END   | Total Size M |   \ Where END   = TC_FIXNUM
        |----------------------|    \      SNMH  = TC_MANIFEST_SPECIAL_...
        | SNMH  |      1       |    |      CONST = TC_CONSTANT
        |----------------------|    |      PURE  = TC_NULL
        |                      |    |
        |                      |    |
        |    CONSTANT AREA     |    |
        |                      |    |
        |                      |     \
     ---|----------------------|      >  M
    /   | CONST | Pure Size N  |     /
   /    |----------------------|    |
   |    | SNMH  |      1       |    |
   |    |----------------------|    |
   |    |                      |    |
N <     |                      |    |
   |    |      PURE AREA       |    |
   |    |                      |    |
   |    |                      |    |
   \    |----------------------|    /
    \   | PURE  | Total Size M |   /
     ---|----------------------|---
        | SNMH  | Pure Size N  |
        |----------------------|

  >>start of constant area (constant_start).

*/

DEFINE_PRIMITIVE ("PRIMITIVE-PURIFY", Prim_primitive_purify, 3, 3,
		  "(OBJECT PURE? SAFETY-MARGIN)\n\
Copy OBJECT from the heap into constant space (or pure space, if PURE?\n\
is true).  This requires a spare heap, and is tricky to use -- it\n\
should only be used through the wrapper provided in the Scheme runtime\n\
system.")
{
  SCHEME_OBJECT object;
  bool purify_mode;
  SCHEME_OBJECT daemon;
  PRIMITIVE_HEADER (3);

  canonicalize_primitive_context ();

  if (STACK_OVERFLOWED_P ())
    stack_death ("PURIFY");

  /* Purify only works from the high heap.  If in the low heap, tell
     the runtime system.  */
  if (active_heap_start < inactive_heap_start)
    PRIMITIVE_RETURN (SHARP_F);

  object = (ARG_REF (1));
  purify_mode = (BOOLEAN_ARG (2));
  active_heap_reserved = (ARG_HEAP_RESERVED (3));

  POP_PRIMITIVE_FRAME (3);

  ENTER_CRITICAL_SECTION ("purify");
  purify (object, purify_mode);

 Will_Push (CONTINUATION_SIZE);
  SET_RC (RC_NORMAL_GC_DONE);
  SET_EXP (cons (SHARP_T, (ULONG_TO_FIXNUM (HEAP_AVAILABLE))));
  SAVE_CONT ();
 Pushed ();

  RENAME_CRITICAL_SECTION ("purify daemon");
  daemon = (VECTOR_REF (fixed_objects, GC_DAEMON));
  if (daemon == SHARP_F)
    PRIMITIVE_ABORT (PRIM_POP_RETURN);
    /*NOTREACHED*/

 Will_Push (2);
  STACK_PUSH (daemon);
  PUSH_APPLY_FRAME_HEADER (0);
 Pushed ();
  PRIMITIVE_ABORT (PRIM_APPLY);
  /*NOTREACHED*/
  return (UNSPECIFIC);
}

static void
purify (SCHEME_OBJECT object, bool pure_p)
{
  SCHEME_OBJECT * start_copy;
  SCHEME_OBJECT * start_pure;
  unsigned long pure_length;
  unsigned long total_length;

  if (STACK_OVERFLOWED_P ())
    stack_death ("PURIFY");

  constant_end = constant_alloc_next;

  start_copy = constant_alloc_next;
  (*constant_alloc_next++) = SHARP_F;

  start_pure = constant_alloc_next;
  (*constant_alloc_next++) = object;

  if (pure_p)
    purify_loop (start_pure, (&constant_alloc_next), true);

  pure_length = ((constant_alloc_next + 2) - start_pure);
  (*start_copy) = (MAKE_OBJECT (TC_MANIFEST_SPECIAL_NM_VECTOR, pure_length));
  (*constant_alloc_next++) = (MAKE_OBJECT (TC_MANIFEST_SPECIAL_NM_VECTOR, 1));
  (*constant_alloc_next++) = (MAKE_OBJECT (CONSTANT_PART, pure_length));

  constant_end = constant_alloc_next;

  if (pure_p)
    purify_loop (start_pure, (&constant_alloc_next), false);
  else
    {
      initialize_weak_chain ();
      gc_loop (start_copy, (&constant_alloc_next), (&constant_alloc_next));
      update_weak_pointers ();
    }

  total_length = ((constant_alloc_next + 2) - start_pure);
  (*start_pure) = (MAKE_OBJECT (PURE_PART, total_length));
  (*constant_alloc_next++) = (MAKE_OBJECT (TC_MANIFEST_SPECIAL_NM_VECTOR, 1));
  (*constant_alloc_next++) = (MAKE_OBJECT (END_OF_BLOCK, total_length));

  PRESERVE_OLD_SPACE_LIMITS ();
  if (!update_allocator_parameters (0))
    gc_death (TERM_NO_SPACE, 0, 0, "object too large");

  RESET_HEAP_ALLOC_LIMIT ();
  garbage_collect ();
}

static void
purify_loop (SCHEME_OBJECT * scan, SCHEME_OBJECT ** pto, bool pure_p)
{
  static bool initialized_p = false;
  pl_ctx_t ctx0;
  gc_ctx_t * ctx = ((gc_ctx_t *) (&ctx0));

  if (!initialized_p)
    {
      initialize_gc_table ((&purify_table),
			   purify_tuple,
			   purify_vector,
			   purify_cc_entry,
			   purify_weak_pair,
			   precheck_from);

      (GCT_ENTRY ((&purify_table), TC_LINKAGE_SECTION))
	= handle_linkage_section;
      (GCT_ENTRY ((&purify_table), TC_MANIFEST_CLOSURE))
	= handle_manifest_closure;
      (GCT_ENTRY ((&purify_table), TC_REFERENCE_TRAP)) = handle_reference_trap;
      (GCT_ENTRY ((&purify_table), TC_INTERNED_SYMBOL)) = handle_symbol;
      (GCT_ENTRY ((&purify_table), TC_UNINTERNED_SYMBOL)) = handle_symbol;
      (GCT_ENTRY ((&purify_table), TC_COMPILED_ENTRY)) = handle_cc_entry;
      (GCT_ENTRY ((&purify_table), TC_COMPILED_CODE_BLOCK)) = handle_cc_block;
      (GCT_ENTRY ((&purify_table), TC_ENVIRONMENT)) = handle_environment;
      initialized_p = true;
    }

  (GCTX_TABLE (ctx)) = (&purify_table);
  (GCTX_PTO (ctx)) = pto;
  (CTX_PURE_P (ctx)) = pure_p;
  run_gc_loop (scan, pto, ctx);
}

static
DEFINE_GC_HANDLER (handle_linkage_section)
{
  if (CTX_PURE_P (ctx))
    gc_death (TERM_COMPILER_DEATH, (GCTX_SCAN (ctx)), (GCTX_PTO (ctx)),
	      "linkage section in pure area");
  return (gc_handle_linkage_section (scan, object, ctx));
}

static
DEFINE_GC_HANDLER (handle_manifest_closure)
{
  if (CTX_PURE_P (ctx))
    gc_death (TERM_COMPILER_DEATH, (GCTX_SCAN (ctx)), (GCTX_PTO (ctx)),
	      "compiled closure in pure area");
  return (gc_handle_manifest_closure (scan, object, ctx));
}

static
DEFINE_GC_HANDLER (handle_reference_trap)
{
  return
    ((CTX_PURE_P (ctx))
     ? (scan + 1)
     : (gc_handle_reference_trap (scan, object, ctx)));
}

static
DEFINE_GC_HANDLER (handle_symbol)
{
  if (CTX_PURE_P (ctx))
    {
      purify_symbol_name (object, ctx);
      return (scan + 1);
    }
  else
    return (gc_handle_pair (scan, object, ctx));
}

static
DEFINE_GC_HANDLER (handle_cc_entry)
{
  return
    ((CTX_PURE_P (ctx))
     ? (scan + 1)
     : (gc_handle_cc_entry (scan, object, ctx)));
}

static
DEFINE_GC_HANDLER (handle_cc_block)
{
  return
    ((CTX_PURE_P (ctx))
     ? (scan + 1)
     : (gc_handle_aligned_vector (scan, object, ctx)));
}

static
DEFINE_GC_HANDLER (handle_environment)
{
  return
    ((CTX_PURE_P (ctx))
     ? (scan + 1)
     : (gc_handle_unaligned_vector (scan, object, ctx)));
}

static
DEFINE_GC_TUPLE_HANDLER (purify_tuple)
{
  SCHEME_OBJECT * from = (OBJECT_ADDRESS (tuple));
  SCHEME_OBJECT * address = (GC_PRECHECK_FROM (from, ctx));
  return
    (OBJECT_NEW_ADDRESS (tuple,
			 ((address != 0)
			  ? address
			  : (gc_transport_words (from,
						 n_words,
						 false,
						 ctx)))));
}

static
DEFINE_GC_VECTOR_HANDLER (purify_vector)
{
  SCHEME_OBJECT * from = (OBJECT_ADDRESS (vector));
  SCHEME_OBJECT * address = (GC_PRECHECK_FROM (from, ctx));
  return
    (OBJECT_NEW_ADDRESS (vector,
			 ((address != 0)
			  ? address
			  : (gc_transport_words (from,
						 (1 + (OBJECT_DATUM (*from))),
						 align_p,
						 ctx)))));
}

static
DEFINE_GC_OBJECT_HANDLER (purify_cc_entry)
{
#ifdef CC_SUPPORT_P
  SCHEME_OBJECT old_block = (cc_entry_to_block (object));
  return
    (CC_ENTRY_NEW_BLOCK
     (object,
      (OBJECT_ADDRESS (purify_vector (old_block, true, ctx))),
      (OBJECT_ADDRESS (old_block))));
#else
  gc_death (TERM_EXIT, (GCTX_SCAN (ctx)), (GCTX_PTO (ctx)),
	    "No native-code support.");
  return (object);
#endif
}

static
DEFINE_GC_OBJECT_HANDLER (purify_weak_pair)
{
  SCHEME_OBJECT * new_address
    = (GC_PRECHECK_FROM ((OBJECT_ADDRESS (object)), ctx));
  return
    ((new_address != 0)
     ? (OBJECT_NEW_ADDRESS (object, new_address))
     : (gc_transport_weak_pair (object, ctx)));
}

static void
purify_symbol_name (SCHEME_OBJECT symbol, gc_ctx_t * ctx)
{
  MEMORY_SET (symbol, SYMBOL_NAME,
	      (purify_vector ((MEMORY_REF (symbol, SYMBOL_NAME)),
			      false,
			      ctx)));
}

static SCHEME_OBJECT *
precheck_from (SCHEME_OBJECT * from, gc_ctx_t * ctx)
{
  return
    (((from >= constant_start) && (from < constant_alloc_next))
     ? from
     : (BROKEN_HEART_P (*from))
     ? (OBJECT_ADDRESS (*from))
     : 0);
}

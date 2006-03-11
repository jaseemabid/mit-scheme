/* -*-C-*-

$Id: gcloop.c,v 9.51.2.4 2006/03/11 02:34:23 cph Exp $

Copyright 1986,1987,1988,1989,1990,1991 Massachusetts Institute of Technology
Copyright 1992,1993,2000,2001,2005 Massachusetts Institute of Technology

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

/* Inner loop of garbage collector.  */

#include "scheme.h"
#include "gccode.h"

#ifdef ENABLE_GC_DEBUGGING_TOOLS
#  ifndef GC_SCAN_HISTORY_SIZE
#    define GC_SCAN_HISTORY_SIZE 1024
#  endif
#  define INITIALIZE_GC_HISTORY initialize_gc_history
#  define HANDLE_GC_TRAP handle_gc_trap
#else
#  define INITIALIZE_GC_HISTORY() do {} while (0)
#  define HANDLE_GC_TRAP(scan, pto, object) do {} while (0)
#endif

#ifndef READ_REFERENCE_OBJECT
#  define READ_REFERENCE_OBJECT(addr)					\
     (MAKE_POINTER_OBJECT (TC_HUNK3, (* ((SCHEME_OBJECT **) (addr)))))
#  define WRITE_REFERENCE_OBJECT(ref, addr)				\
     ((* ((SCHEME_OBJECT **) (addr))) = (OBJECT_ADDRESS (ref)))
#endif

static gc_table_t gc_table;
static gc_tuple_handler_t gc_tuple;
static gc_vector_handler_t gc_vector;
static gc_object_handler_t gc_cc_entry;
static gc_object_handler_t gc_weak_pair;
static SCHEME_OBJECT * precheck_from (SCHEME_OBJECT *, gc_ctx_t *);

#define SIMPLE_HANDLER(name)						\
  (GCT_ENTRY (table, i)) = name;					\
  break

void
initialize_gc_table (gc_table_t * table,
		     gc_tuple_handler_t * tuple_handler,
		     gc_vector_handler_t * vector_handler,
		     gc_object_handler_t * cc_entry_handler,
		     gc_object_handler_t * weak_pair_handler)
{
  unsigned int i;
  for (i = 0; (i < N_TYPE_CODES); i += 1)
    switch (gc_type_map[i])
      {
      case GC_NON_POINTER: SIMPLE_HANDLER (gc_handle_non_pointer);
      case GC_CELL:        SIMPLE_HANDLER (gc_handle_cell);
      case GC_PAIR:        SIMPLE_HANDLER (gc_handle_pair);
      case GC_TRIPLE:      SIMPLE_HANDLER (gc_handle_triple);
      case GC_QUADRUPLE:   SIMPLE_HANDLER (gc_handle_quadruple);
      case GC_COMPILED:    SIMPLE_HANDLER (gc_handle_cc_entry);
      case GC_UNDEFINED:   SIMPLE_HANDLER (gc_handle_undefined);

      case GC_VECTOR:
	(GCT_ENTRY (table, i))
	  = (((i == TC_COMPILED_CODE_BLOCK) || (i == TC_BIG_FLONUM))
	     ? gc_handle_aligned_vector
	     : gc_handle_unaligned_vector);
	break;

      case GC_SPECIAL:
	switch (i)
	  {
	  case TC_BROKEN_HEART:
	    SIMPLE_HANDLER (gc_handle_broken_heart);

	  case TC_REFERENCE_TRAP:
	    SIMPLE_HANDLER (gc_handle_reference_trap);

	  case TC_LINKAGE_SECTION:
	    SIMPLE_HANDLER (gc_handle_linkage_section);

	  case TC_MANIFEST_CLOSURE:
	    SIMPLE_HANDLER (gc_handle_manifest_closure);

	  case TC_MANIFEST_NM_VECTOR:
	  case TC_MANIFEST_SPECIAL_NM_VECTOR:
	    SIMPLE_HANDLER (gc_handle_nmv);

	  default:
	    outf_fatal ("\nunknown GC special type: %#x\n", i);
	    termination_init_error ();
	    break;
	  }
	break;
      }
  (GCT_TUPLE (table)) = tuple_handler;
  (GCT_VECTOR (table)) = vector_handler;
  (GCT_CC_ENTRY (table)) = cc_entry_handler;
  (GCT_WEAK_PAIR (table)) = weak_pair_handler;
}

void
run_gc_loop (SCHEME_OBJECT * scan, SCHEME_OBJECT ** pend, gc_ctx_t * ctx)
{
  INITIALIZE_GC_HISTORY ();
  while (scan < (*pend))
    {
      SCHEME_OBJECT object = (*scan);
      HANDLE_GC_TRAP (scan, (GCTX_PTO (ctx)), object);
      (GCTX_SCAN (ctx)) = scan;
      (GCTX_OBJECT (ctx)) = object;
      scan
	= ((* (GCT_ENTRY ((GCTX_TABLE (ctx)), (OBJECT_TYPE (object)))))
	   (scan, object, ctx));
    }
}

DEFINE_GC_HANDLER (gc_handle_non_pointer)
{
  return (scan + 1);
}

DEFINE_GC_HANDLER (gc_handle_cell)
{
  (*scan) = (GC_HANDLE_TUPLE (object, 1, ctx));
  return (scan + 1);
}

DEFINE_GC_HANDLER (gc_handle_pair)
{
  (*scan) = (GC_HANDLE_TUPLE (object, 2, ctx));
  return (scan + 1);
}

DEFINE_GC_HANDLER (gc_handle_triple)
{
  (*scan) = (GC_HANDLE_TUPLE (object, 3, ctx));
  return (scan + 1);
}

DEFINE_GC_HANDLER (gc_handle_quadruple)
{
  (*scan) = (GC_HANDLE_TUPLE (object, 4, ctx));
  return (scan + 1);
}

DEFINE_GC_HANDLER (gc_handle_cc_entry)
{
  (*scan) = (GC_HANDLE_CC_ENTRY (object, ctx));
  return (scan + 1);
}

DEFINE_GC_HANDLER (gc_handle_aligned_vector)
{
  (*scan) = (GC_HANDLE_VECTOR (object, true, ctx));
  return (scan + 1);
}

DEFINE_GC_HANDLER (gc_handle_unaligned_vector)
{
  (*scan) = (GC_HANDLE_VECTOR (object, false, ctx));
  return (scan + 1);
}

DEFINE_GC_HANDLER (gc_handle_broken_heart)
{
  gc_death (TERM_BROKEN_HEART, (GCTX_SCAN (ctx)), (GCTX_PTO (ctx)),
	    "broken heart in scan: %#lx", object);
  /*NOTREACHED*/
  return (scan);
}

DEFINE_GC_HANDLER (gc_handle_nmv)
{
  return (scan + 1 + (OBJECT_DATUM (object)));
}

DEFINE_GC_HANDLER (gc_handle_reference_trap)
{
  (*scan) = (((OBJECT_DATUM (object)) <= TRAP_MAX_IMMEDIATE)
	     ? object
	     : (GC_HANDLE_TUPLE (object, 2, ctx)));
  return (scan + 1);
}

DEFINE_GC_HANDLER (gc_handle_linkage_section)
{
  unsigned long count = (linkage_section_count (object));
  scan += 1;
  switch (linkage_section_type (object))
    {
    case LINKAGE_SECTION_TYPE_REFERENCE:
    case LINKAGE_SECTION_TYPE_ASSIGNMENT:
      while (count > 0)
	{
	  WRITE_REFERENCE_OBJECT
	    ((GC_HANDLE_TUPLE ((READ_REFERENCE_OBJECT (scan)), 3, ctx)),
	     scan);
	  scan += 1;
	  count -= 1;
	}
      break;

    case LINKAGE_SECTION_TYPE_OPERATOR:
    case LINKAGE_SECTION_TYPE_GLOBAL_OPERATOR:
      {
	DECLARE_RELOCATION_REFERENCE (ref);
	START_OPERATOR_RELOCATION (scan, ref);
	while (count > 0)
	  {
	    write_uuo_target
	      ((GC_HANDLE_CC_ENTRY ((READ_UUO_TARGET (scan, ref)), ctx)),
	       scan);
	    scan += UUO_LINK_SIZE;
	    count -= 1;
	  }
	END_OPERATOR_RELOCATION (scan, ref);
      }
      break;

    default:
      gc_death (TERM_EXIT, (GCTX_SCAN (ctx)), (GCTX_PTO (ctx)),
		"Unknown linkage-section type.");
      break;
    }
  return (scan);
}

DEFINE_GC_HANDLER (gc_handle_manifest_closure)
{
#ifdef EMBEDDED_CLOSURE_ADDRS_P
  DECLARE_RELOCATION_REFERENCE (ref);
  START_CLOSURE_RELOCATION (scan, ref);
  scan += 1;
  {
    insn_t * start = (compiled_closure_start (scan));
    unsigned long count = (compiled_closure_count (scan));
    while (count > 0)
      {
	write_compiled_closure_target
	  ((GC_HANDLE_CC_ENTRY ((READ_COMPILED_CLOSURE_TARGET (start, ref)),
				ctx)),
	   start);
	start = (compiled_closure_next (start));
	count -= 1;
      }
    scan = (skip_compiled_closure_padding (start));
  }
  END_CLOSURE_RELOCATION (scan, ref);
  return (scan);
#else
  return (compiled_closure_objects (scan + 1));
#endif
}

DEFINE_GC_HANDLER (gc_handle_undefined)
{
  GC_BAD_TYPE (object, (GCTX_SCAN (ctx)), (GCTX_PTO (ctx)));
  return (scan + 1);
}

void
gc_loop (SCHEME_OBJECT * scan, SCHEME_OBJECT ** pend, SCHEME_OBJECT ** pto)
{
  static bool initialized_p = false;
  gc_ctx_t ctx0;
  gc_ctx_t * ctx = (&ctx0);

  if (!initialized_p)
    {
      initialize_gc_table
	((&gc_table), gc_tuple, gc_vector, gc_cc_entry, gc_weak_pair);
      initialized_p = true;
    }
  (GCTX_TABLE (ctx)) = (&gc_table);
  (GCTX_PTO (ctx)) = pto;
  run_gc_loop (scan, pend, ctx);
}

static
DEFINE_GC_TUPLE_HANDLER (gc_tuple)
{
  SCHEME_OBJECT * from = (OBJECT_ADDRESS (tuple));
  SCHEME_OBJECT * new_address = (precheck_from (from, ctx));
  return
    (OBJECT_NEW_ADDRESS (tuple,
			 ((new_address != 0)
			  ? new_address
			  : (gc_transport_words (from,
						 n_words,
						 false,
						 ctx)))));
}

static
DEFINE_GC_VECTOR_HANDLER (gc_vector)
{
  SCHEME_OBJECT * from = (OBJECT_ADDRESS (vector));
  SCHEME_OBJECT * new_address = (precheck_from (from, ctx));
  return
    (OBJECT_NEW_ADDRESS (vector,
			 ((new_address != 0)
			  ? new_address
			  : (gc_transport_words (from,
						 (1 + (OBJECT_DATUM (*from))),
						 align_p,
						 ctx)))));
}

static
DEFINE_GC_OBJECT_HANDLER (gc_cc_entry)
{
  SCHEME_OBJECT old_block = (cc_entry_to_block (object));
  SCHEME_OBJECT new_block = (GC_HANDLE_VECTOR (old_block, true, ctx));
  return (CC_ENTRY_NEW_BLOCK (object,
			      (OBJECT_ADDRESS (new_block)),
			      (OBJECT_ADDRESS (old_block))));
}

static
DEFINE_GC_OBJECT_HANDLER (gc_weak_pair)
{
  SCHEME_OBJECT * new_address
    = (precheck_from ((OBJECT_ADDRESS (object)), ctx));
  return ((new_address != 0)
	  ? (OBJECT_NEW_ADDRESS (object, new_address))
	  : (gc_transport_weak_pair (object, ctx)));
}

static SCHEME_OBJECT *
precheck_from (SCHEME_OBJECT * from, gc_ctx_t * ctx)
{
#ifdef ENABLE_GC_DEBUGGING_TOOLS
  if (!ADDRESS_IN_MEMORY_BLOCK_P (from))
    gc_death (TERM_EXIT, (GCTX_SCAN (ctx)), (GCTX_PTO (ctx)),
	      "out of range pointer: %#lx", ((unsigned long) from));
#endif
  return
    ((!ADDRESS_IN_OLD_SPACE_P (from))
     ? from
     : (BROKEN_HEART_P (*from))
     ? (OBJECT_ADDRESS (*from))
     : 0);
}

SCHEME_OBJECT *
gc_transport_words (SCHEME_OBJECT * from,
		    unsigned long n_words,
		    bool align_p,
		    gc_ctx_t * ctx)
{
  SCHEME_OBJECT * to = (* (GCTX_PTO (ctx)));
  if (align_p)
    ALIGN_FLOAT (to);
#ifdef ENABLE_GC_DEBUGGING_TOOLS
  {
    SCHEME_OBJECT * end = (to + n_words);
    if (! ((ADDRESS_IN_ACTIVE_HEAP_P (to))
	   ? (ADDRESS_IN_ACTIVE_HEAP_P (end))
	   : (to == constant_alloc_next)))
      gc_death (TERM_EXIT, (GCTX_SCAN (ctx)), (GCTX_PTO (ctx)),
		"block overflows target space: %#lx %#lx",
		((unsigned long) to),
		((unsigned long) end));
  }
  if (n_words > 0x10000)
    {
      outf_error ("\nWarning: copying large block: %lu\n", n_words);
      outf_flush_error ();
    }
#endif
  {
    SCHEME_OBJECT * scan_to = to;
    SCHEME_OBJECT * scan_from = from;
    while (n_words > 0)
      {
	DEBUG_TRANSPORT_ONE_WORD ((GCTX_OBJECT (ctx)), scan_from);
	(*scan_to++) = (*scan_from++);
	n_words -= 1;
      }
    (* (GCTX_PTO (ctx))) = scan_to;
  }
  (*from) = (MAKE_BROKEN_HEART (to));
  return (to);
}

SCHEME_OBJECT
gc_transport_weak_pair (SCHEME_OBJECT pair, gc_ctx_t * ctx)
{
  SCHEME_OBJECT * old_addr = (OBJECT_ADDRESS (pair));
  SCHEME_OBJECT * new_addr = (gc_transport_words (old_addr, 2, false, ctx));
  note_weak_pair (pair, new_addr);
  return (OBJECT_NEW_ADDRESS (pair, new_addr));
}

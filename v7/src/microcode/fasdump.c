/* -*-C-*-

$Id: fasdump.c,v 9.68.2.8 2006/08/30 18:54:29 cph Exp $

Copyright 1986,1987,1988,1989,1990,1991 Massachusetts Institute of Technology
Copyright 1992,1993,1996,1997,2000,2001 Massachusetts Institute of Technology
Copyright 2005,2006 Massachusetts Institute of Technology

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

/* This file contains code for fasdump and dump-band. */

#include "scheme.h"
#include "prims.h"
#include "osscheme.h"
#include "osio.h"
#include "osfile.h"
#include "osfs.h"
#define In_Fasdump
#include "gccode.h"
#include "trap.h"
#include "lookup.h"
#include "fasl.h"
#include <setjmp.h>

typedef enum { FE_ERROR, FE_DUMP, FE_DROP_CC } env_mode_t;

typedef struct
{
  gc_ctx_t gc_ctx;
  SCHEME_OBJECT ** pfix;
  env_mode_t mode;
  prim_renumber_t * pr;
  bool cc_seen_p;
} fd_ctx_t;

#define CTX_PFIX(ctx) (((fd_ctx_t *) (ctx))->pfix)
#define CTX_MODE(ctx) (((fd_ctx_t *) (ctx))->mode)
#define CTX_PR(ctx) (((fd_ctx_t *) (ctx))->pr)
#define CTX_CC_SEEN_P(ctx) (((fd_ctx_t *) (ctx))->cc_seen_p)

static jmp_buf fasdump_loop_env;
static long fasdump_loop_result;

static gc_table_t fasdump_table;
static gc_handler_t handle_primitive;
static gc_handler_t handle_manifest_closure;
static gc_handler_t handle_linkage_section;
static gc_handler_t handle_symbol;
static gc_handler_t handle_broken_heart;
static gc_handler_t handle_variable;
static gc_handler_t handle_environment;

static long fasdump_loop
  (env_mode_t, prim_renumber_t *, bool *, SCHEME_OBJECT **,
   SCHEME_OBJECT **, SCHEME_OBJECT **, SCHEME_OBJECT *);

static gc_tuple_handler_t fasdump_tuple;
static gc_vector_handler_t fasdump_vector;
static gc_object_handler_t fasdump_cc_entry;
static gc_object_handler_t fasdump_weak_pair;

static SCHEME_OBJECT * fasdump_transport_words
  (SCHEME_OBJECT *, unsigned long, bool, gc_ctx_t *);

static void initialize_object_fasl_header (fasl_header_t *);
static void initialize_band_fasl_header (fasl_header_t *);
static void initialize_fasl_header_1 (fasl_header_t *);
static void save_fasl_header_cc_info (fasl_header_t *, bool);
static bool write_fasl_file
  (fasl_header_t *, SCHEME_OBJECT *, fasl_file_handle_t);

/* FASDUMP:

   In order to dump an object it must be traced (as in a garbage
   collection), but with some significant differences.  First, the
   copy must have the global value cell of symbols set to UNBOUND.
   Second, and worse, all the broken hearts created during the process
   must be restored to their original values.  This last is done by
   growing the copy of the object in the bottom of spare heap, keeping
   track of the locations of broken hearts and original contents at
   the top of the spare heap.  */

DEFINE_PRIMITIVE ("PRIMITIVE-FASDUMP", Prim_prim_fasdump, 3, 3,
		  "(OBJECT NAMESTRING FLAG)\n\
Writes a binary representation of OBJECT to the file NAMESTRING.\n\
Returns #T if the operation is successful, or #F otherwise.\n\
\n\
FLAG specifies how to handle environment objects that OBJECT points\n\
to: #F means generate an error; #T means write them as ordinary\n\
objects; any other value is like #F except that environments pointed\n\
at by compiled code are ignored (and discarded).")
{
  const char * filename;
  fasl_file_handle_t handle;
  fasl_header_t h;
  bool cc_seen_p;
  SCHEME_OBJECT * to;
  SCHEME_OBJECT * fix;
  prim_renumber_t * pr;
  long code;
  SCHEME_OBJECT * prim_table_start;
  PRIMITIVE_HEADER (3);

  filename = (STRING_ARG (2));
  if (!open_fasl_output_file (filename, (&handle)))
    error_bad_range_arg (2);

  initialize_object_fasl_header (&h);

  cc_seen_p = false;
  to = inactive_heap_start;
  fix = inactive_heap_end;

  (FASLHDR_HEAP_START (&h)) = to;
  (FASLHDR_ROOT_POINTER (&h)) = to;
  (*to++) = (ARG_REF (1));

  transaction_begin ();
  pr = (make_prim_renumber ());

  code = (fasdump_loop ((((ARG_REF (3)) == SHARP_F)
			 ? FE_ERROR
			 : ((ARG_REF (3)) == SHARP_T)
			 ? FE_DUMP
			 : FE_DROP_CC),
			pr,
			(&cc_seen_p),
			(&to),
			(&fix),
			(&fix),
			(FASLHDR_ROOT_POINTER (&h))));
  if (code != PRIM_DONE)
    {
      transaction_abort ();
      goto done;
    }
  (FASLHDR_HEAP_END (&h)) = to;
  save_fasl_header_cc_info ((&h), cc_seen_p);

  prim_table_start = to;
  (FASLHDR_N_PRIMITIVES (&h)) = (pr->next_code);
  (FASLHDR_PRIMITIVE_TABLE_SIZE (&h))
    = (renumbered_primitives_export_length (pr));
  to += (FASLHDR_PRIMITIVE_TABLE_SIZE (&h));
  if (to > (fix - 2))		/* 2 accounts for fixup to be pushed */
    {
      transaction_abort ();
      code = PRIM_INTERRUPT;
      goto done;
    }
  export_renumbered_primitives (prim_table_start, pr);
  transaction_commit ();

  code
    = ((write_fasl_file ((&h), prim_table_start, handle))
       ? PRIM_DONE
       : PRIM_INTERRUPT);

 done:

  while (fix < inactive_heap_end)
    {
      SCHEME_OBJECT * address = (OBJECT_ADDRESS (*fix++));
      (*address) = (*fix++);
    }

  if (!close_fasl_output_file (handle))
    OS_file_remove (filename);

  if (code == PRIM_DONE)
    PRIMITIVE_RETURN (SHARP_T);
  else if (code == PRIM_INTERRUPT)
    PRIMITIVE_RETURN (SHARP_F);
  else
    {
      signal_error_from_primitive (code);
      /*NOTREACHED*/
      PRIMITIVE_RETURN (0);
    }
}

/* Copy of std_gc_loop, except (a) copies out of constant space into the
   object to be dumped; (b) changes symbols and variables as
   described; (c) keeps track of broken hearts and their original
   contents.  */

static long
fasdump_loop (env_mode_t mode, prim_renumber_t * pr, bool * cc_seen_p,
	      SCHEME_OBJECT ** pto, SCHEME_OBJECT ** pto_end,
	      SCHEME_OBJECT ** pfix, SCHEME_OBJECT * scan)
{
  static bool initialized_p = false;
  fd_ctx_t ctx0;
  gc_ctx_t * ctx = ((gc_ctx_t *) (&ctx0));

  if (!initialized_p)
    {
      initialize_gc_table ((&fasdump_table),
			   fasdump_tuple,
			   fasdump_vector,
			   fasdump_cc_entry,
			   fasdump_weak_pair,
			   gc_precheck_from);
      (GCT_ENTRY ((&fasdump_table), TC_PRIMITIVE)) = handle_primitive;
      (GCT_ENTRY ((&fasdump_table), TC_PCOMB0)) = handle_primitive;
      (GCT_ENTRY ((&fasdump_table), TC_MANIFEST_CLOSURE))
	= handle_manifest_closure;
      (GCT_ENTRY ((&fasdump_table), TC_LINKAGE_SECTION))
	= handle_linkage_section;
      (GCT_ENTRY ((&fasdump_table), TC_INTERNED_SYMBOL)) = handle_symbol;
      (GCT_ENTRY ((&fasdump_table), TC_BROKEN_HEART)) = handle_broken_heart;
      (GCT_ENTRY ((&fasdump_table), TC_UNINTERNED_SYMBOL)) = handle_symbol;
      (GCT_ENTRY ((&fasdump_table), TC_VARIABLE)) = handle_variable;
      (GCT_ENTRY ((&fasdump_table), TC_ENVIRONMENT)) = handle_environment;
      initialized_p = true;
    }

  (GCTX_TABLE (ctx)) = (&fasdump_table);
  (GCTX_PTO (ctx)) = pto;
  (GCTX_PTO_END (ctx)) = pto_end;
  (GCTX_FROM_START (ctx)) = active_heap_start;
  (GCTX_FROM_END (ctx)) = Free;
  (CTX_PFIX (ctx)) = pfix;
  (CTX_MODE (ctx)) = mode;
  (CTX_PR (ctx)) = pr;
  (CTX_CC_SEEN_P (ctx)) = false;

  if (setjmp (fasdump_loop_env))
    return (fasdump_loop_result);

  run_gc_loop (scan, pto, ctx);

  return (PRIM_DONE);
}

static
DEFINE_GC_HANDLER (handle_primitive)
{
  (*scan) = (renumber_primitive (object, (CTX_PR (ctx))));
  return (scan + 1);
}

static
DEFINE_GC_HANDLER (handle_manifest_closure)
{
  (CTX_CC_SEEN_P (ctx)) = true;
  return (gc_handle_manifest_closure (scan, object, ctx));
}

static
DEFINE_GC_HANDLER (handle_linkage_section)
{
  (CTX_CC_SEEN_P (ctx)) = true;
  return (gc_handle_linkage_section (scan, object, ctx));
}

static
DEFINE_GC_HANDLER (handle_symbol)
{
  SCHEME_OBJECT * from = (OBJECT_ADDRESS (object));
  SCHEME_OBJECT * new_address;

  if (BROKEN_HEART_P (*from))
    new_address = (OBJECT_ADDRESS (*from));
  else
    {
      new_address = (fasdump_transport_words (from, 2, 0, ctx));
      (new_address[SYMBOL_GLOBAL_VALUE])
	= (((OBJECT_TYPE (object)) == TC_INTERNED_SYMBOL)
	   ? BROKEN_HEART_ZERO
	   : UNBOUND_OBJECT);
    }
  (*scan) = (OBJECT_NEW_ADDRESS (object, new_address));
  return (scan + 1);
}

static
DEFINE_GC_HANDLER (handle_broken_heart)
{
  return
    (((OBJECT_DATUM (object)) == 0)
     ? (scan + 1)
     : (gc_handle_broken_heart (scan, object, ctx)));
}

static
DEFINE_GC_HANDLER (handle_variable)
{
  SCHEME_OBJECT * from = (OBJECT_ADDRESS (object));
  SCHEME_OBJECT * new_address;

  if (BROKEN_HEART_P (*from))
    new_address = (OBJECT_ADDRESS (*from));
  else
    {
      new_address = (fasdump_transport_words (from, 3, 0, ctx));
      (new_address[1]) = UNCOMPILED_VARIABLE;
      (new_address[2]) = SHARP_F;
    }
  (*scan) = (OBJECT_NEW_ADDRESS (object, new_address));
  return (scan + 1);
}

static
DEFINE_GC_HANDLER (handle_environment)
{
  if ((CTX_MODE (ctx)) == FE_DUMP)
    {
      (*scan) = (fasdump_vector (object, false, ctx));
      return (scan + 1);
    }
  else
    {
      fasdump_loop_result = ERR_FASDUMP_ENVIRONMENT;
      longjmp (fasdump_loop_env, 1);
    }
}

static
DEFINE_GC_TUPLE_HANDLER (fasdump_tuple)
{
  SCHEME_OBJECT * from = (OBJECT_ADDRESS (tuple));
  return
    (OBJECT_NEW_ADDRESS
     (tuple,
      ((BROKEN_HEART_P (*from))
       ? (OBJECT_ADDRESS (*from))
       : (fasdump_transport_words (from, n_words, 0, ctx)))));
}

static
DEFINE_GC_VECTOR_HANDLER (fasdump_vector)
{
  SCHEME_OBJECT * from = (OBJECT_ADDRESS (vector));
  return
    (OBJECT_NEW_ADDRESS
     (vector,
      ((BROKEN_HEART_P (*from))
       ? (OBJECT_ADDRESS (*from))
       : (fasdump_transport_words (from,
				   (1 + (OBJECT_DATUM (*from))),
				   align_p,
				   ctx)))));
}

static
DEFINE_GC_OBJECT_HANDLER (fasdump_cc_entry)
{
#ifdef CC_SUPPORT_P
  SCHEME_OBJECT * old_block = (cc_entry_to_block_address (object));
  SCHEME_OBJECT * new_block;

  (CTX_CC_SEEN_P (ctx)) = true;
  if (old_block == (OBJECT_ADDRESS (compiler_utilities)))
    return (object);
  if (BROKEN_HEART_P (*old_block))
    new_block = (OBJECT_ADDRESS (*old_block));
  else
    {
      unsigned long length = (OBJECT_DATUM (*old_block));
      new_block = (fasdump_transport_words (old_block, (1 + length), 1, ctx));
      if (((CTX_MODE (ctx)) == FE_DROP_CC)
	  && ((OBJECT_TYPE (new_block[length])) == TC_ENVIRONMENT))
	(new_block[length]) = SHARP_F;
    }
  return (CC_ENTRY_NEW_BLOCK (object, new_block, old_block));
#else
  return (object);
#endif
}

static
DEFINE_GC_OBJECT_HANDLER (fasdump_weak_pair)
{
  return (fasdump_tuple (object, 2, ctx));
}

static SCHEME_OBJECT *
fasdump_transport_words (SCHEME_OBJECT * from,
			 unsigned long n_words,
			 bool align_p,
			 gc_ctx_t * ctx)
{
  if (((* (GCTX_PTO (ctx))) + n_words) > ((* (CTX_PFIX (ctx))) - 2))
    {
      /* Insufficient space to do the transport.  */
      fasdump_loop_result = PRIM_INTERRUPT;
      longjmp (fasdump_loop_env, 1);
    }
  {
    SCHEME_OBJECT old_contents = (*from);
    SCHEME_OBJECT * new_address
      = (gc_transport_words (from, n_words, align_p, ctx));
    (* (-- (* (CTX_PFIX (ctx))))) = old_contents;
    (* (-- (* (CTX_PFIX (ctx))))) = (ADDRESS_TO_DATUM (from));
    return (new_address);
  }
}

DEFINE_PRIMITIVE ("DUMP-BAND", Prim_band_dump, 2, 2,
		  "(PROCEDURE NAMESTRING)\n\
Saves an image of the current world to the file NAMESTRING.\n\
When the file is reloaded, PROCEDURE is called with an argument of #F.")
{
  SCHEME_OBJECT * to = Free;
  fasl_header_t h;
  SCHEME_OBJECT * prim_table_start;
  bool result;
  PRIMITIVE_HEADER (2);

  CHECK_ARG (1, INTERPRETER_APPLICABLE_P);
  CHECK_ARG (2, STRING_P);

  /* Cause the image to be in the low heap, to increase
     the probability that no relocation is needed on reload. */
  if (inactive_heap_start < active_heap_start)
    Primitive_GC (0);

  Primitive_GC_If_Needed (5);
  initialize_band_fasl_header (&h);
  {
    SCHEME_OBJECT comb;
    SCHEME_OBJECT root;

    comb = (MAKE_POINTER_OBJECT (TC_COMBINATION_1, to));
    (to[COMB_1_FN]) = (ARG_REF (1));
    (to[COMB_1_ARG_1]) = SHARP_F;
    to += 2;

    root = (MAKE_POINTER_OBJECT (TC_LIST, to));
    (*to++) = comb;
    (*to++) = compiler_utilities;

    (FASLHDR_ROOT_POINTER (&h)) = to;
    (*to++) = root;
  }

  prim_table_start = to;
  (FASLHDR_N_PRIMITIVES (&h)) = MAX_PRIMITIVE;
  (FASLHDR_PRIMITIVE_TABLE_SIZE (&h)) = (primitive_table_export_length ());
  to += (FASLHDR_PRIMITIVE_TABLE_SIZE (&h));
  if (to > active_heap_end)
    result = false;
  else
    {
      const char * filename = (STRING_POINTER (ARG_REF (2)));
      SCHEME_OBJECT * faligned_heap = active_heap_start;
      SCHEME_OBJECT * faligned_constant = constant_start;
      fasl_file_handle_t handle;

      export_primitive_table (prim_table_start);

      while (!FLOATING_ALIGNED_P (faligned_heap))
	faligned_heap += 1;

      while (!FLOATING_ALIGNED_P (faligned_constant))
	faligned_constant += 1;

      (FASLHDR_HEAP_START (&h)) = faligned_heap;
      (FASLHDR_HEAP_END (&h)) = to;
      (FASLHDR_CONSTANT_START (&h)) = faligned_constant;
      (FASLHDR_CONSTANT_END (&h)) = constant_alloc_next;

      OS_file_remove_link (filename);
      if (!open_fasl_output_file (filename, (&handle)))
	error_bad_range_arg (2);

      result = (write_fasl_file ((&h), prim_table_start, handle));

      if (!close_fasl_output_file (handle))
	OS_file_remove (filename);
    }
  PRIMITIVE_RETURN (BOOLEAN_TO_OBJECT (result));
}

static void
initialize_object_fasl_header (fasl_header_t * h)
{
  initialize_fasl_header_1 (h);
  (FASLHDR_BAND_P (h)) = false;
  (FASLHDR_CONSTANT_START (h)) = constant_start;
  (FASLHDR_CONSTANT_END (h)) = constant_start;
}

static void
initialize_band_fasl_header (fasl_header_t * h)
{
  initialize_fasl_header_1 (h);
  (FASLHDR_BAND_P (h)) = true;
  save_fasl_header_cc_info (h, true);
}

static void
initialize_fasl_header_1 (fasl_header_t * h)
{
  (FASLHDR_VERSION (h)) = CURRENT_FASL_VERSION;
  (FASLHDR_ARCH (h)) = CURRENT_FASL_ARCH;

#ifdef HEAP_IN_LOW_MEMORY
  (FASLHDR_MEMORY_BASE (h)) = 0;
#else
  (FASLHDR_MEMORY_BASE (h)) = memory_block_start;
#endif
  (FASLHDR_HEAP_RESERVED (h)) = active_heap_reserved;

  (FASLHDR_STACK_START (h)) = stack_start;
  (FASLHDR_STACK_END (h)) = stack_end;
}

static void
save_fasl_header_cc_info (fasl_header_t * h, bool save_p)
{
  if (save_p)
    {
      (FASLHDR_CC_VERSION (h)) = compiler_interface_version;
      (FASLHDR_CC_ARCH (h)) = compiler_processor_type;
      (FASLHDR_UTILITIES_VECTOR (h)) = compiler_utilities;
    }
  else
    {
      (FASLHDR_CC_VERSION (h)) = 0;
      (FASLHDR_CC_ARCH (h)) = COMPILER_NONE_TYPE;
      (FASLHDR_UTILITIES_VECTOR (h)) = SHARP_F;
    }
}

static bool
write_fasl_file (fasl_header_t * h,
		 SCHEME_OBJECT * prim_table_start,
		 fasl_file_handle_t handle)
{
  return
    ((write_fasl_header (h, handle))
     && (write_fasl_section ((FASLHDR_HEAP_START (h)),
			     (FASLHDR_HEAP_SIZE (h)),
			     handle))
     && (write_fasl_section ((FASLHDR_CONSTANT_START (h)),
			     (FASLHDR_CONSTANT_SIZE (h)),
			     handle))
     && (write_fasl_section (prim_table_start,
			     (FASLHDR_PRIMITIVE_TABLE_SIZE (h)),
			     handle)));
}

/* -*-C-*-

$Id: fasload.c,v 9.96.2.4 2006/08/16 19:15:44 cph Exp $

Copyright 1986,1987,1988,1989,1990,1991 Massachusetts Institute of Technology
Copyright 1992,1993,1994,1995,1996,1997 Massachusetts Institute of Technology
Copyright 1998,2000,2001,2002,2005 Massachusetts Institute of Technology

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

/* The "fast loader" reads a FASL file, which contains a binary
   representation of an object.  The "band loader" reads a special
   FASL file containing a world image.  */

#include "scheme.h"
#include "prims.h"
#include "history.h"
#include "osscheme.h"
#include "osfile.h"
#include "osio.h"
#include "gccode.h"
#include "trap.h"
#include "option.h"
#include "fasl.h"

typedef struct
{
  SCHEME_OBJECT * heap;
  SCHEME_OBJECT * constant;
  SCHEME_OBJECT * stack_start;
  SCHEME_OBJECT * stack_end;
  SCHEME_OBJECT * utilities;
} fasl_basis_t;

typedef struct
{
  gc_ctx_t gc_ctx;
  fasl_header_t * header;
  fasl_basis_t * basis;
  SCHEME_OBJECT * prim_table;
} fl_ctx_t;

#define CTX_HEADER(ctx) (((fl_ctx_t *) (ctx))->header)
#define CTX_BASIS(ctx) (((fl_ctx_t *) (ctx))->basis)
#define CTX_PRIM_TABLE(ctx) (((fl_ctx_t *) (ctx))->prim_table)

struct load_band_termination_state
{
  const char * file_name;
  bool no_return_p;
};

typedef void (*cleanup_t) (void);

static const char * reload_band_name = 0;
static Tptrvec reload_cleanups = 0;
static unsigned long reload_heap_size = 0;
static unsigned long reload_constant_size = 0;

static gc_table_t fasload_table;
static gc_table_t intern_table;

static Tchannel read_file_start (const char *, bool, fasl_header_t *h);
static SCHEME_OBJECT load_file (Tchannel, fasl_header_t *);
static void * read_from_file (Tchannel, void *, size_t);
static bool primitive_numbers_unchanged_p (SCHEME_OBJECT *, fasl_header_t *);

static void relocate_block
  (SCHEME_OBJECT *, SCHEME_OBJECT *, fasl_header_t *, fasl_basis_t *,
   SCHEME_OBJECT *);

static gc_handler_t handle_primitive;
static gc_tuple_handler_t fasload_tuple;
static gc_vector_handler_t fasload_vector;
static gc_object_handler_t fasload_cc_entry;
static gc_object_handler_t fasload_weak_pair;

static void * relocate_address (void *, fasl_header_t *, fasl_basis_t *);
static void intern_block (SCHEME_OBJECT *, SCHEME_OBJECT *);

static gc_handler_t intern_handle_symbol;
static gc_tuple_handler_t intern_tuple;
static gc_vector_handler_t intern_vector;
static gc_object_handler_t intern_cc_entry;
static gc_object_handler_t intern_weak_pair;

static SCHEME_OBJECT read_band_file (SCHEME_OBJECT);
static void save_memmag_state (void);
static void abort_band_load (void *);
static void terminate_band_load (void *);

static bool decode_fasl_header (SCHEME_OBJECT *, fasl_header_t *);
static SCHEME_OBJECT * faslobj_address (SCHEME_OBJECT, fasl_header_t *);
static fasl_read_status_t check_fasl_version (fasl_header_t *);
static fasl_read_status_t check_fasl_cc_version (fasl_header_t *);
static void print_fasl_information (fasl_header_t *);

DEFINE_PRIMITIVE ("BINARY-FASLOAD", Prim_binary_fasload, 1, 1, "(NAMESTRING)\n\
Load the contents of the file NAMESTRING into memory.  The file was\n\
presumably made by a call to PRIMITIVE-FASDUMP, and may contain data\n\
for the heap and/or the pure area.  The value returned is the object\n\
that was dumped.")
{
  PRIMITIVE_HEADER (1);
  canonicalize_primitive_context ();
  {
    fasl_header_t h;
    PRIMITIVE_RETURN
      (load_file ((read_file_start ((STRING_ARG (1)), false, (&h))), (&h)));
  }
}

static Tchannel
read_file_start (const char * file_name, bool band_mode_p, fasl_header_t * h)
{
  static unsigned long failed_heap_length = 0;
  Tchannel channel;

  if (Per_File)
    debug_edit_flags ();

  channel = (OS_open_load_file (file_name));
  if (channel == NO_CHANNEL)
    error_bad_range_arg (1);

  {
    SCHEME_OBJECT raw [FASL_HEADER_LENGTH];
    read_from_file (channel, raw, FASL_HEADER_LENGTH);
    if (!decode_fasl_header (raw, h))
      {
	OS_channel_close_noerror (channel);
	signal_error_from_primitive (ERR_FASL_FILE_BAD_DATA);
      }
  }

#ifndef INHIBIT_FASL_VERSION_CHECK
  {
    fasl_read_status_t status = (check_fasl_version (h));
    if (status != FASL_FILE_FINE)
      {
	OS_channel_close_noerror (channel);
	signal_error_from_primitive (ERR_FASL_FILE_BAD_DATA);
	/*NOTREACHED*/
      }
  }
#endif

#ifndef INHIBIT_COMPILED_VERSION_CHECK
  {
    fasl_read_status_t status = (check_fasl_cc_version (h));
    if (status != FASL_FILE_FINE)
      {
	OS_channel_close_noerror (channel);
	signal_error_from_primitive (ERR_FASLOAD_COMPILED_MISMATCH);
	/*NOTREACHED*/
      }
  }
#endif

  if (Reloc_Debug || File_Load_Debug)
    print_fasl_information (h);

  if (! (band_mode_p
	 ? (update_allocator_parameters (FASLHDR_CONSTANT_SIZE (h)))
	 : ((constant_alloc_next + (FASLHDR_CONSTANT_SIZE (h)))
	    <= constant_end)))
    {
      OS_channel_close_noerror (channel);
      signal_error_from_primitive (ERR_FASL_FILE_TOO_BIG);
    }
  if (band_mode_p)
    SET_HEAP_ALLOC_LIMIT (active_heap_end);
  {
    unsigned long heap_length
      = ((FASLHDR_HEAP_SIZE (h))
	 + (FASLHDR_N_PRIMITIVES (h))
	 + (FASLHDR_PRIMITIVE_TABLE_SIZE (h)));
    if (GC_NEEDED_P (heap_length))
      {
	if (band_mode_p || (heap_length == failed_heap_length))
	  {
	    OS_channel_close_noerror (channel);
	    signal_error_from_primitive (ERR_FASL_FILE_TOO_BIG);
	    /*NOTREACHED*/
	  }
	else
	  {
	    failed_heap_length = heap_length;
	    OS_channel_close_noerror (channel);
	    REQUEST_GC (heap_length);
	    signal_interrupt_from_primitive ();
	    /*NOTREACHED*/
	  }
      }
  }
  failed_heap_length = 0;

  if ((FASLHDR_BAND_P (h)) && (!band_mode_p))
    {
      OS_channel_close_noerror (channel);
      signal_error_from_primitive (ERR_FASLOAD_BAND);
      /*NOTREACHED*/
    }

  return (channel);
}

static SCHEME_OBJECT
load_file (Tchannel channel, fasl_header_t * h)
{
  fasl_basis_t new_basis;
  fasl_basis_t * nb = (&new_basis);
  SCHEME_OBJECT * prim_table;

  (nb->heap) = Free;
  (nb->constant) = constant_alloc_next;
  (nb->stack_start) = stack_start;
  (nb->stack_end) = stack_end;
  (nb->utilities)
    = ((compiler_utilities == SHARP_F)
       ? 0
       : (OBJECT_ADDRESS (compiler_utilities)));

  Free = (read_from_file (channel, Free, (FASLHDR_HEAP_SIZE (h))));
  constant_alloc_next
    = (read_from_file (channel,
		       constant_alloc_next,
		       (FASLHDR_CONSTANT_SIZE (h))));

  prim_table = Free;
  {
    SCHEME_OBJECT * raw_prim_table = (Free + (FASLHDR_N_PRIMITIVES (h)));
    read_from_file (channel,
		    raw_prim_table,
		    (FASLHDR_PRIMITIVE_TABLE_SIZE (h)));
    OS_channel_close_noerror (channel);
    import_primitive_table
      (raw_prim_table, (FASLHDR_N_PRIMITIVES (h)), prim_table);
  }

  if ((!FASLHDR_BAND_P (h))
      && ((FASLHDR_UTILITIES_VECTOR (h)) != SHARP_F)
      && (compiler_utilities == SHARP_F))
    /* The file contains compiled code, but there's no compiled-code
       support available.  */
    signal_error_from_primitive (ERR_FASLOAD_COMPILED_MISMATCH);

  if (! ((FASLHDR_BAND_P (h))
	 && ((FASLHDR_HEAP_START (h)) == (nb->heap))
	 && (((FASLHDR_CONSTANT_START (h)) == (nb->constant))
	     || ((FASLHDR_CONSTANT_START (h)) == (FASLHDR_CONSTANT_END (h))))
	 && (((FASLHDR_STACK_START (h)) == 0)
	     || ((FASLHDR_STACK_START (h)) == (nb->stack_start)))
	 && ((FASLHDR_STACK_END (h)) == (nb->stack_end))
	 && (primitive_numbers_unchanged_p (prim_table, h))))
    {
      if (Reloc_Debug)
	outf_console
	  ("relocation of heap: %#lx -> %#lx; constant: %#lx -> %#lx\n",
	   ((unsigned long) (FASLHDR_HEAP_START (h))),
	   ((unsigned long) (nb->heap)),
	   ((unsigned long) (FASLHDR_CONSTANT_START (h))),
	   ((unsigned long) (nb->constant)));
      relocate_block ((nb->heap), Free, h, nb, prim_table);
      relocate_block ((nb->constant), constant_alloc_next, h, nb, prim_table);
    }
  if (!FASLHDR_BAND_P (h))
    {
      intern_block ((nb->heap), Free);
      intern_block ((nb->constant), constant_alloc_next);
    }

#ifdef PUSH_D_CACHE_REGION
  if ((FASLHDR_CC_VERSION (h)) != COMPILER_NONE_TYPE)
    {
      if ((FASLHDR_HEAP_SIZE (h)) > 0)
	PUSH_D_CACHE_REGION ((nb->heap), (FASLHDR_HEAP_SIZE (h)));
      if ((FASLHDR_CONSTANT_SIZE (h)) > 0)
	PUSH_D_CACHE_REGION ((nb->constant), (FASLHDR_CONSTANT_SIZE (h)));
    }
#endif

  return
    (* ((SCHEME_OBJECT *)
	(relocate_address ((FASLHDR_ROOT_POINTER (h)), h, nb))));
}

static void *
read_from_file (Tchannel channel, void * p, size_t n_words)
{
  size_t n_bytes = (n_words * (sizeof (SCHEME_OBJECT)));
  if ((OS_channel_read_load_file (channel, p, n_bytes)) < n_bytes)
    {
      OS_channel_close_noerror (channel);
      signal_error_from_primitive (ERR_FASL_FILE_BAD_DATA);
    }
  return (((char *) p) + n_bytes);
}

static bool
primitive_numbers_unchanged_p (SCHEME_OBJECT * table, fasl_header_t * h)
{
  unsigned long count;

  for (count = 0; (count < (FASLHDR_N_PRIMITIVES (h))); count += 1)
    if ((table[count]) != (MAKE_PRIMITIVE_OBJECT (count)))
      return (false);
  return (true);
}

/* Next_Pointer starts by pointing to the beginning of the block of
   memory to be handled.  This loop relocates all pointers in the
   block of memory.  */

static void
relocate_block (SCHEME_OBJECT * scan,
		SCHEME_OBJECT * end,
		fasl_header_t * h,
		fasl_basis_t * nb,
		SCHEME_OBJECT * prim_table)
{
  static bool initialized_p = false;
  fl_ctx_t ctx0;
  gc_ctx_t * ctx = ((gc_ctx_t *) (&ctx0));

  if (!initialized_p)
    {
      initialize_gc_table ((&fasload_table),
			   fasload_tuple,
			   fasload_vector,
			   fasload_cc_entry,
			   fasload_weak_pair,
			   gc_precheck_from);
      (GCT_ENTRY ((&fasload_table), TC_PRIMITIVE)) = handle_primitive;
      (GCT_ENTRY ((&fasload_table), TC_PCOMB0)) = handle_primitive;
      (GCT_ENTRY ((&fasload_table), TC_BROKEN_HEART)) = gc_handle_non_pointer;
      (GCT_ENTRY ((&fasload_table), TC_MANIFEST_SPECIAL_NM_VECTOR))
	= gc_handle_non_pointer;
      initialized_p = true;
    }

  (GCTX_TABLE (ctx)) = (&fasload_table);
  (GCTX_PTO (ctx)) = 0;
  (CTX_HEADER (ctx)) = h;
  (CTX_BASIS (ctx)) = nb;
  (CTX_PRIM_TABLE (ctx)) = prim_table;

  if (Reloc_Debug)
    outf_error ("\nblock = %#lx, length = %#lx, end = %#lx.\n",
		((unsigned long) scan),
		((unsigned long) ((end - scan) - 1)),
		((unsigned long) end));

  run_gc_loop (scan, (&end), ctx);
}

static
DEFINE_GC_HANDLER (handle_primitive)
{
  unsigned long datum = (OBJECT_DATUM (object));
  unsigned long high_bits = (datum >> HALF_DATUM_LENGTH);
  (*scan)
    = (MAKE_OBJECT_FROM_OBJECTS
       (object,
	((CTX_PRIM_TABLE (ctx)) [((high_bits != 0) ? high_bits : datum)])));
  return (scan + 1);
}

#define RELOCATE_OBJECT(object, ctx)					\
  (OBJECT_NEW_ADDRESS ((object),					\
		       (relocate_address ((OBJECT_ADDRESS (object)),	\
					  (CTX_HEADER (ctx)),		\
					  (CTX_BASIS (ctx))))))

static
DEFINE_GC_TUPLE_HANDLER (fasload_tuple)
{
  return (RELOCATE_OBJECT (tuple, ctx));
}

static
DEFINE_GC_VECTOR_HANDLER (fasload_vector)
{
  return (RELOCATE_OBJECT (vector, ctx));
}

static
DEFINE_GC_OBJECT_HANDLER (fasload_cc_entry)
{
  return (CC_ENTRY_NEW_ADDRESS (object,
				(relocate_address ((CC_ENTRY_ADDRESS (object)),
						   (CTX_HEADER (ctx)),
						   (CTX_BASIS (ctx))))));
}

static
DEFINE_GC_OBJECT_HANDLER (fasload_weak_pair)
{
  return (RELOCATE_OBJECT (object, ctx));
}

/* Relocate a pointer as read in from the file.  If the pointer used
   to point into the heap, relocate it into the heap.  If it used to
   be constant area, relocate it to constant area.  Otherwise give an
   error.  */

static void *
relocate_address (void * vaddr, fasl_header_t * h, fasl_basis_t * nb)
{
  byte_t * caddr = vaddr;
  byte_t * result;

  if ((caddr >= ((byte_t *) (FASLHDR_HEAP_START (h))))
      && (caddr < ((byte_t *) (FASLHDR_HEAP_END (h)))))
    result
      = (((byte_t *) (nb->heap))
	 + (caddr - ((byte_t *) (FASLHDR_HEAP_START (h)))));
  else if ((caddr >= ((byte_t *) (FASLHDR_CONSTANT_START (h))))
	   && (caddr < ((byte_t *) (FASLHDR_CONSTANT_END (h)))))
    result
      = (((byte_t *) (nb->constant))
	 + (caddr - ((byte_t *) (FASLHDR_CONSTANT_START (h)))));
  else if ((caddr >= ((byte_t *) (FASLHDR_UTILITIES_START (h))))
	   && (caddr < ((byte_t *) (FASLHDR_UTILITIES_END (h)))))
    result
      = (((byte_t *) (nb->utilities))
	 + (caddr - ((byte_t *) (FASLHDR_UTILITIES_START (h)))));
  else if (ADDRESS_IN_STACK_REGION_P (caddr,
				      ((byte_t *) (FASLHDR_STACK_START (h))),
				      ((byte_t *) (FASLHDR_STACK_END (h)))))
    result
      = (N_PUSHED_TO_SP
	 ((SP_TO_N_PUSHED (caddr,
			   ((byte_t *) (FASLHDR_STACK_START (h))),
			   ((byte_t *) (FASLHDR_STACK_END (h))))),
	  ((byte_t *) (nb->stack_start)),
	  ((byte_t *) (nb->stack_end))));
  else
    {
      outf_console ("Pointer out of range: %#lx\n", ((unsigned long) caddr));
      outf_console ("Heap: %#lx-%#lx, Constant: %#lx-%#lx, Stack: %#lx-%#lx\n",
		    ((unsigned long) (FASLHDR_HEAP_START (h))),
		    ((unsigned long) (FASLHDR_HEAP_END (h))),
		    ((unsigned long) (FASLHDR_CONSTANT_START (h))),
		    ((unsigned long) (FASLHDR_CONSTANT_END (h))),
		    ((unsigned long) (FASLHDR_STACK_START (h))),
		    ((unsigned long) (FASLHDR_STACK_END (h))));
      termination_init_error ();
    }
  if (Reloc_Debug)
    outf_console ("%#lx -> %#lx\n",
		  ((unsigned long) caddr),
		  ((unsigned long) result));
  return (result);
}

static void
intern_block (SCHEME_OBJECT * scan, SCHEME_OBJECT * end)
{
  static bool initialized_p = false;
  gc_ctx_t ctx0;
  gc_ctx_t * ctx = (&ctx0);

  if (!initialized_p)
    {
      initialize_gc_table ((&intern_table),
			   intern_tuple,
			   intern_vector,
			   intern_cc_entry,
			   intern_weak_pair,
			   gc_precheck_from);
      (GCT_ENTRY ((&intern_table), TC_INTERNED_SYMBOL)) = intern_handle_symbol;
      (GCT_ENTRY ((&intern_table), TC_BROKEN_HEART)) = gc_handle_non_pointer;
      (GCT_ENTRY ((&intern_table), TC_MANIFEST_SPECIAL_NM_VECTOR))
	= gc_handle_non_pointer;
      initialized_p = true;
    }

  (GCTX_TABLE (ctx)) = (&intern_table);
  (GCTX_PTO (ctx)) = 0;

  if (Reloc_Debug)
    outf_console ("Interning a block.\n");

  run_gc_loop (scan, (&end), ctx);

  if (Reloc_Debug)
    outf_console ("Done interning block.\n");
}

static
DEFINE_GC_HANDLER (intern_handle_symbol)
{
  if (BROKEN_HEART_P (GET_SYMBOL_GLOBAL_VALUE (object)))
    {
      SET_SYMBOL_GLOBAL_VALUE (object, UNBOUND_OBJECT);
      {
	SCHEME_OBJECT new = (intern_symbol (object));
	if (new != object)
	  {
	    (*scan) = new;
	    SET_SYMBOL_NAME (object, (OBJECT_NEW_TYPE (TC_BROKEN_HEART, new)));
	  }
      }
    }
  else if (BROKEN_HEART_P (GET_SYMBOL_NAME (object)))
    (*scan)
      = (MAKE_OBJECT_FROM_OBJECTS (object,
				   (GET_SYMBOL_NAME (object))));
  return (scan + 1);
}

static
DEFINE_GC_TUPLE_HANDLER (intern_tuple)
{
  return (tuple);
}

static
DEFINE_GC_VECTOR_HANDLER (intern_vector)
{
  return (vector);
}

static
DEFINE_GC_OBJECT_HANDLER (intern_cc_entry)
{
  return (object);
}

static
DEFINE_GC_OBJECT_HANDLER (intern_weak_pair)
{
  return (object);
}

DEFINE_PRIMITIVE ("LOAD-BAND", Prim_band_load, 1, 1, "(NAMESTRING)\n\
Restores the heap and constant space from the contents of the file\n\
NAMESTRING, which is typically a file created by DUMP-BAND.  The file\n\
can, however, be any file which can be loaded with BINARY-FASLOAD.")
{
  SCHEME_OBJECT result;
  PRIMITIVE_HEADER (1);

  CHECK_ARG (1, STRING_P);
  canonicalize_primitive_context ();
  result = (read_band_file (ARG_REF (1)));

  /* Reset implementation state paramenters.  */
  INITIALIZE_INTERRUPTS ();
  INITIALIZE_STACK ();
  RESET_HEAP_ALLOC_LIMIT ();
  compiler_utilities = (PAIR_CDR (result));
  if (compiler_utilities != SHARP_F)
    compiler_reset (compiler_utilities);
  else
    compiler_initialize (true);
  SET_INTERRUPT_MASK (0);	/* Until the continuation is invoked.  */
  fixed_objects = SHARP_F;
  current_state_point = SHARP_F;

  /* Setup initial program */
  SET_RC (RC_END_OF_COMPUTATION);
  SET_EXP (SHARP_F);
  SAVE_CONT ();
  SET_EXP (PAIR_CAR (result));
  SET_ENV (THE_GLOBAL_ENV);

  /* Clear various interpreter state parameters.  */
  trapping = false;
  history_register = (make_dummy_history ());
  prev_restore_history_offset = 0;
  CC_TRANSPORT_END ();
  execute_reload_cleanups ();
  EXIT_CRITICAL_SECTION ({});

  /* Return in a non-standard way. */
  PRIMITIVE_ABORT (PRIM_DO_EXPRESSION);
  /*NOTREACHED*/
  PRIMITIVE_RETURN (UNSPECIFIC);
}

static SCHEME_OBJECT
read_band_file (SCHEME_OBJECT s)
{
  const char * file_name;
  struct load_band_termination_state * state;
  fasl_header_t h;
  Tchannel channel;
  SCHEME_OBJECT result;

  transaction_begin ();		/* 1 */
  file_name = (OS_malloc ((STRING_LENGTH (s)) + 1));
  strcpy (((char *) file_name), (STRING_POINTER (s)));
  state = (dstack_alloc (sizeof (struct load_band_termination_state)));
  (state->file_name) = file_name;
  (state->no_return_p) = false;
  transaction_record_action (tat_abort, terminate_band_load, state);

  transaction_begin ();		/* 2 */
  save_memmag_state ();
  reset_allocator_parameters ();
  SET_HEAP_ALLOC_LIMIT (active_heap_end);
  ENTER_CRITICAL_SECTION ("band load");
  channel = (read_file_start (file_name, true, (&h)));
  transaction_commit ();	/* 2 */

  /* Now read the file into memory.  Past this point we can't abort
     and return to the old image.  */
  (state->no_return_p) = true;
  result = (load_file (channel, (&h)));

  /* Done -- we have the new image.  */
  transaction_commit ();	/* 1 */
  {
    void * old_name = ((void *) reload_band_name);
    reload_band_name = file_name;
    if (old_name != 0)
      OS_free (old_name);
  }
  return (result);
}

DEFINE_PRIMITIVE ("RELOAD-BAND-NAME", Prim_reload_band_name, 0, 0, "()\n\
Return the filename from which the runtime system was last restored.\n\
The result is a string, or #F if the system was not restored.")
{
  PRIMITIVE_HEADER (0);
  PRIMITIVE_RETURN
    ((reload_band_name != 0)
     ? (char_pointer_to_string (reload_band_name))
     : (option_band_file != 0)
     ? (char_pointer_to_string (option_band_file))
     : SHARP_F);
}

struct memmag_state
{
  SCHEME_OBJECT * active_heap_start;
  SCHEME_OBJECT * active_heap_end;
  SCHEME_OBJECT * inactive_heap_start;
  SCHEME_OBJECT * inactive_heap_end;
  SCHEME_OBJECT * free;
  SCHEME_OBJECT * heap_alloc_limit;
  SCHEME_OBJECT * constant_space;
  SCHEME_OBJECT * constant_top;
  SCHEME_OBJECT * constant_alloc_next;
  SCHEME_OBJECT * stack_pointer;
  SCHEME_OBJECT * stack_start;
  SCHEME_OBJECT * stack_end;
  SCHEME_OBJECT * stack_guard;
};

static void
save_memmag_state (void)
{
  struct memmag_state * mp = (dstack_alloc (sizeof (struct memmag_state)));
  (mp->active_heap_start) = active_heap_start;
  (mp->active_heap_end) = active_heap_end;
  (mp->inactive_heap_start) = inactive_heap_start;
  (mp->inactive_heap_end) = inactive_heap_end;
  (mp->free) = Free;
  (mp->heap_alloc_limit) = heap_alloc_limit;
  (mp->constant_alloc_next) = constant_alloc_next;
  (mp->constant_space) = constant_start;
  (mp->constant_top) = constant_end;
  (mp->stack_pointer) = stack_pointer;
  (mp->stack_start) = stack_start;
  (mp->stack_end) = stack_end;
  (mp->stack_guard) = stack_guard;
  transaction_record_action (tat_abort, abort_band_load, mp);
}

static void
abort_band_load (void * ap)
{
  struct memmag_state * mp = ((struct memmag_state *) ap);
  active_heap_start = (mp->active_heap_start);
  active_heap_end = (mp->active_heap_end);
  inactive_heap_start = (mp->inactive_heap_start);
  inactive_heap_end = (mp->inactive_heap_end);
  Free = (mp->free);
  constant_alloc_next = (mp->constant_alloc_next);
  constant_start = (mp->constant_space);
  constant_end = (mp->constant_top);
  stack_pointer = (mp->stack_pointer);
  stack_start = (mp->stack_start);
  stack_end = (mp->stack_end);
  stack_guard = (mp->stack_guard);
  SET_HEAP_ALLOC_LIMIT (mp->heap_alloc_limit);
  EXIT_CRITICAL_SECTION ({});
}

static void
terminate_band_load (void * ap)
{
  struct load_band_termination_state * state = ap;
  int abort_value;

  if (! (state->no_return_p))
    {
      OS_free ((void *) (state->file_name));
      return;
    }

  abort_value = (abort_to_interpreter_argument ());

  fputs ("\nload-band: ", stderr);
  if (abort_value > 0)
    outf_fatal ("Error %d (%s)", abort_value, (Error_Names[abort_value]));
  else
    {
      abort_value = ((-abort_value) - 1);
      outf_fatal ("Abort %d (%s)", abort_value, (Abort_Names[abort_value]));
    }
  outf_fatal (" past the point of no return.\n");
  outf_fatal ("file name = \"%s\".\n", (state->file_name));
  OS_free ((void *) (state->file_name));

  execute_reload_cleanups ();
  EXIT_CRITICAL_SECTION ({});
  Microcode_Termination (TERM_DISK_RESTORE);
  /*NOTREACHED*/
}

void
get_band_parameters (unsigned long * heap_size, unsigned long * const_size)
{
  (*heap_size) = reload_heap_size;
  (*const_size) = reload_constant_size;
}

void
add_reload_cleanup (cleanup_t cleanup_procedure)
{
  if (reload_cleanups == 0)
    {
      reload_cleanups = (ptrvec_allocate (1));
      (* ((cleanup_t *) (PTRVEC_LOC (reload_cleanups, 0))))
	= cleanup_procedure;
    }
  else
    ptrvec_adjoin (reload_cleanups, cleanup_procedure);
}

void
execute_reload_cleanups (void)
{
  void ** scan = (PTRVEC_START (reload_cleanups));
  void ** end = (PTRVEC_END (reload_cleanups));
  while (scan < end)
    (* ((cleanup_t *) (scan++))) ();
}

static bool
decode_fasl_header (SCHEME_OBJECT * raw, fasl_header_t * h)
{
  if ((raw[FASL_OFFSET_MARKER]) != FASL_FILE_MARKER)
    return (false);
  {
    SCHEME_OBJECT object = (raw[FASL_OFFSET_VERSION]);
    (h->version) = (FASL_VERSION (object));
    (h->arch) = (FASL_ARCH (object));
  }
  {
    SCHEME_OBJECT object = (raw[FASL_OFFSET_CI_VERSION]);
    (h->cc_version) = (CI_VERSION (object));
    (h->cc_arch) = (CI_PROCESSOR (object));
    (h->band_p) = (CI_BAND_P (object));
  }
  {
    SCHEME_OBJECT * fasl_memory_base
      = ((SCHEME_OBJECT *) (raw[FASL_OFFSET_MEM_BASE]));
    (FASLHDR_MEMORY_BASE (h)) = fasl_memory_base;

    (FASLHDR_ROOT_POINTER (h))
      = (faslobj_address ((raw[FASL_OFFSET_DUMPED_OBJ]), h));

    (FASLHDR_HEAP_START (h))
      = (faslobj_address ((raw[FASL_OFFSET_HEAP_BASE]), h));
    (FASLHDR_HEAP_END (h))
      = ((FASLHDR_HEAP_START (h))
	 + (OBJECT_DATUM (raw[FASL_OFFSET_HEAP_SIZE])));
    (FASLHDR_HEAP_RESERVED (h))
      = (((h->version) >= FASL_VERSION_STACK_END)
	 ? (OBJECT_DATUM (raw[FASL_OFFSET_HEAP_RSVD]))
	 : 4500);

    (FASLHDR_CONSTANT_START (h))
      = (faslobj_address ((raw[FASL_OFFSET_CONST_BASE]), h));
    (FASLHDR_CONSTANT_END (h))
      = ((FASLHDR_CONSTANT_START (h))
	 + (OBJECT_DATUM (raw[FASL_OFFSET_CONST_SIZE])));

    if ((h->version) >= FASL_VERSION_STACK_END)
      {
	(FASLHDR_STACK_START (h))
	  = (faslobj_address ((raw[FASL_OFFSET_STACK_START]), h));
	(FASLHDR_STACK_END (h))
	  = ((FASLHDR_STACK_START (h))
	     + (OBJECT_DATUM (raw[FASL_OFFSET_STACK_SIZE])));
      }
    else
      /* In older versions, the "stack start" field held "stack
	 bottom" instead.  Since the stack grows downwards, this was
	 the maximum address.  */
      {
	(FASLHDR_STACK_END (h))
	  = (faslobj_address ((raw[FASL_OFFSET_STACK_START]), h));
	/* If !HEAP_IN_LOW_MEMORY then fasl_memory_base is the right
	   value.  Otherwise, fasl_memory_base is zero and that is at
	   least guaranteed to encompass the whole stack.  */
	(FASLHDR_STACK_START (h)) = fasl_memory_base;
      }

    (FASLHDR_N_PRIMITIVES (h))
      = (OBJECT_DATUM (raw[FASL_OFFSET_PRIM_LENGTH]));
    (FASLHDR_PRIMITIVE_TABLE_SIZE (h))
      = (OBJECT_DATUM (raw[FASL_OFFSET_PRIM_SIZE]));

    {
      SCHEME_OBJECT ruv = (raw[FASL_OFFSET_UT_BASE]);
      if (ruv == SHARP_F)
	{
	  (FASLHDR_UTILITIES_VECTOR (h)) = SHARP_F;
	  (FASLHDR_UTILITIES_START (h)) = 0;
	  (FASLHDR_UTILITIES_END (h)) = 0;
	}
      else
	{
	  SCHEME_OBJECT fuv
	    = (OBJECT_NEW_ADDRESS (ruv, (faslobj_address (ruv, h))));
	  (FASLHDR_UTILITIES_VECTOR (h)) = fuv;
	  (FASLHDR_UTILITIES_START (h)) = (OBJECT_ADDRESS (fuv));
	  (FASLHDR_UTILITIES_END (h))
	    = (VECTOR_LOC (fuv, (VECTOR_LENGTH (fuv))));
	}
    }
  }
  return (true);
}

static SCHEME_OBJECT *
faslobj_address (SCHEME_OBJECT o, fasl_header_t * h)
{
  return
    (((FASLHDR_MEMORY_BASE (h)) == 0)
     ? (OBJECT_ADDRESS (o))
     : ((FASLHDR_MEMORY_BASE (h)) + (OBJECT_DATUM (o))));
}

/* The error messages here should be handled by the runtime system! */

static fasl_read_status_t
check_fasl_version (fasl_header_t * h)
{
  if (((h->version) >= INPUT_FASL_VERSION)
      && ((h->version) <= CURRENT_FASL_VERSION))
    return (FASL_FILE_FINE);

  outf_error
    ("\nFASL File: Version %4u Architecture %4u.\n",
     (FASLHDR_VERSION (h)), (FASLHDR_ARCH (h)));
  outf_error
    ("Expected:  Version %4u Architecture %4u.\n",
     INPUT_FASL_VERSION,
     CURRENT_FASL_ARCH);

  return (((FASLHDR_ARCH (h)) != CURRENT_FASL_ARCH)
	  ? FASL_FILE_BAD_MACHINE
	  : FASL_FILE_BAD_VERSION);
}

static fasl_read_status_t
check_fasl_cc_version (fasl_header_t * h)
{
  if ((((FASLHDR_CC_VERSION (h)) == 0)
       && ((FASLHDR_CC_ARCH (h)) == COMPILER_NONE_TYPE))
      || (((FASLHDR_CC_VERSION (h)) == compiler_interface_version)
	  && ((FASLHDR_CC_ARCH (h)) == compiler_processor_type)))
    return (FASL_FILE_FINE);

  outf_error
    ("\nFASL File: compiled-code interface %4u; architecture %4u.\n",
     (FASLHDR_CC_VERSION (h)), (FASLHDR_CC_ARCH (h)));
  outf_error
    ("Expected:  compiled code interface %4ld; architecture %4ld.\n",
     compiler_interface_version, compiler_processor_type);

  return
    (((FASLHDR_CC_ARCH (h)) == compiler_processor_type)
     ? FASL_FILE_BAD_INTERFACE
     : FASL_FILE_BAD_PROCESSOR);
}

static void
print_fasl_information (fasl_header_t * h)
{
  printf ("FASL file information:\n");
  printf ("\n");
  printf ("architecture = %u; version = %u\n",
	  (FASLHDR_ARCH (h)),
	  (FASLHDR_VERSION (h)));
  if (! (((FASLHDR_CC_VERSION (h)) == 0)
	 && ((FASLHDR_CC_ARCH (h)) == COMPILER_NONE_TYPE)))
    printf ("compiled-code interface version = %u; architecture = %u\n",
	    (FASLHDR_CC_VERSION (h)),
	    (FASLHDR_CC_ARCH (h)));
  if (FASLHDR_BAND_P (h))
    printf ("the file contains a world image (band)\n");

  printf ("\n");

  printf ("relocation information:\n");
  printf ("\n");
  printf ("heap: start = %#lx; end = %#lx; size = %lu\n",
	  ((unsigned long) (FASLHDR_HEAP_START (h))),
	  ((unsigned long) (FASLHDR_HEAP_END (h))),
	  (FASLHDR_HEAP_SIZE (h)));
  printf ("constant: start = %#lx; end = %#lx; size = %lu\n",
	  ((unsigned long) (FASLHDR_CONSTANT_START (h))),
	  ((unsigned long) (FASLHDR_CONSTANT_END (h))),
	  (FASLHDR_CONSTANT_SIZE (h)));
  printf ("stack: start = %#lx; end = %#lx; size = %lu\n",
	  ((unsigned long) (FASLHDR_STACK_START (h))),
	  ((unsigned long) (FASLHDR_STACK_END (h))),
	  (FASLHDR_STACK_SIZE (h)));

  printf ("\n");

  printf ("objects:\n");
  printf ("\n");
  printf ("root object at %#lx\n",
	  ((unsigned long) (FASLHDR_ROOT_POINTER (h))));
  printf ("compiled-code utilities = %#lx\n",
	  (FASLHDR_UTILITIES_VECTOR (h)));
  printf ("number of primitives = %lu\n",
	  (FASLHDR_N_PRIMITIVES (h)));
}

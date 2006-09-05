/* -*-C-*-

$Id: fasload.c,v 9.96.2.11 2006/09/05 19:10:17 cph Exp $

Copyright 1986,1987,1988,1989,1990,1991 Massachusetts Institute of Technology
Copyright 1992,1993,1994,1995,1996,1997 Massachusetts Institute of Technology
Copyright 1998,2000,2001,2002,2005,2006 Massachusetts Institute of Technology

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

static fasl_header_t fasl_header;
static fasl_header_t * fh;
static SCHEME_OBJECT * new_heap_start;
static SCHEME_OBJECT * new_constant_start;
static SCHEME_OBJECT * new_stack_start;
static SCHEME_OBJECT * new_stack_end;
static SCHEME_OBJECT * new_utilities;
static SCHEME_OBJECT * new_prim_table;

#define REQUIRED_HEAP(h)						\
  ((FASLHDR_HEAP_SIZE (h))						\
   + (FASLHDR_N_PRIMITIVES (h))						\
   + (FASLHDR_PRIMITIVE_TABLE_SIZE (h)))

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

static void init_fasl_file (const char *, bool, fasl_file_handle_t *);
static void close_fasl_file (void *);
static fasl_read_status_t check_fasl_version (void);
static fasl_read_status_t check_fasl_cc_version (void);

static SCHEME_OBJECT load_file (fasl_file_handle_t);
static void * read_from_file (void *, size_t, fasl_file_handle_t);
static bool primitive_numbers_unchanged_p (SCHEME_OBJECT *);

static gc_table_t * relocate_block_table (void);
static gc_handler_t handle_primitive;
static gc_tuple_handler_t fasload_tuple;
static gc_vector_handler_t fasload_vector;
static gc_object_handler_t fasload_cc_entry;
static gc_object_handler_t fasload_weak_pair;
static void * relocate_address (void *);

static gc_table_t * intern_block_table (void);
static gc_handler_t intern_handle_symbol;
static gc_tuple_handler_t intern_tuple;
static gc_vector_handler_t intern_vector;
static gc_object_handler_t intern_cc_entry;
static gc_object_handler_t intern_weak_pair;

static SCHEME_OBJECT read_band_file (SCHEME_OBJECT);
static void terminate_band_load (void *);

DEFINE_PRIMITIVE ("BINARY-FASLOAD", Prim_binary_fasload, 1, 1, "(NAMESTRING)\n\
Load the contents of the file NAMESTRING into memory.  The file was\n\
presumably made by a call to PRIMITIVE-FASDUMP, and may contain data\n\
for the heap and/or the pure area.  The value returned is the object\n\
that was dumped.")
{
  fasl_file_handle_t handle;
  static unsigned long failed_heap_length = 0;
  unsigned long heap_length;
  SCHEME_OBJECT result;
  PRIMITIVE_HEADER (1);

  canonicalize_primitive_context ();
  transaction_begin ();

  init_fasl_file ((STRING_ARG (1)), false, (&handle));
  if ((FASLHDR_CONSTANT_SIZE (fh)) > 0)
    signal_error_from_primitive (ERR_FASL_FILE_TOO_BIG);

  heap_length = (REQUIRED_HEAP (fh));
  if (GC_NEEDED_P (heap_length))
    {
      if (heap_length == failed_heap_length)
	signal_error_from_primitive (ERR_FASL_FILE_TOO_BIG);
      failed_heap_length = heap_length;
      REQUEST_GC (heap_length);
      signal_interrupt_from_primitive ();
    }
  failed_heap_length = 0;

  result = (load_file (handle));
  transaction_commit ();
  PRIMITIVE_RETURN (result);
}

static void
init_fasl_file (const char * file_name, bool band_p,
		fasl_file_handle_t * handle)
{
  if (!open_fasl_input_file (file_name, handle))
    error_bad_range_arg (1);
  transaction_record_action (tat_always, close_fasl_file, handle);

  fh = (&fasl_header);
  if (!read_fasl_header (fh, (*handle)))
    signal_error_from_primitive (ERR_FASL_FILE_BAD_DATA);

#ifndef INHIBIT_FASL_VERSION_CHECK
  if ((check_fasl_version ()) != FASL_FILE_FINE)
    signal_error_from_primitive (ERR_FASL_FILE_BAD_DATA);
#endif

#ifndef INHIBIT_COMPILED_VERSION_CHECK
  if ((check_fasl_cc_version ()) != FASL_FILE_FINE)
    signal_error_from_primitive (ERR_FASLOAD_COMPILED_MISMATCH);
#endif

  if ((FASLHDR_BAND_P (fh)) != band_p)
    signal_error_from_primitive (ERR_FASLOAD_BAND);
}

static void
close_fasl_file (void * p)
{
  (void) close_fasl_input_file (* ((fasl_file_handle_t *) p));
}

/* The error messages here should be handled by the runtime system! */

static fasl_read_status_t
check_fasl_version (void)
{
  if (((FASLHDR_VERSION (fh)) >= INPUT_FASL_VERSION)
      && ((FASLHDR_VERSION (fh)) <= CURRENT_FASL_VERSION))
    return (FASL_FILE_FINE);

  outf_error
    ("\nFASL File: Version %4u Architecture %4u.\n",
     (FASLHDR_VERSION (fh)), (FASLHDR_ARCH (fh)));
  outf_error
    ("Expected:  Version %4u Architecture %4u.\n",
     INPUT_FASL_VERSION,
     CURRENT_FASL_ARCH);

  return (((FASLHDR_ARCH (fh)) != CURRENT_FASL_ARCH)
	  ? FASL_FILE_BAD_MACHINE
	  : FASL_FILE_BAD_VERSION);
}

static fasl_read_status_t
check_fasl_cc_version (void)
{
  if ((((FASLHDR_CC_VERSION (fh)) == 0)
       && ((FASLHDR_CC_ARCH (fh)) == COMPILER_NONE_TYPE))
      || (((FASLHDR_CC_VERSION (fh)) == compiler_interface_version)
	  && ((FASLHDR_CC_ARCH (fh)) == compiler_processor_type)))
    return (FASL_FILE_FINE);

  outf_error
    ("\nFASL File: compiled-code interface %4u; architecture %4u.\n",
     (FASLHDR_CC_VERSION (fh)), (FASLHDR_CC_ARCH (fh)));
  outf_error
    ("Expected:  compiled code interface %4ld; architecture %4ld.\n",
     compiler_interface_version, compiler_processor_type);

  return
    (((FASLHDR_CC_ARCH (fh)) == compiler_processor_type)
     ? FASL_FILE_BAD_INTERFACE
     : FASL_FILE_BAD_PROCESSOR);
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

  /* Reset implementation state parameters.  */
  INITIALIZE_INTERRUPTS (0);
#ifdef CC_SUPPORT_P
  compiler_utilities = (PAIR_CDR (result));
  if (compiler_utilities != SHARP_F)
    compiler_reset (compiler_utilities);
  else
    compiler_initialize (true);
#endif
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
  fasl_file_handle_t handle;
  SCHEME_OBJECT result;
  void * old_name;

  transaction_begin ();
  file_name = (OS_malloc ((STRING_LENGTH (s)) + 1));
  strcpy (((char *) file_name), (STRING_POINTER (s)));
  state = (dstack_alloc (sizeof (struct load_band_termination_state)));
  (state->file_name) = file_name;
  (state->no_return_p) = false;
  transaction_record_action (tat_abort, terminate_band_load, state);

  init_fasl_file (file_name, true, (&handle));
  if (!allocations_ok_p ((FASLHDR_CONSTANT_SIZE (fh)),
			 (REQUIRED_HEAP (fh))))
    signal_error_from_primitive (ERR_FASL_FILE_TOO_BIG);

  /* Now read the file into memory.  Past this point we can't abort
     and return to the old image.  */
  ENTER_CRITICAL_SECTION ("band load");
  (state->no_return_p) = true;

  reset_allocator_parameters (FASLHDR_CONSTANT_SIZE (fh));
  result = (load_file (handle));

  /* Done -- we have the new image.  */
  transaction_commit ();

  /* Save the band name for possible later use.  */
  old_name = ((void *) reload_band_name);
  reload_band_name = file_name;
  if (old_name != 0)
    OS_free (old_name);

  return (result);
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
    {
      const char * message
	= ((abort_value <= MAX_ERROR)
	   ? (Error_Names[abort_value])
	   : 0);
      if (message == 0)
	outf_fatal ("Unknown error %#lx", ((unsigned long) abort_value));
      else
	outf_fatal ("Error %#lx (%s)", ((unsigned long) abort_value), message);
    }
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

static SCHEME_OBJECT
load_file (fasl_file_handle_t handle)
{
  new_heap_start = Free;
  new_constant_start = constant_alloc_next;
  new_stack_start = stack_start;
  new_stack_end = stack_end;
  new_utilities
    = ((compiler_utilities == SHARP_F)
       ? 0
       : (OBJECT_ADDRESS (compiler_utilities)));

  Free = (read_from_file (Free, (FASLHDR_HEAP_SIZE (fh)), handle));
  constant_alloc_next
    = (read_from_file (constant_alloc_next,
		       (FASLHDR_CONSTANT_SIZE (fh)),
		       handle));

  new_prim_table = Free;
  {
    SCHEME_OBJECT * raw_prim_table = (Free + (FASLHDR_N_PRIMITIVES (fh)));
    read_from_file (raw_prim_table,
		    (FASLHDR_PRIMITIVE_TABLE_SIZE (fh)),
		    handle);
    import_primitive_table
      (raw_prim_table, (FASLHDR_N_PRIMITIVES (fh)), new_prim_table);
  }

  if ((!FASLHDR_BAND_P (fh))
      && ((FASLHDR_UTILITIES_VECTOR (fh)) != SHARP_F)
      && (compiler_utilities == SHARP_F))
    /* The file contains compiled code, but there's no compiled-code
       support available.  */
    signal_error_from_primitive (ERR_FASLOAD_COMPILED_MISMATCH);

  if (! ((FASLHDR_BAND_P (fh))
	 && ((FASLHDR_HEAP_START (fh)) == new_heap_start)
	 && (((FASLHDR_CONSTANT_START (fh)) == new_constant_start)
	     || ((FASLHDR_CONSTANT_START (fh)) == (FASLHDR_CONSTANT_END (fh))))
	 && (((FASLHDR_STACK_START (fh)) == 0)
	     || ((FASLHDR_STACK_START (fh)) == new_stack_start))
	 && ((FASLHDR_STACK_END (fh)) == new_stack_end)
	 && (primitive_numbers_unchanged_p (new_prim_table))))
    {
      current_gc_table = (relocate_block_table ());
      gc_scan_oldspace (new_heap_start, Free);
      gc_scan_oldspace (new_constant_start, constant_alloc_next);
    }
  if (!FASLHDR_BAND_P (fh))
    {
      current_gc_table = (intern_block_table ());
      gc_scan_oldspace (new_heap_start, Free);
      gc_scan_oldspace (new_constant_start, constant_alloc_next);
    }

#ifdef PUSH_D_CACHE_REGION
  if ((FASLHDR_CC_VERSION (fh)) != COMPILER_NONE_TYPE)
    {
      if ((FASLHDR_HEAP_SIZE (fh)) > 0)
	PUSH_D_CACHE_REGION (new_heap_start, (FASLHDR_HEAP_SIZE (fh)));
      if ((FASLHDR_CONSTANT_SIZE (fh)) > 0)
	PUSH_D_CACHE_REGION (new_constant_start, (FASLHDR_CONSTANT_SIZE (fh)));
    }
#endif

  return
    (* ((SCHEME_OBJECT *)
	(relocate_address (FASLHDR_ROOT_POINTER (fh)))));
}

static void *
read_from_file (void * p, size_t n_words, fasl_file_handle_t handle)
{
  if (!read_from_fasl_file (p, n_words, handle))
    signal_error_from_primitive (ERR_FASL_FILE_BAD_DATA);
  return (((char *) p) + (n_words * SIZEOF_SCHEME_OBJECT));
}

static bool
primitive_numbers_unchanged_p (SCHEME_OBJECT * table)
{
  unsigned long count;

  for (count = 0; (count < (FASLHDR_N_PRIMITIVES (fh))); count += 1)
    if ((table[count]) != (MAKE_PRIMITIVE_OBJECT (count)))
      return (false);
  return (true);
}

static gc_table_t *
relocate_block_table (void)
{
  static bool initialized_p = false;
  static gc_table_t table;

  if (!initialized_p)
    {
      initialize_gc_table ((&table), false);

      (GCT_TUPLE (&table)) = fasload_tuple;
      (GCT_VECTOR (&table)) = fasload_vector;
      (GCT_CC_ENTRY (&table)) = fasload_cc_entry;
      (GCT_WEAK_PAIR (&table)) = fasload_weak_pair;

      (GCT_ENTRY ((&table), TC_PRIMITIVE)) = handle_primitive;
      (GCT_ENTRY ((&table), TC_PCOMB0)) = handle_primitive;
      (GCT_ENTRY ((&table), TC_BROKEN_HEART)) = gc_handle_non_pointer;
      (GCT_ENTRY ((&table), TC_MANIFEST_SPECIAL_NM_VECTOR))
	= gc_handle_non_pointer;

      initialized_p = true;
    }
  return (&table);
}

static
DEFINE_GC_HANDLER (handle_primitive)
{
  unsigned long datum = (OBJECT_DATUM (object));
  unsigned long high_bits = (datum >> HALF_DATUM_LENGTH);
  (*scan)
    = (MAKE_OBJECT_FROM_OBJECTS
       (object,
	(new_prim_table [((high_bits != 0) ? high_bits : datum)])));
  return (scan + 1);
}

#define RELOCATE_OBJECT(object)						\
  (OBJECT_NEW_ADDRESS ((object),					\
		       (relocate_address (OBJECT_ADDRESS (object)))))

static
DEFINE_GC_TUPLE_HANDLER (fasload_tuple)
{
  return (RELOCATE_OBJECT (tuple));
}

static
DEFINE_GC_VECTOR_HANDLER (fasload_vector)
{
  return (RELOCATE_OBJECT (vector));
}

static
DEFINE_GC_OBJECT_HANDLER (fasload_cc_entry)
{
#ifdef CC_SUPPORT_P
  return
    (CC_ENTRY_NEW_ADDRESS (object,
			   (relocate_address (CC_ENTRY_ADDRESS (object)))));
#else
  return (object);
#endif
}

static
DEFINE_GC_OBJECT_HANDLER (fasload_weak_pair)
{
  return (RELOCATE_OBJECT (object));
}

/* Relocate an address as read in from the file.  The address is
   examined to see what region of memory it belongs in.  */

static void *
relocate_address (void * vaddr)
{
  byte_t * caddr = vaddr;
  byte_t * result;

  if ((caddr >= ((byte_t *) (FASLHDR_HEAP_START (fh))))
      && (caddr < ((byte_t *) (FASLHDR_HEAP_END (fh)))))
    result
      = (((byte_t *) new_heap_start)
	 + (caddr - ((byte_t *) (FASLHDR_HEAP_START (fh)))));
  else if ((caddr >= ((byte_t *) (FASLHDR_CONSTANT_START (fh))))
	   && (caddr < ((byte_t *) (FASLHDR_CONSTANT_END (fh)))))
    result
      = (((byte_t *) new_constant_start)
	 + (caddr - ((byte_t *) (FASLHDR_CONSTANT_START (fh)))));
  else if ((caddr >= ((byte_t *) (FASLHDR_UTILITIES_START (fh))))
	   && (caddr < ((byte_t *) (FASLHDR_UTILITIES_END (fh)))))
    result
      = (((byte_t *) new_utilities)
	 + (caddr - ((byte_t *) (FASLHDR_UTILITIES_START (fh)))));
  else if (ADDRESS_IN_STACK_REGION_P (caddr,
				      ((byte_t *) (FASLHDR_STACK_START (fh))),
				      ((byte_t *) (FASLHDR_STACK_END (fh)))))
    result
      = (N_PUSHED_TO_SP
	 ((SP_TO_N_PUSHED (caddr,
			   ((byte_t *) (FASLHDR_STACK_START (fh))),
			   ((byte_t *) (FASLHDR_STACK_END (fh))))),
	  ((byte_t *) new_stack_start),
	  ((byte_t *) new_stack_end)));
  else
    {
      outf_fatal ("Pointer out of range: %#lx\n", ((unsigned long) caddr));
      outf_fatal ("Heap: %#lx-%#lx, Constant: %#lx-%#lx, Stack: %#lx-%#lx\n",
		  ((unsigned long) (FASLHDR_HEAP_START (fh))),
		  ((unsigned long) (FASLHDR_HEAP_END (fh))),
		  ((unsigned long) (FASLHDR_CONSTANT_START (fh))),
		  ((unsigned long) (FASLHDR_CONSTANT_END (fh))),
		  ((unsigned long) (FASLHDR_STACK_START (fh))),
		  ((unsigned long) (FASLHDR_STACK_END (fh))));
      termination_init_error ();
    }
  return (result);
}

static gc_table_t *
intern_block_table (void)
{
  static bool initialized_p = false;
  static gc_table_t table;

  if (!initialized_p)
    {
      initialize_gc_table ((&table), false);

      (GCT_TUPLE (&table)) = intern_tuple;
      (GCT_VECTOR (&table)) = intern_vector;
      (GCT_CC_ENTRY (&table)) = intern_cc_entry;
      (GCT_WEAK_PAIR (&table)) = intern_weak_pair;

      (GCT_ENTRY ((&table), TC_INTERNED_SYMBOL)) = intern_handle_symbol;
      (GCT_ENTRY ((&table), TC_BROKEN_HEART)) = gc_handle_non_pointer;
      (GCT_ENTRY ((&table), TC_MANIFEST_SPECIAL_NM_VECTOR))
	= gc_handle_non_pointer;

      initialized_p = true;
    }

  return (&table);
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

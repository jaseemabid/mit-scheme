/* -*-C-*-

$Id: gccode.h,v 9.60.2.12 2007/01/06 00:09:57 cph Exp $

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007 Massachusetts Institute of Technology

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

/* This file contains the macros for use in code which does GC-like
   loops over memory.  It is only included in a few files, unlike
   gc.h which contains general purpose macros and constants. */

#ifndef SCM_GCCODE_H
#define SCM_GCCODE_H 1

#include "gc.h"
#include "cmpgc.h"
#include "fasl.h"

#ifdef ENABLE_DEBUGGING_TOOLS
#  ifndef ENABLE_GC_DEBUGGING_TOOLS
#    define ENABLE_GC_DEBUGGING_TOOLS
#  endif
#endif

typedef SCHEME_OBJECT * gc_handler_t
  (SCHEME_OBJECT *, SCHEME_OBJECT);

#define DEFINE_GC_HANDLER(handler_name)					\
SCHEME_OBJECT *								\
handler_name (SCHEME_OBJECT * scan, SCHEME_OBJECT object)

typedef SCHEME_OBJECT gc_tuple_handler_t
  (SCHEME_OBJECT, unsigned int);

#define DEFINE_GC_TUPLE_HANDLER(handler_name)				\
SCHEME_OBJECT								\
handler_name (SCHEME_OBJECT tuple, unsigned int n_words)

typedef SCHEME_OBJECT gc_vector_handler_t
  (SCHEME_OBJECT, bool);

#define DEFINE_GC_VECTOR_HANDLER(handler_name)				\
SCHEME_OBJECT								\
handler_name (SCHEME_OBJECT vector, bool align_p)

typedef SCHEME_OBJECT gc_object_handler_t
  (SCHEME_OBJECT);

#define DEFINE_GC_OBJECT_HANDLER(handler_name)				\
SCHEME_OBJECT								\
handler_name (SCHEME_OBJECT object)

typedef SCHEME_OBJECT * gc_precheck_from_t (SCHEME_OBJECT *);

#define DEFINE_GC_PRECHECK_FROM(handler_name)				\
SCHEME_OBJECT *								\
handler_name (SCHEME_OBJECT * from)

typedef SCHEME_OBJECT * gc_transport_words_t
  (SCHEME_OBJECT *, unsigned long, bool);

#define DEFINE_GC_TRANSPORT_WORDS(handler_name)				\
SCHEME_OBJECT *								\
handler_name (SCHEME_OBJECT * from, unsigned long n_words, bool align_p)

typedef bool gc_ignore_object_p_t (SCHEME_OBJECT);

#define DEFINE_GC_IGNORE_OBJECT_P(handler_name)				\
bool									\
handler_name (SCHEME_OBJECT object)

typedef struct
{
  gc_handler_t * handlers [N_TYPE_CODES];
  gc_tuple_handler_t * tuple_handler;
  gc_vector_handler_t * vector_handler;
  gc_object_handler_t * cc_entry_handler;
  gc_precheck_from_t * precheck_from;
  gc_transport_words_t * transport_words;
  gc_ignore_object_p_t * ignore_object_p;
} gc_table_t;

#define GCT_ENTRY(table, type) (((table)->handlers) [(type)])
#define GCT_TUPLE(table) ((table)->tuple_handler)
#define GCT_VECTOR(table) ((table)->vector_handler)
#define GCT_CC_ENTRY(table) ((table)->cc_entry_handler)
#define GCT_PRECHECK_FROM(table) ((table)->precheck_from)
#define GCT_TRANSPORT_WORDS(table) ((table)->transport_words)
#define GCT_IGNORE_OBJECT_P(table) ((table)->ignore_object_p)

#define GC_HANDLE_TUPLE(object, n_words)				\
  ((* (GCT_TUPLE (current_gc_table))) ((object), (n_words)))

#define GC_HANDLE_VECTOR(object, align_p)				\
  ((* (GCT_VECTOR (current_gc_table))) ((object), (align_p)))

#define GC_HANDLE_CC_ENTRY(object)					\
  ((* (GCT_CC_ENTRY (current_gc_table))) ((object)))

#define GC_PRECHECK_FROM(from)						\
  ((* (GCT_PRECHECK_FROM (current_gc_table))) ((from)))

#define GC_TRANSPORT_WORDS(from, n_words, align_p)			\
  ((* (GCT_TRANSPORT_WORDS (current_gc_table))) ((from), (n_words), (align_p)))

extern gc_table_t * current_gc_table;

extern gc_handler_t gc_handle_non_pointer;
extern gc_handler_t gc_handle_cell;
extern gc_handler_t gc_handle_pair;
extern gc_handler_t gc_handle_triple;
extern gc_handler_t gc_handle_quadruple;
extern gc_handler_t gc_handle_weak_pair;
extern gc_handler_t gc_handle_cc_entry;
extern gc_handler_t gc_handle_aligned_vector;
extern gc_handler_t gc_handle_unaligned_vector;
extern gc_handler_t gc_handle_broken_heart;
extern gc_handler_t gc_handle_nmv;
extern gc_handler_t gc_handle_reference_trap;
extern gc_handler_t gc_handle_linkage_section;
extern gc_handler_t gc_handle_manifest_closure;
extern gc_handler_t gc_handle_undefined;

extern gc_tuple_handler_t gc_tuple;
extern gc_vector_handler_t gc_vector;
extern gc_object_handler_t gc_cc_entry;
extern gc_precheck_from_t gc_precheck_from;
extern gc_precheck_from_t gc_precheck_from_no_transport;
extern gc_transport_words_t gc_transport_words;
extern gc_transport_words_t gc_no_transport_words;

extern void initialize_gc_table (gc_table_t *, bool);

typedef void gc_tospace_allocator_t
  (unsigned long, SCHEME_OBJECT **, SCHEME_OBJECT **);
typedef void gc_abort_handler_t (void);
typedef bool gc_walk_proc_t (SCHEME_OBJECT *, SCHEME_OBJECT *, void *);

extern void initialize_gc
  (unsigned long, SCHEME_OBJECT **, SCHEME_OBJECT **,
   gc_tospace_allocator_t *, gc_abort_handler_t * NORETURN);

extern void resize_tospace (unsigned long);
extern void open_tospace (SCHEME_OBJECT *);
extern bool tospace_available_p (unsigned long);
extern void add_to_tospace (SCHEME_OBJECT);
extern SCHEME_OBJECT read_tospace (SCHEME_OBJECT *);
extern void write_tospace (SCHEME_OBJECT *, SCHEME_OBJECT);
extern void increment_tospace_ptr (unsigned long);
extern SCHEME_OBJECT * get_newspace_ptr (void);
extern void * tospace_to_newspace (void *);
extern void * newspace_to_tospace (void *);
extern bool save_tospace (gc_walk_proc_t *, void *);

extern void initialize_weak_chain (void);
extern void update_weak_pointers (void);

extern gc_table_t * std_gc_table (void);
extern void gc_scan_oldspace (SCHEME_OBJECT *, SCHEME_OBJECT *);
extern void gc_scan_tospace (SCHEME_OBJECT *, SCHEME_OBJECT *);

extern void std_gc_death (const char *, ...)
  ATTRIBUTE ((__noreturn__, __format__ (__printf__, 1, 2)));
extern void gc_no_cc_support (void) NORETURN;
extern void gc_bad_type (SCHEME_OBJECT) NORETURN;

#ifdef ENABLE_GC_DEBUGGING_TOOLS
   extern void collect_gc_object_references (SCHEME_OBJECT, SCHEME_OBJECT);
   extern void initialize_gc_object_references (void);
   extern void finalize_gc_object_references (void);
#endif

#endif /* not SCM_GCCODE_H */

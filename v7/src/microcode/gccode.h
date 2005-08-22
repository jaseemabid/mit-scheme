/* -*-C-*-

$Id: gccode.h,v 9.60.2.1 2005/08/22 18:05:58 cph Exp $

Copyright 1986,1987,1988,1989,1991,1992 Massachusetts Institute of Technology
Copyright 1993,1995,1997,2000,2001,2002 Massachusetts Institute of Technology
Copyright 2005 Massachusetts Institute of Technology

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

/* This file contains the macros for use in code which does GC-like
   loops over memory.  It is only included in a few files, unlike
   gc.h which contains general purpose macros and constants. */

#ifndef SCM_GCCODE_H
#define SCM_GCCODE_H 1

#include "cmpgc.h"

#ifdef ENABLE_DEBUGGING_TOOLS
#  ifndef ENABLE_GC_DEBUGGING_TOOLS
#    define ENABLE_GC_DEBUGGING_TOOLS
#  endif
#endif

typedef struct gc_table_s gc_table_t;

typedef struct
{
  gc_table_t * table;		/* gc dispatch table */
  SCHEME_OBJECT ** pto;		/* pointer to 'to' */
  SCHEME_OBJECT * scan;		/* scan value where object found */
  SCHEME_OBJECT object;		/* original object being processed */
} gc_ctx_t;

#define GCTX_TABLE(ctx) ((ctx)->table)
#define GCTX_PTO(ctx) ((ctx)->pto)
#define GCTX_SCAN(ctx) ((ctx)->scan)
#define GCTX_OBJECT(ctx) ((ctx)->object)

typedef SCHEME_OBJECT * gc_handler_t
  (SCHEME_OBJECT *, SCHEME_OBJECT, gc_ctx_t *);

#define DEFINE_GC_HANDLER(handler_name)					\
SCHEME_OBJECT *								\
handler_name (SCHEME_OBJECT * scan, SCHEME_OBJECT object, gc_ctx_t * ctx)

typedef SCHEME_OBJECT gc_tuple_handler_t
  (SCHEME_OBJECT, unsigned int, gc_ctx_t *);

#define DEFINE_GC_TUPLE_HANDLER(handler_name)				\
SCHEME_OBJECT								\
handler_name (SCHEME_OBJECT tuple, unsigned int n_words, gc_ctx_t * ctx)

typedef SCHEME_OBJECT gc_vector_handler_t
  (SCHEME_OBJECT, bool, gc_ctx_t *);

#define DEFINE_GC_VECTOR_HANDLER(handler_name)				\
SCHEME_OBJECT								\
handler_name (SCHEME_OBJECT vector, bool align_p, gc_ctx_t * ctx)

typedef SCHEME_OBJECT gc_object_handler_t
  (SCHEME_OBJECT, gc_ctx_t *);

#define DEFINE_GC_OBJECT_HANDLER(handler_name)				\
SCHEME_OBJECT								\
handler_name (SCHEME_OBJECT object, gc_ctx_t * ctx)

struct gc_table_s
{
  gc_handler_t * handlers [N_TYPE_CODES];
  gc_tuple_handler_t * tuple_handler;
  gc_vector_handler_t * vector_handler;
  gc_object_handler_t * cc_entry_handler;
  gc_object_handler_t * weak_pair_handler;
};

#define GCT_ENTRY(table, type) (((table)->handlers) [(type)])
#define GCT_TUPLE(table) ((table)->tuple_handler)
#define GCT_VECTOR(table) ((table)->vector_handler)
#define GCT_CC_ENTRY(table) ((table)->cc_entry_handler)
#define GCT_WEAK_PAIR(table) ((table)->weak_pair_handler)

#define GC_HANDLE_TUPLE(object, n_words, ctx)				\
  ((* (GCT_TUPLE ((ctx)->table))) ((object), (n_words), (ctx)))

#define GC_HANDLE_VECTOR(object, align_p, ctx)				\
  ((* (GCT_VECTOR ((ctx)->table))) ((object), (align_p), (ctx)))

#define GC_HANDLE_CC_ENTRY(object, ctx)					\
  ((* (GCT_CC_ENTRY ((ctx)->table))) ((object), (ctx)))

extern gc_handler_t gc_handle_non_pointer;
extern gc_handler_t gc_handle_cell;
extern gc_handler_t gc_handle_pair;
extern gc_handler_t gc_handle_triple;
extern gc_handler_t gc_handle_quadruple;
extern gc_handler_t gc_handle_cc_entry;
extern gc_handler_t gc_handle_aligned_vector;
extern gc_handler_t gc_handle_unaligned_vector;
extern gc_handler_t gc_handle_broken_heart;
extern gc_handler_t gc_handle_nmv;
extern gc_handler_t gc_handle_reference_trap;
extern gc_handler_t gc_handle_linkage_section;
extern gc_handler_t gc_handle_manifest_closure;
extern gc_handler_t gc_handle_undefined;

extern void initialize_gc_table
  (gc_table_t *, gc_tuple_handler_t *, gc_vector_handler_t *,
   gc_object_handler_t *, gc_object_handler_t *);

extern void run_gc_loop (SCHEME_OBJECT *, SCHEME_OBJECT **, gc_ctx_t *);

extern SCHEME_OBJECT * gc_transport_words
  (SCHEME_OBJECT *, unsigned long, bool, gc_ctx_t *);

extern SCHEME_OBJECT gc_transport_weak_pair (SCHEME_OBJECT, gc_ctx_t *);

#ifndef BAD_TYPES_INNOCUOUS

#define GC_BAD_TYPE(object, scan, pto)					\
  (gc_death								\
   (TERM_INVALID_TYPE_CODE, (scan), (pto),				\
    "bad type code: %#02lx %#lx", (OBJECT_TYPE (object)), (object)))

#else

#define GC_BAD_TYPE(object, scan, pto)					\
  (outf_error								\
   ("\nbad type code: %#02lx %#lx -- treating as non-pointer.\n",	\
    (OBJECT_TYPE (object)), (object)))

#endif

#ifdef ENABLE_GC_DEBUGGING_TOOLS
   extern void debug_transport_one_word (SCHEME_OBJECT, SCHEME_OBJECT *);
#  define DEBUG_TRANSPORT_ONE_WORD debug_transport_one_word
#else
#  define DEBUG_TRANSPORT_ONE_WORD(object, from) do {} while (0)
#endif

#define PRESERVE_OLD_SPACE_LIMITS() do					\
{									\
  old_space_start = active_heap_start;					\
  old_space_end = active_heap_end;					\
} while (0)

#define ADDRESS_IN_OLD_SPACE_P(address)					\
  (((address) >= old_space_start) && ((address) < old_space_end))

extern SCHEME_OBJECT * old_space_start;
extern SCHEME_OBJECT * old_space_end;

extern void garbage_collect (void);
extern void gc_loop (SCHEME_OBJECT *, SCHEME_OBJECT **, SCHEME_OBJECT **);

extern void initialize_weak_chain (void);
extern void note_weak_pair (SCHEME_OBJECT, SCHEME_OBJECT *);
extern void update_weak_pointers (void);

#ifdef ENABLE_GC_DEBUGGING_TOOLS
extern void initialize_gc_history (void);
extern void handle_gc_trap (SCHEME_OBJECT *, SCHEME_OBJECT **, SCHEME_OBJECT);
#endif

extern void gc_death
  (long, SCHEME_OBJECT *, SCHEME_OBJECT **, const char *, ...)
  ATTRIBUTE ((__noreturn__, __format__ (__printf__, 4, 5)));

#endif /* not SCM_GCCODE_H */

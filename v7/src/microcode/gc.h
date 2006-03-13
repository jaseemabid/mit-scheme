/* -*-C-*-

$Id: gc.h,v 9.36.2.3 2006/03/13 05:15:39 cph Exp $

Copyright 1986,1987,1988,1989,1992,1993 Massachusetts Institute of Technology
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

/* Garbage collection related macros of sufficient utility to be
   included in all compilations.  */

#ifndef SCM_GC_H
#define SCM_GC_H 1

/* GC Types. */

typedef enum
{
  GC_COMPILED = -4,
  GC_VECTOR,
  GC_SPECIAL,			/* Internal GC types */
  GC_UNDEFINED,
  GC_NON_POINTER,
  GC_CELL,
  GC_PAIR,
  GC_TRIPLE,
  GC_QUADRUPLE
} gc_type_t;

#define GC_TYPE_TO_INT(type) ((int) (type))

#ifdef BAD_TYPES_INNOCUOUS
#  define GC_TYPE_CODE(type) (gc_type_map[(type)])
#else
#  define GC_TYPE_CODE gc_type_code
   extern gc_type_t gc_type_code (unsigned int);
#endif

#define GC_TYPE(object) (GC_TYPE_CODE (OBJECT_TYPE (object)))

extern gc_type_t gc_type_map [];

#define GC_TYPE_NON_POINTER(object)	((GC_TYPE (object)) == GC_NON_POINTER)
#define GC_TYPE_CELL(object)		((GC_TYPE (object)) == GC_CELL)
#define GC_TYPE_PAIR(object)		((GC_TYPE (object)) == GC_PAIR)
#define GC_TYPE_TRIPLE(object)		((GC_TYPE (object)) == GC_TRIPLE)
#define GC_TYPE_QUADRUPLE(object)	((GC_TYPE (object)) == GC_QUADRUPLE)
#define GC_TYPE_UNDEFINED(object)	((GC_TYPE (object)) == GC_UNDEFINED)
#define GC_TYPE_SPECIAL(object)		((GC_TYPE (object)) == GC_SPECIAL)
#define GC_TYPE_VECTOR(object)		((GC_TYPE (object)) == GC_VECTOR)
#define GC_TYPE_COMPILED(object)	((GC_TYPE (object)) == GC_COMPILED)

typedef enum
{
  GC_POINTER_NORMAL,
  GC_POINTER_COMPILED,
  GC_POINTER_NOT
} gc_ptr_type_t;

extern gc_ptr_type_t gc_ptr_type (SCHEME_OBJECT);
extern SCHEME_OBJECT * get_object_address (SCHEME_OBJECT);
extern bool object_in_active_heap_p (SCHEME_OBJECT);
extern bool object_in_constant_space_p (SCHEME_OBJECT);
extern bool object_pure_p (SCHEME_OBJECT);

/* Overflow detection, various cases */

#define GC_ENABLED_P() (INTERRUPT_ENABLED_P (INT_GC))

#define HEAP_AVAILABLE							\
  ((unsigned long) ((FREE_OK_P (Free)) ? (heap_alloc_limit - Free) : 0))

#define FREE_OK_P(free)							\
  (((free) >= active_heap_start) && ((free) < heap_alloc_limit))

#define HEAP_AVAILABLE_P(n_words)					\
  ((FREE_OK_P (Free)) && ((Free + (n_words)) <= heap_alloc_limit))

#define GC_NEEDED_P(n_words)						\
  ((!HEAP_AVAILABLE_P (n_words)) && (GC_ENABLED_P ()))

#define SPACE_BEFORE_GC()						\
  ((GC_ENABLED_P ())							\
   ? HEAP_AVAILABLE							\
   : (ADDRESS_IN_ACTIVE_HEAP_P (Free))					\
   ? ((unsigned long) (active_heap_end - Free))				\
   : 0)

#define REQUEST_GC(n_words) do						\
{									\
  REQUEST_INTERRUPT (INT_GC);						\
  gc_space_needed = (n_words);						\
} while (0)

#define SET_HEAP_ALLOC_LIMIT(addr) do					\
{									\
  heap_alloc_limit = (addr);						\
  COMPILER_SETUP_INTERRUPT ();						\
} while (0)

#define RESET_HEAP_ALLOC_LIMIT()					\
  SET_HEAP_ALLOC_LIMIT (active_heap_end - active_heap_reserved)

#define ARG_HEAP_RESERVED(n)						\
  (arg_ulong_index_integer ((n), ((active_heap_end - active_heap_start) / 2)))

#define ADDRESS_IN_MEMORY_BLOCK_P(address)				\
  (((address) >= memory_block_start) && ((address) < memory_block_end))

#define ADDRESS_IN_ACTIVE_HEAP_P(address)				\
  (((address) >= active_heap_start) && ((address) < active_heap_end))

#define ADDRESS_IN_INACTIVE_HEAP_P(address)				\
  (((address) >= inactive_heap_start) && ((address) < inactive_heap_end))

#define ADDRESS_IN_STACK_P(address)					\
  (((address) >= stack_start) && ((address) < stack_end))

#define ADDRESS_IN_CONSTANT_P(address)					\
  (((address) >= constant_start) && ((address) < constant_end))

#endif /* not SCM_GC_H */

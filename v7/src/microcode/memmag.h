/* -*-C-*-

$Id: memmag.h,v 1.10.2.3 2006/08/30 05:17:31 cph Exp $

Copyright 1993,1995,1996,1998,2000,2006 Massachusetts Institute of Technology

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

/* Memory management */

#ifndef SCM_MEMMAG_H
#define SCM_MEMMAG_H 1

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

extern bool object_in_active_heap_p (SCHEME_OBJECT);
extern bool object_in_constant_space_p (SCHEME_OBJECT);
extern bool object_pure_p (SCHEME_OBJECT);

#endif /* SCM_MEMMAG_H */

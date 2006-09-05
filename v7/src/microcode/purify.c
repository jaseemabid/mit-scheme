/* -*-C-*-

$Id: purify.c,v 9.65.2.9 2006/09/05 03:15:39 cph Exp $

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

static bool current_pure_p;

static void purify (SCHEME_OBJECT, bool);
static gc_table_t * purify_table (void);
static gc_handler_t handle_linkage_section;
static gc_handler_t handle_manifest_closure;
static gc_precheck_from_t purify_precheck_from;
static gc_ignore_object_p_t purify_ignore_object_p;

/* Description of the algorithm for PURIFY:

   Purify increases the size of constant space at the expense of the
   heap.  A GC-like relocation is performed with the object being
   purified as the root.  The object is copied and relocated from the
   heap to the area adjacent to constant space.  Then a normal GC is
   finished after changing the end of constant-space marker.

   In order to make a pure object, the copy process proceeds in two
   halves.  During the first half (which collects the pure part)
   Compiled Code, Environments, Symbols, and Variables (i.e. things
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
Copy OBJECT from the heap into constant/pure space.\n\
If PURE? is true, OBJECT goes to pure space,\n\
otherwise it goes to constant space.")
{
  SCHEME_OBJECT object;
  bool pure_p;
  unsigned long safety_margin;
  SCHEME_OBJECT daemon;
  PRIMITIVE_HEADER (3);

  canonicalize_primitive_context ();
  STACK_CHECK_FATAL ("PURIFY");

  object = (ARG_REF (1));
  pure_p = (BOOLEAN_ARG (2));
  safety_margin = (ARG_HEAP_RESERVED (3));
  POP_PRIMITIVE_FRAME (3);

  ENTER_CRITICAL_SECTION ("purify");
  heap_reserved = safety_margin;
  purify (object, pure_p);

 Will_Push (CONTINUATION_SIZE);
  SET_RC (RC_NORMAL_GC_DONE);
  SET_EXP (cons (SHARP_T, (ULONG_TO_FIXNUM (HEAP_AVAILABLE))));
  SAVE_CONT ();
 Pushed ();

  RENAME_CRITICAL_SECTION ("purify daemon");
  daemon = (VECTOR_REF (fixed_objects, GC_DAEMON));
  if (daemon != SHARP_F)
    {
     Will_Push (2);
      STACK_PUSH (daemon);
      PUSH_APPLY_FRAME_HEADER (0);
     Pushed ();
      PRIMITIVE_ABORT (PRIM_APPLY);
    }
  PRIMITIVE_ABORT (PRIM_POP_RETURN);
  /*NOTREACHED*/
  PRIMITIVE_RETURN (UNSPECIFIC);
}

static void
purify (SCHEME_OBJECT object, bool pure_p)
{
  SCHEME_OBJECT * start_copy;
  SCHEME_OBJECT * pure_copy_start;
  SCHEME_OBJECT * pure_copy_end;
  SCHEME_OBJECT * constant_copy_start;
  SCHEME_OBJECT * constant_copy_end;
  SCHEME_OBJECT * new_constant_alloc_next;
  SCHEME_OBJECT * heap_copy_start;
  unsigned long pure_length;
  unsigned long total_length;

  STACK_CHECK_FATAL ("PURIFY");

  open_tospace (constant_alloc_next);
  initialize_weak_chain ();

  start_copy = (get_newspace_ptr ());
  add_to_tospace (SHARP_F);

  pure_copy_start = (get_newspace_ptr ());
  add_to_tospace (object);

  if (pure_p)
    {
      current_gc_table = (purify_table ());
      current_pure_p = true;
      gc_scan_tospace (pure_copy_start, 0);
    }
  pure_copy_end = (get_newspace_ptr ());

  pure_length = ((pure_copy_end - pure_copy_start) + 2);
  write_tospace (start_copy,
		 (MAKE_OBJECT (TC_MANIFEST_SPECIAL_NM_VECTOR, pure_length)));
  add_to_tospace (MAKE_OBJECT (TC_MANIFEST_SPECIAL_NM_VECTOR, 1));
  add_to_tospace (MAKE_OBJECT (CONSTANT_PART, pure_length));

  constant_copy_start = (get_newspace_ptr ());
  if (pure_p)
    {
      current_gc_table = (purify_table ());
      current_pure_p = false;
      gc_scan_tospace (pure_copy_start, pure_copy_end);
    }

  current_gc_table = (std_gc_table ());
  gc_scan_tospace (constant_copy_start, 0);
  update_weak_pointers ();
  constant_copy_end = (get_newspace_ptr ());

  total_length = ((constant_copy_end - pure_copy_start) + 2);
  write_tospace (pure_copy_start, (MAKE_OBJECT (PURE_PART, total_length)));
  add_to_tospace (MAKE_OBJECT (TC_MANIFEST_SPECIAL_NM_VECTOR, 1));
  add_to_tospace (MAKE_OBJECT (END_OF_BLOCK, total_length));

  new_constant_alloc_next = (get_newspace_ptr ());
  increment_tospace_ptr (CONSTANT_SPACE_FUDGE);
  heap_copy_start = (get_newspace_ptr ());

  std_gc_pt1 ();

  constant_alloc_next = new_constant_alloc_next;
  constant_end = heap_copy_start;
  heap_start = constant_end;

  std_gc_pt2 ();

  resize_tospace (heap_end - heap_start);
}

static gc_table_t *
purify_table (void)
{
  static bool initialized_p = false;
  static gc_table_t table;

  if (!initialized_p)
    {
      initialize_gc_table ((&table), true);
      (GCT_PRECHECK_FROM (&table)) = purify_precheck_from;
      (GCT_IGNORE_OBJECT_P (&table)) = purify_ignore_object_p;
      (GCT_ENTRY ((&table), TC_LINKAGE_SECTION)) = handle_linkage_section;
      (GCT_ENTRY ((&table), TC_MANIFEST_CLOSURE)) = handle_manifest_closure;
      initialized_p = true;
    }
  return (&table);
}

static
DEFINE_GC_PRECHECK_FROM (purify_precheck_from)
{
  return
    (((from >= constant_start) && (from < constant_alloc_next))
     ? from
     : (BROKEN_HEART_P (*from))
     ? (OBJECT_ADDRESS (*from))
     : 0);
}

static
DEFINE_GC_IGNORE_OBJECT_P (purify_ignore_object_p)
{
  bool phase2;

  if (SYMBOL_P (object))
    {
      if (!current_pure_p)
	return (false);
      MEMORY_SET (object, SYMBOL_NAME,
		  (GC_HANDLE_VECTOR ((MEMORY_REF (object, SYMBOL_NAME)),
				     false)));
      return (true);
    }
  phase2 = (((OBJECT_TYPE (object)) == TC_REFERENCE_TRAP)
	    || ((OBJECT_TYPE (object)) == TC_COMPILED_ENTRY)
	    || ((OBJECT_TYPE (object)) == TC_COMPILED_CODE_BLOCK)
	    || ((OBJECT_TYPE (object)) == TC_ENVIRONMENT));
  return (current_pure_p ? phase2 : (!phase2));
}

static
DEFINE_GC_HANDLER (handle_linkage_section)
{
  std_gc_death ("linkage section in pure area");
  return (0);
}

static
DEFINE_GC_HANDLER (handle_manifest_closure)
{
  std_gc_death ("compiled closure in pure area");
  return (0);
}

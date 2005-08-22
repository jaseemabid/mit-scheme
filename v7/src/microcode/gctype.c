/* -*-C-*-

$Id: gctype.c,v 9.36.2.1 2005/08/22 18:05:59 cph Exp $

Copyright 1986,1987,1988,1992,1997,2002 Massachusetts Institute of Technology
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

/* This file contains the table which maps between Types and GC Types.  */

#include "config.h"
#include "gc.h"

gc_type_t gc_type_map [N_TYPE_CODES] =
{
  GC_NON_POINTER,		/* TC_NULL,etc */
  GC_PAIR,			/* TC_LIST */
  GC_NON_POINTER,		/* TC_CHARACTER */
  GC_PAIR,		   	/* TC_SCODE_QUOTE */
  GC_TRIPLE,		        /* TC_PCOMB2 */
  GC_PAIR,			/* TC_UNINTERNED_SYMBOL */
  GC_VECTOR,			/* TC_BIG_FLONUM */
  GC_PAIR,			/* TC_COMBINATION_1 */
  GC_NON_POINTER,		/* TC_CONSTANT */
  GC_PAIR,			/* TC_EXTENDED_PROCEDURE */
  GC_VECTOR,			/* TC_VECTOR */
  GC_NON_POINTER,		/* TC_RETURN_CODE */
  GC_TRIPLE,			/* TC_COMBINATION_2 */
  GC_SPECIAL,			/* TC_MANIFEST_CLOSURE */
  GC_VECTOR,			/* TC_BIG_FIXNUM */
  GC_PAIR,			/* TC_PROCEDURE */
  GC_PAIR,			/* TC_ENTITY */
  GC_PAIR,			/* TC_DELAY */
  GC_VECTOR,			/* TC_ENVIRONMENT */
  GC_PAIR,			/* TC_DELAYED */
  GC_TRIPLE,			/* TC_EXTENDED_LAMBDA */
  GC_PAIR,			/* TC_COMMENT */
  GC_VECTOR,			/* TC_NON_MARKED_VECTOR */
  GC_PAIR,			/* TC_LAMBDA */
  GC_NON_POINTER,		/* TC_PRIMITIVE */
  GC_PAIR,			/* TC_SEQUENCE_2 */
  GC_NON_POINTER,		/* TC_FIXNUM */
  GC_PAIR,			/* TC_PCOMB1 */
  GC_VECTOR,			/* TC_CONTROL_POINT */
  GC_PAIR,			/* TC_INTERNED_SYMBOL */
  GC_VECTOR,			/* TC_CHARACTER_STRING,TC_VECTOR_8B */
  GC_PAIR,			/* TC_ACCESS */
  GC_TRIPLE,			/* TC_HUNK3_A */
  GC_PAIR,			/* TC_DEFINITION */
  GC_SPECIAL,			/* TC_BROKEN_HEART */
  GC_PAIR,			/* TC_ASSIGNMENT */
  GC_TRIPLE,			/* TC_HUNK3_B */
  GC_PAIR,			/* TC_IN_PACKAGE */
  GC_VECTOR,			/* TC_COMBINATION */
  GC_SPECIAL,			/* TC_MANIFEST_NM_VECTOR */
  GC_COMPILED,			/* TC_COMPILED_ENTRY */
  GC_PAIR,			/* TC_LEXPR */
  GC_VECTOR,			/* TC_PCOMB3 */
  GC_SPECIAL,			/* TC_MANIFEST_SPECIAL_NM_VECTOR */
  GC_TRIPLE,			/* TC_VARIABLE */
  GC_NON_POINTER,		/* TC_THE_ENVIRONMENT */
  GC_UNDEFINED,			/* 0x2E */
  GC_VECTOR,			/* TC_VECTOR_1B,TC_BIT_STRING */
  GC_NON_POINTER,		/* TC_PCOMB0 */
  GC_VECTOR,			/* TC_VECTOR_16B */
  GC_SPECIAL,			/* TC_REFERENCE_TRAP */
  GC_TRIPLE,			/* TC_SEQUENCE_3 */
  GC_TRIPLE,			/* TC_CONDITIONAL */
  GC_PAIR,			/* TC_DISJUNCTION */
  GC_CELL,			/* TC_CELL */
  GC_PAIR,			/* TC_WEAK_CONS */
  GC_QUADRUPLE,			/* TC_QUAD */
  GC_SPECIAL,			/* TC_LINKAGE_SECTION */
  GC_PAIR,			/* TC_RATNUM */
  GC_NON_POINTER,		/* TC_STACK_ENVIRONMENT */
  GC_PAIR,			/* TC_COMPLEX */
  GC_VECTOR,			/* TC_COMPILED_CODE_BLOCK */
  GC_VECTOR,			/* TC_RECORD */
  GC_UNDEFINED			/* 0x3F */
};

#if (N_TYPE_CODES != 0x40)
#  include "gctype.c and object.h inconsistent -- gc_type_map"
#endif

gc_ptr_type_t
gc_ptr_type (SCHEME_OBJECT object)
{
  switch (GC_TYPE (object))
    {
    case GC_SPECIAL:
      return
	(((REFERENCE_TRAP_P (object))
	  && ((OBJECT_DATUM (object)) >= TRAP_MAX_IMMEDIATE))
	 ? GC_POINTER_NORMAL
	 : GC_POINTER_NOT);

    case GC_CELL:
    case GC_PAIR:
    case GC_TRIPLE:
    case GC_QUADRUPLE:
    case GC_VECTOR:
      return (GC_POINTER_NORMAL);

    case GC_COMPILED:
      return (GC_POINTER_COMPILED);
      break;

    default:
      return (GC_POINTER_NOT);
    }
}

SCHEME_OBJECT *
get_object_address (SCHEME_OBJECT object)
{
  switch (gc_ptr_type (object))
    {
    case GC_POINTER_NORMAL:
      return (OBJECT_ADDRESS (object));

    case GC_POINTER_COMPILED:
      return (cc_entry_to_block_address (object));

    default:
      return (0);
    }
}

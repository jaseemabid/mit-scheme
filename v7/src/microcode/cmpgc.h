/* -*-C-*-

$Id: cmpgc.h,v 1.34.2.1 2005/08/22 18:05:57 cph Exp $

Copyright 1989,1990,1991,1992,1993,1994 Massachusetts Institute of Technology
Copyright 1995,2000,2001,2005 Massachusetts Institute of Technology

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

/* Utilities to relocate compiled code in garbage collection-like
   processes.  */

#ifndef SCM_CMPGC_H
#define SCM_CMPGC_H 1

#include "cmpint.h"

/* When the target address of a closure is stored as a relative
   reference, the following three macros are used to aid relocation of
   these targets.

   DECLARE_RELOCATION_REFERENCE(reference) declares a variable to hold
   reference information when code pointers are relative.

   START_CLOSURE_RELOCATION(block_addr, reference) is called
   immediately before relocating a block of closure entries.

   END_CLOSURE_RELOCATION(block_addr, reference) is called immediately
   after relocating a block of closure entries.  */

#ifndef DECLARE_RELOCATION_REFERENCE
#  define DECLARE_RELOCATION_REFERENCE(reference)
#endif
#ifndef START_CLOSURE_RELOCATION
#  define START_CLOSURE_RELOCATION(scan, reference) do {} while (0)
#endif
#ifndef END_CLOSURE_RELOCATION
#  define END_CLOSURE_RELOCATION(scan, reference) do {} while (0)
#endif
#ifndef START_OPERATOR_RELOCATION
#  define START_OPERATOR_RELOCATION(scan, reference) do {} while (0)
#endif
#ifndef END_OPERATOR_RELOCATION
#  define END_OPERATOR_RELOCATION(scan, reference) do {} while (0)
#endif

#define CC_TRANSPORT_END() do						\
{									\
  SET_CLOSURE_FREE (0);							\
  SET_CLOSURE_SPACE (0);						\
  FLUSH_I_CACHE ();							\
} while (0)

#endif /* SCM_CMPGC_H */

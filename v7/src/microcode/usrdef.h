/* -*-C-*-

$Id: usrdef.h,v 9.46.2.1 2005/08/22 18:06:01 cph Exp $

Copyright 1987,1988,1992,1993,1996,2001 Massachusetts Institute of Technology
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

/* Macros and header for usrdef.c and variants. */

#ifndef SCM_USRDEF_H
#define SCM_USRDEF_H

#include "scheme.h"
#include "prims.h"

extern SCHEME_OBJECT (* (Static_Primitive_Procedure_Table[])) (void);
extern int Static_Primitive_Arity_Table[];
extern int Static_Primitive_Count_Table[];
extern const char * Static_Primitive_Name_Table[];
extern const char * Static_Primitive_Documentation_Table[];
extern long MAX_STATIC_PRIMITIVE;

#endif /* SCM_USRDEF_H */

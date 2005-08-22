/* -*-C-*-

$Id: scheme.h,v 9.42.2.1 2005/08/22 18:06:00 cph Exp $

Copyright 1986,1987,1988,1989,1990,1993 Massachusetts Institute of Technology
Copyright 1997,2000,2005 Massachusetts Institute of Technology

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

/* General declarations for the SCode interpreter.  This
   file is INCLUDED by others and contains declarations only. */

#ifndef SCM_SCHEME_H
#define SCM_SCHEME_H 1

#include "config.h"
#include "dstack.h"		/* Dynamic stack support package */
#include "obstack.h"		/* Obstack package */
#include "types.h"		/* Type code numbers */
#include "const.h"		/* Various named constants */
#include "object.h"		/* Scheme object representation */
#include "intrpt.h"		/* Interrupt processing macros */
#include "critsec.h"		/* Critical sections */
#include "gc.h"			/* Memory management related macros */
#include "scode.h"		/* Scheme scode representation */
#include "sdata.h"		/* Scheme user data representation */
#include "errors.h"		/* Error code numbers */
#include "returns.h"		/* Return code numbers */
#include "fixobj.h"		/* Format of fixed objects vector */
#include "stack.h"		/* Macros for stack (stacklet) manipulation */
#include "interp.h"		/* Macros for interpreter */
#include "outf.h"		/* Formatted output for errors */
#include "bkpt.h"		/* Shadows some defaults */
#include "extern.h"		/* External declarations */
#include "bignum.h"		/* Bignum declarations */
#include "prim.h"		/* Declarations for primitives. */
#include "cmpint.h"		/* compiled-code interface */

#endif /* SCM_SCHEME_H */

/* -*-C-*-

$Id: os.h,v 1.9.2.1 2005/08/22 18:05:59 cph Exp $

Copyright 1990,1993,1994,2000,2005 Massachusetts Institute of Technology

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

#ifndef SCM_OS_H
#define SCM_OS_H

#include "config.h"

typedef unsigned int Tchannel;

extern void * OS_malloc (unsigned int);
extern void * OS_realloc (void *, unsigned int);
extern void OS_free (void *);

#define FASTCOPY(from, to, n) (memcpy ((to), (from), (n)))

#endif /* SCM_OS_H */

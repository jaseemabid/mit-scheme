/* -*-C-*-

$Id: osctty.h,v 1.6.2.1 2005/08/22 18:06:00 cph Exp $

Copyright 1990,1992,1993,2005 Massachusetts Institute of Technology

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

#ifndef SCM_OSCTTY_H
#define SCM_OSCTTY_H

#include "os.h"

/* If this procedure returns 0, the interrupt control procedures will
   not work correctly. */
extern int OS_ctty_interrupt_control (void);

typedef unsigned int Tinterrupt_enables;
extern void OS_ctty_get_interrupt_enables (Tinterrupt_enables * mask);
extern void OS_ctty_set_interrupt_enables (Tinterrupt_enables * mask);

extern unsigned int OS_ctty_num_int_chars (void);
extern cc_t * OS_ctty_get_int_chars (void);
extern cc_t * OS_ctty_get_int_char_handlers (void);
extern void OS_ctty_set_int_chars (cc_t *);
extern void OS_ctty_set_int_char_handlers (cc_t *);

#endif /* SCM_OSCTTY_H */

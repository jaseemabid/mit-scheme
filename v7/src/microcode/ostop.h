/* -*-C-*-

$Id: ostop.h,v 1.7.2.2 2005/08/23 02:55:11 cph Exp $

Copyright 1990,1991,1993,2005 Massachusetts Institute of Technology

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

#ifndef SCM_OSTOP_H
#define SCM_OSTOP_H

#include "os.h"

extern int OS_under_emacs_p (void);
extern void OS_initialize (void);
extern void OS_reset (void);
extern void OS_quit (int code, int abnormal_p);
extern void OS_restartable_exit (void);
extern void OS_save_external_state (void);
extern void OS_save_internal_state (void);
extern void OS_restore_internal_state (void);
extern void OS_restore_external_state (void);
extern const char * OS_error_code_to_message (unsigned int code);

#endif /* SCM_OSTOP_H */

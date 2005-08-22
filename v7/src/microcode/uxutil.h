/* -*-C-*-

$Id: uxutil.h,v 1.5.2.1 2005/08/22 18:06:01 cph Exp $

Copyright 1990,1993,2005 Massachusetts Institute of Technology

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

#ifndef SCM_UXUTIL_H
#define SCM_UXUTIL_H

#include "os.h"

extern const char * char_description (unsigned char c, int long_p);
extern void userio_buffered_input (void);
extern char userio_read_char (void);
extern char userio_read_char_raw (void);
extern char userio_choose_option
  (const char * herald, const char * prompt, const char ** choices);
extern int userio_confirm (const char * prompt);

#endif /* SCM_UXUTIL_H */

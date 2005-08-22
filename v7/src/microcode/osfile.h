/* -*-C-*-

$Id: osfile.h,v 1.6.2.1 2005/08/22 18:06:00 cph Exp $

Copyright 1990,1993,2004,2005 Massachusetts Institute of Technology

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

#ifndef SCM_OSFILE_H
#define SCM_OSFILE_H

#include "os.h"

extern Tchannel OS_open_input_file (const char * filename);
extern Tchannel OS_open_output_file (const char * filename);
extern Tchannel OS_open_io_file (const char * filename);
extern Tchannel OS_open_append_file (const char * filename);
extern Tchannel OS_open_load_file (const char * filename);
extern Tchannel OS_open_dump_file (const char * filename);
extern off_t OS_file_length (Tchannel channel);
extern off_t OS_file_position (Tchannel channel);
extern void OS_file_set_position (Tchannel channel, off_t position);
extern void OS_file_truncate (Tchannel channel, off_t length);

#endif /* SCM_OSFILE_H */

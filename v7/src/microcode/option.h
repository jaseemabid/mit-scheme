/* -*-C-*-

$Id: option.h,v 1.16.2.2 2005/08/23 02:55:11 cph Exp $

Copyright 1990,1991,1992,1993,1995,2003 Massachusetts Institute of Technology
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
Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301,
USA.

*/

#ifndef SCM_OPTION_H
#define SCM_OPTION_H

#include "config.h"

extern int option_saved_argc;
extern const char ** option_saved_argv;
extern int option_unused_argc;
extern const char ** option_unused_argv;

/* Boolean options */
extern bool option_emacs_subprocess;
extern bool option_force_interactive;
extern bool option_disable_core_dump;
extern bool option_batch_mode;
extern bool option_band_specified;
extern bool option_gc_keep;

/* String options */
extern const char ** option_library_path;
extern const char * option_band_file;
extern const char * option_fasl_file;
extern const char * option_utabmd_file;
extern const char * option_gc_directory;
extern const char * option_gc_drone;
extern const char * option_gc_file;

/* Numeric options */
extern unsigned long option_heap_size;
extern unsigned long option_constant_size;
extern unsigned long option_stack_size;
extern unsigned long option_gc_read_overlap;
extern unsigned long option_gc_window_size;
extern unsigned long option_gc_write_overlap;
extern unsigned long option_gc_start_position;
extern unsigned long option_gc_end_position;

extern void read_command_line_options (int argc, const char ** argv);

extern const char * search_for_library_file (const char *);

extern const char * search_path_for_file
  (const char * option, const char * filename, bool default_p, bool fail_p);

#endif /* SCM_OPTION_H */

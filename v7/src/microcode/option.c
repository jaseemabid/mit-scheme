/* -*-C-*-

$Id: option.c,v 1.61.2.1 2005/08/22 18:05:59 cph Exp $

Copyright 1990,1991,1992,1993,1994,1995 Massachusetts Institute of Technology
Copyright 1996,1997,1998,1999,2000,2001 Massachusetts Institute of Technology
Copyright 2002,2003,2005 Massachusetts Institute of Technology

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

/* Command-line option processing */

#include <ctype.h>
#include "scheme.h"
#include "fasl.h"
#include "osenv.h"
#include "osfs.h"
#include <sys/stat.h>

#define xfree(p) OS_free ((void *) (p))

#ifdef HAVE_UNISTD_H
#  include <unistd.h>
#endif

#ifdef __WIN32__
#  include <io.h>
#  include "nt.h"
#  include "ntio.h"
#endif

#if defined(__WIN32__) || defined(__OS2__)
#  define DOS_LIKE_FILENAMES
#endif

#ifndef SUB_DIRECTORY_DELIMITER
#  ifdef DOS_LIKE_FILENAMES
#    define SUB_DIRECTORY_DELIMITER '\\'
#  else
#    define SUB_DIRECTORY_DELIMITER '/'
#  endif
#endif

#ifndef PATH_DELIMITER
#  ifdef DOS_LIKE_FILENAMES
#    define PATH_DELIMITER ';'
#  else
#    define PATH_DELIMITER ':'
#  endif
#endif

#ifdef DOS_LIKE_FILENAMES
#  define FILE_ABSOLUTE(filename)			\
     ((((filename) [0]) == SUB_DIRECTORY_DELIMITER)	\
      || (((filename) [1]) == ':'))
#else
#  define FILE_ABSOLUTE(filename) (((filename) [0]) == SUB_DIRECTORY_DELIMITER)
#endif

#define FILE_READABLE(filename) (OS_file_access ((filename), 4))

static bool option_summary;
static bool option_large_sizes;
static bool option_compiler_defaults;
static bool option_edwin_defaults;

static const char * option_raw_library;
static const char * option_raw_utabmd;
static const char * option_raw_utab;
static const char * option_raw_band;
static const char * option_raw_heap;
static const char * option_raw_constant;
static const char * option_raw_stack;
static const char * option_raw_gc_end_position;
static const char * option_raw_gc_file;
static const char * option_raw_gc_read_overlap;
static const char * option_raw_gc_start_position;
static const char * option_raw_gc_window_size;
static const char * option_raw_gc_write_overlap;

/* Command-line arguments */
int option_saved_argc;
const char ** option_saved_argv;
int option_unused_argc;
const char ** option_unused_argv;

/* Boolean options */
bool option_emacs_subprocess;
bool option_force_interactive;
bool option_disable_core_dump;
bool option_band_specified;
bool option_batch_mode;
bool option_gc_keep;

/* String options */
const char ** option_library_path = 0;
const char * option_band_file = 0;
const char * option_fasl_file = 0;
const char * option_utabmd_file = 0;
const char * option_gc_directory = 0;
const char * option_gc_drone = 0;
const char * option_gc_file = 0;

/* Numeric options */
unsigned long option_heap_size;
unsigned long option_constant_size;
unsigned long option_stack_size;
unsigned long option_gc_window_size;
unsigned long option_gc_read_overlap;
unsigned long option_gc_write_overlap;
unsigned long option_gc_start_position;
unsigned long option_gc_end_position;

/*

Scheme accepts the following command-line options.  The options may
appear in any order, but they must all appear before any other
arguments on the command line.

--library PATH
  Sets the library search path to PATH.  This is a colon-separated
  list of directories that is searched to find various library files,
  such as bands.  If this option is not given, the value of the
  environment variable MITSCHEME_LIBRARY_PATH is used; it that isn't
  defined, "/usr/local/lib/mit-scheme" is used.

--band FILENAME
  Specifies the initial band to be loaded.  Searches for FILENAME in
  the working directory and the library directories, returning the
  full pathname of the first readable file of that name.  If this
  option isn't given, the filename is the value of the environment
  variable MITSCHEME_BAND, or if that isn't defined, "runtime.com"; in
  these cases the library directories are searched, but not the
  working directory.

--fasl FILENAME
  Specifies that a cold load should be performed, using FILENAME as
  the initial file to be loaded.  If this option isn't given, a normal
  load is performed instead.  This option may not be used together
  with the "--band" option.

--utabmd FILENAME
  Specifies the name of the microcode tables file.  The file is
  searched for in the working directory and the library directories.
  If this option isn't given, the filename is the value of the
  environment variable MITSCHEME_UTABMD_FILE, or if that isn't
  defined, "utabmd.bin"; in these cases the library directories are
  searched, but not the working directory.

--utab FILENAME
  An alternate name for the "--utabmd" option.  At most one of these
  options may be given.

--large
  Specifies that large heap, constant, and stack default sizes should
  be used.  These are specified by the environment variables
  MITSCHEME_LARGE_HEAP, MITSCHEME_LARGE_CONSTANT, and
  MITSCHEME_LARGE_STACK.  If this option isn't given, the small sizes
  are used, specified by the environment variables
  MITSCHEME_SMALL_HEAP, MITSCHEME_SMALL_CONSTANT, and
  MITSCHEME_SMALL_STACK.  There are reasonable built-in defaults for
  these environment variables, should any of them be undefined.  [The
  Scheme procedure `(print-gc-statistics)' shows how much heap and
  constant space is available and in use.]

--heap BLOCKS
  Specifies the size of the heap in 1024-word blocks.  Overrides any
  default.  Normally two such heaps are allocated; `bchscheme'
  allocates only one.

--constant BLOCKS
  Specifies the size of constant space in 1024-word blocks.  Overrides
  any default.

--stack BLOCKS
  Specifies the size of the stack in 1024-word blocks.  Overrides any
  default.

--option-summary
  Causes Scheme to write option information to standard error.

--emacs
  Specifies that Scheme is running as a subprocess of GNU Emacs.
  This option is automatically supplied by GNU Emacs, and should not
  be given under other circumstances.

--interactive
  If this option isn't specified, and Scheme's standard I/O is not a
  terminal, Scheme will detach itself from its controlling terminal.
  This will prevent it from getting signals sent to the process group
  of that terminal.  If this option is specified, Scheme will not
  detach itself from the controlling terminal.

--nocore
  Specifies that Scheme should not generate a core dump under any
  circumstances.

The following options are available only on machines with
compiled-code support:

--compiler
  This option specifies defaults appropriate for loading the compiler.
  It changes the defaults for "--band": the environment variable
  MITSCHEME_COMPILER_BAND is used, otherwise "compiler.com" is used.
  It also specifies the use of large sizes, exactly like "--large".

--edwin
  This option specifies defaults appropriate for loading the editor.
  It changes the defaults for "--band": the environment variable
  MITSCHEME_EDWIN_BAND is used, otherwise "edwin.com" is used.  It
  also specifies the use of large sizes, exactly like "--large".

The following options are only meaningful to bchscheme:

--gc-directory DIRECTORY
  Specifies what directory to use to allocate the garbage collection file.

--gc-drone FILENAME
  Specifies the program to use as the gc drones for overlapped I/O.

--gc-end-position N
  Specifies a position into the gc file past which bchscheme should not use.

--gc-file FILENAME
  Specifies that FILENAME should be used garbage collection.  Overrides
  -gc-directory if it is an absolute pathname.  -gcfile means the same thing,
  but is deprecated.

--gc-keep
  Specifles that newly allocated gc files should be kept rather than deleted.

--gc-read-overlap N
  Specifies the number of additional GC windows to use when reading
  for overlapped I/O.  Each implies a drone process to manage it,
  if supported.

--gc-start-position N
  Specifies a position into the gc file before which bchscheme should not use.

--gc-window-size BLOCKS
  Specifies the size in 1024-word blocks of each GC window.

--gc-write-overlap N
  Specifies the number of additional GC windows to use when writing for
  overlapped I/O.  Each implies a drone process to manage it, if supported.

*/

#ifndef LIBRARY_PATH_VARIABLE
#  define LIBRARY_PATH_VARIABLE "MITSCHEME_LIBRARY_PATH"
#endif

#ifndef DEFAULT_LIBRARY_PATH
#  ifdef DOS_LIKE_FILENAMES
#    define DEFAULT_LIBRARY_PATH "\\scheme\\lib"
#  else
#    define DEFAULT_LIBRARY_PATH "/usr/local/lib/mit-scheme"
#  endif
#endif

#ifndef BAND_VARIABLE
#  define BAND_VARIABLE "MITSCHEME_BAND"
#endif

#ifndef DEFAULT_BAND
#  define DEFAULT_BAND "runtime.com"
#endif

#ifndef COMPILER_BAND_VARIABLE
#  define COMPILER_BAND_VARIABLE "MITSCHEME_COMPILER_BAND"
#endif

#ifndef COMPILER_DEFAULT_BAND
#  define COMPILER_DEFAULT_BAND "compiler.com"
#endif

#ifndef EDWIN_BAND_VARIABLE
#  define EDWIN_BAND_VARIABLE "MITSCHEME_EDWIN_BAND"
#endif

#ifndef EDWIN_DEFAULT_BAND
#  define EDWIN_DEFAULT_BAND "edwin.com"
#endif

#ifndef ALL_BAND_VARIABLE
#  define ALL_BAND_VARIABLE "MITSCHEME_ALL_BAND"
#endif

#ifndef ALL_DEFAULT_BAND
#  define ALL_DEFAULT_BAND "all.com"
#endif

#ifndef UTABMD_FILE_VARIABLE
#  define UTABMD_FILE_VARIABLE "MITSCHEME_UTABMD_FILE"
#endif

#ifndef DEFAULT_UTABMD_FILE
#  define DEFAULT_UTABMD_FILE "utabmd.bin"
#endif

#if (COMPILER_PROCESSOR_TYPE == COMPILER_SPECTRUM_TYPE)
#  ifndef DEFAULT_SMALL_CONSTANT
#    define DEFAULT_SMALL_CONSTANT 600
#  endif
#  ifndef DEFAULT_LARGE_CONSTANT
#    define DEFAULT_LARGE_CONSTANT 1400
#  endif
#endif

#if (COMPILER_PROCESSOR_TYPE == COMPILER_MIPS_TYPE)
#  ifndef DEFAULT_SMALL_CONSTANT
#    define DEFAULT_SMALL_CONSTANT 700
#  endif
#  ifndef DEFAULT_LARGE_CONSTANT
#    define DEFAULT_LARGE_CONSTANT 1500
#  endif
#endif

#if (COMPILER_PROCESSOR_TYPE == COMPILER_IA32_TYPE)
#  ifndef DEFAULT_SMALL_CONSTANT
#    define DEFAULT_SMALL_CONSTANT 600
#  endif
#  ifndef DEFAULT_LARGE_CONSTANT
#    define DEFAULT_LARGE_CONSTANT 1200
#  endif
#endif

#if (COMPILER_PROCESSOR_TYPE == COMPILER_SVM_TYPE)
#  ifndef DEFAULT_SMALL_CONSTANT
#    define DEFAULT_SMALL_CONSTANT 600
#  endif
#  ifndef DEFAULT_LARGE_CONSTANT
#    define DEFAULT_LARGE_CONSTANT 1200
#  endif
#endif

#ifndef DEFAULT_SMALL_HEAP
#  define DEFAULT_SMALL_HEAP 250
#endif

#ifndef SMALL_HEAP_VARIABLE
#  define SMALL_HEAP_VARIABLE "MITSCHEME_SMALL_HEAP"
#endif

#ifndef DEFAULT_SMALL_CONSTANT
#  define DEFAULT_SMALL_CONSTANT 450
#endif

#ifndef SMALL_CONSTANT_VARIABLE
#  define SMALL_CONSTANT_VARIABLE "MITSCHEME_SMALL_CONSTANT"
#endif

#ifndef DEFAULT_SMALL_STACK
#  define DEFAULT_SMALL_STACK 100
#endif

#ifndef SMALL_STACK_VARIABLE
#  define SMALL_STACK_VARIABLE "MITSCHEME_SMALL_STACK"
#endif

#ifndef DEFAULT_LARGE_HEAP
#  define DEFAULT_LARGE_HEAP 1000
#endif

#ifndef LARGE_HEAP_VARIABLE
#  define LARGE_HEAP_VARIABLE "MITSCHEME_LARGE_HEAP"
#endif

#ifndef DEFAULT_LARGE_CONSTANT
#  define DEFAULT_LARGE_CONSTANT 1000
#endif

#ifndef LARGE_CONSTANT_VARIABLE
#  define LARGE_CONSTANT_VARIABLE "MITSCHEME_LARGE_CONSTANT"
#endif

#ifndef DEFAULT_LARGE_STACK
#  define DEFAULT_LARGE_STACK DEFAULT_SMALL_STACK
#endif

#ifndef LARGE_STACK_VARIABLE
#  define LARGE_STACK_VARIABLE "MITSCHEME_LARGE_STACK"
#endif

/* These are only meaningful for bchscheme */

#ifndef DEFAULT_GC_DIRECTORY
#  ifdef DOS_LIKE_FILENAMES
#    define DEFAULT_GC_DIRECTORY "\\tmp"
#  else
#    define DEFAULT_GC_DIRECTORY "/tmp"
#  endif
#endif

#ifndef GC_DIRECTORY_VARIABLE
#  define GC_DIRECTORY_VARIABLE "MITSCHEME_GC_DIRECTORY"
#endif

#ifndef DEFAULT_GC_DRONE
#  define DEFAULT_GC_DRONE "gcdrone"
#endif

#ifndef GC_DRONE_VARIABLE
#  define GC_DRONE_VARIABLE "MITSCHEME_GC_DRONE"
#endif

#ifndef DEFAULT_GC_END_POSITION
#  define DEFAULT_GC_END_POSITION (-1)
#endif

#ifndef GC_END_POSITION_VARIABLE
#  define GC_END_POSITION_VARIABLE "MITSCHEME_GC_END_POSITION"
#endif

#ifndef DEFAULT_GC_FILE
#  define DEFAULT_GC_FILE "GCXXXXXX"
#endif

#ifndef GC_FILE_VARIABLE
#  define GC_FILE_VARIABLE "MITSCHEME_GC_FILE"
#endif

#ifndef DEFAULT_GC_READ_OVERLAP
#  define DEFAULT_GC_READ_OVERLAP 0
#endif

#ifndef GC_READ_OVERLAP_VARIABLE
#  define GC_READ_OVERLAP_VARIABLE "MITSCHEME_GC_READ_OVERLAP"
#endif

#ifndef DEFAULT_GC_START_POSITION
#  define DEFAULT_GC_START_POSITION 0
#endif

#ifndef GC_START_POSITION_VARIABLE
#  define GC_START_POSITION_VARIABLE "MITSCHEME_GC_START_POSITION"
#endif

#ifndef DEFAULT_GC_WINDOW_SIZE
#  define DEFAULT_GC_WINDOW_SIZE 16
#endif

#ifndef GC_WINDOW_SIZE_VARIABLE
#  define GC_WINDOW_SIZE_VARIABLE "MITSCHEME_GC_WINDOW_SIZE"
#endif

#ifndef DEFAULT_GC_WRITE_OVERLAP
#  define DEFAULT_GC_WRITE_OVERLAP 0
#endif

#ifndef GC_WRITE_OVERLAP_VARIABLE
#  define GC_WRITE_OVERLAP_VARIABLE "MITSCHEME_GC_WRITE_OVERLAP"
#endif

static int
string_compare_ci (const char * string1, const char * string2)
{
  const char * scan1 = string1;
  unsigned int length1 = (strlen (string1));
  const char * scan2 = string2;
  unsigned int length2 = (strlen (string2));
  unsigned int length = ((length1 < length2) ? length1 : length2);
  const char * end1 = (scan1 + length);
  const char * end2 = (scan2 + length);
  while ((scan1 < end1) && (scan2 < end2))
    {
      int c1 = (*scan1++);
      int c2 = (*scan2++);
      if (islower (c1))
	{
	  if (! (islower (c2)))
	    c1 = (toupper (c1));
	}
      else
	{
	  if (islower (c2))
	    c2 = (toupper (c2));
	}
      if (c1 != c2)
	return ((c1 < c2) ? (-1) : 1);
    }
  return
    ((length1 == length2)
     ? 0
     : ((length1 < length2) ? (-1) : 1));
}

static char *
string_copy (const char * s)
{
  char * result = (OS_malloc ((strlen (s)) + 1));
  {
    const char * s1 = s;
    char * s2 = result;
    while (((*s2++) = (*s1++)) != '\0') ;
  }
  return (result);
}

struct option_descriptor
{
  const char * option;
  bool argument_p;
  void * value_cell;
};

static void
option_argument (const char * option, bool argument_p, void * value_cell)
{
  struct option_descriptor descriptor;
  (descriptor . option) = option;
  (descriptor . argument_p) = argument_p;
  (descriptor . value_cell) = value_cell;
  obstack_grow ((&scratch_obstack), (&descriptor), (sizeof (descriptor)));
}

static void
parse_options (int argc, const char ** argv)
{
  const char ** scan_argv = (argv + 1);
  const char ** end_argv = (scan_argv + (argc - 1));
  unsigned int n_descriptors =
    ((obstack_object_size (&scratch_obstack))
     / (sizeof (struct option_descriptor)));
  struct option_descriptor * descriptors = (obstack_finish (&scratch_obstack));
  struct option_descriptor * end_desc = (descriptors + n_descriptors);
  struct option_descriptor * scan_desc;
  for (scan_desc = descriptors; (scan_desc < end_desc); scan_desc += 1)
    if (scan_desc->argument_p)
      {
	const char ** value_cell = (scan_desc->value_cell);
	(*value_cell) = 0;
      }
    else
      {
	bool * value_cell = (scan_desc->value_cell);
	(*value_cell) = false;
      }
  while (scan_argv < end_argv)
    {
      const char * option = (*scan_argv++);
      if ((strncmp ("--", option, 2)) == 0)
	option += 2;
      else if ((strncmp ("-", option, 1)) == 0)
	option += 1;
      else
	{
	  scan_argv -= 1;
	  break;
	}
      for (scan_desc = descriptors; (scan_desc < end_desc); scan_desc += 1)
	if ((string_compare_ci (option, (scan_desc->option))) == 0)
	  {
	    if (scan_desc->argument_p)
	      {
		const char ** value_cell = (scan_desc->value_cell);
		if (scan_argv < end_argv)
		  (*value_cell) = (*scan_argv++);
		else
		  {
		    outf_fatal ("%s: option --%s requires an argument.\n",
				scheme_program_name, option);
		    termination_init_error ();
		  }
	      }
	    else
	      {
		bool * value_cell = (scan_desc->value_cell);
		(*value_cell) = true;
	      }
	    break;
	  }
      if (scan_desc == end_desc)
	{
	  scan_argv -= 1;
	  break;
	}
    }
  obstack_free ((&scratch_obstack), descriptors);
  option_saved_argc = argc;
  option_saved_argv = argv;
  option_unused_argc = (end_argv - scan_argv);
  option_unused_argv = scan_argv;
}

static void
parse_standard_options (int argc, const char ** argv)
{
  option_argument ("band", true, (&option_raw_band));
  option_argument ("constant", true, (&option_raw_constant));
  option_argument ("emacs", false, (&option_emacs_subprocess));
  option_argument ("fasl", true, (&option_fasl_file));
  option_argument ("heap", true, (&option_raw_heap));
  option_argument ("interactive", false, (&option_force_interactive));
  option_argument ("large", false, (&option_large_sizes));
  option_argument ("library", true, (&option_raw_library));
  option_argument ("nocore", false, (&option_disable_core_dump));
  option_argument ("option-summary", false, (&option_summary));
  option_argument ("stack", true, (&option_raw_stack));
  option_argument ("utab", true, (&option_raw_utab));
  option_argument ("utabmd", true, (&option_raw_utabmd));
  option_argument ("batch-mode", false, (&option_batch_mode));
  option_argument ("compiler", false, (&option_compiler_defaults));
  option_argument ("edwin", false, (&option_edwin_defaults));

  /* The following options are only meaningful to bchscheme. */
  option_argument ("gc-directory", true, (&option_gc_directory));
  option_argument ("gc-drone", true, (&option_gc_drone));
  option_argument ("gc-end-position", true, (&option_raw_gc_end_position));
  option_argument ("gc-file", true, (&option_gc_file));
  option_argument ("gc-keep", false, (&option_gc_keep));
  option_argument ("gc-start-position", true, (&option_raw_gc_start_position));
  option_argument ("gc-read-overlap", true, (&option_raw_gc_read_overlap));
  option_argument ("gc-window-size", true, (&option_raw_gc_window_size));
  option_argument ("gc-write-overlap", true, (&option_raw_gc_write_overlap));
  option_argument ("gcfile", true, (&option_raw_gc_file)); /* Obsolete */
  parse_options (argc, argv);
}

static const char *
string_option (const char * option, const char * defval)
{
  return ((option == 0) ? defval : option);
}

static const char *
environment_default (const char * variable, const char * defval)
{
  const char * temp = (getenv (variable));
  return ((temp == 0) ? defval : temp);
}

static const char *
standard_string_option (const char * option,
			const char * variable,
			const char * defval)
{
  if (option != 0)
    return (option);
  {
    const char * t = (getenv (variable));
    return ((t != 0) ? t : defval);
  }
}

static unsigned long
standard_numeric_option (const char * option,
			 const char * optval,
			 const char * variable,
			 unsigned long defval)
{
  if (optval != 0)
    {
      char * end;
      unsigned long n = (strtoul (optval, (&end), 0));;
      if ((end == optval) || ((*end) != '\0'))
	{
	  outf_fatal ("%s: illegal argument for option --%s: %s\n",
		      scheme_program_name, option, optval);
	  termination_init_error ();
	}
      return (n);
    }
  {
    const char * t = (getenv (variable));
    if (t != 0)
      {
	char * end;
	unsigned long n = (strtoul (t, (&end), 0));;
	if ((end == t) || ((*end) != '\0'))
	  {
	    outf_fatal ("%s: illegal value for environment variable %s: %s\n",
			scheme_program_name, variable, t);
	    termination_init_error ();
	  }
	return (n);
      }
  }
  return (defval);
}

static const char *
get_wd (void)
{
  const char * wd = (OS_working_dir_pathname ());
  unsigned int len = (strlen (wd));
  if ((wd [len - 1]) == SUB_DIRECTORY_DELIMITER)
    len -= 1;
  {
    char * result = (OS_malloc (len + 1));
    char * scan_result = result;
    const char * scan_wd = wd;
    const char * end_wd = (scan_wd + len);
    while (scan_wd < end_wd)
      (*scan_result++) = (*scan_wd++);
    (*scan_result) = '\0';
    return (result);
  }
}

static const char **
parse_path_string (const char * path)
{
  const char * start = path;
  /* It is important that this get_wd be called here to make sure that
     the the unix getcwd is called now, before it allocates heap space
     This is because getcwd forks off a new process and we want to do
     that before the scheme process gets too big
  */
  const char * wd = (get_wd ());
  unsigned int lwd = (strlen (wd));
  while (1)
    {
      const char * scan = start;
      const char * end;
      while (1)
	{
	  int c = (*scan++);
	  if ((c == '\0') || (c == PATH_DELIMITER))
	    {
	      end = (scan - 1);
	      break;
	    }
	}
      if ((start < end) && ((* (end - 1)) == SUB_DIRECTORY_DELIMITER))
	end -= 1;
      if (end == start)
	obstack_ptr_grow ((&scratch_obstack), (string_copy (wd)));
      else
	{
	  int absolute = (FILE_ABSOLUTE (start));
	  {
	    char * element =
	      (OS_malloc ((absolute ? 0 : (lwd + 1)) + (end - start) + 1));
	    char * scan_element = element;
	    if (!absolute)
	      {
		const char * s = wd;
		const char * e = (wd + lwd);
		while (s < e)
		  (*scan_element++) = (*s++);
		(*scan_element++) = SUB_DIRECTORY_DELIMITER;
	      }
	    {
	      const char * s = start;
	      while (s < end)
		(*scan_element++) = (*s++);
	    }
	    (*scan_element) = '\0';
	    obstack_ptr_grow ((&scratch_obstack), element);
	  }
	}
      if ((* (scan - 1)) == '\0')
	break;
      start = scan;
    }
  obstack_ptr_grow ((&scratch_obstack), 0);
  if (wd != 0)
    xfree (wd);
  {
    unsigned int n_bytes = (obstack_object_size (&scratch_obstack));
    const char ** elements = (obstack_finish (&scratch_obstack));
    const char ** scan = elements;
    const char ** end = (scan + (n_bytes / (sizeof (char *))));
    const char ** result = (OS_malloc (n_bytes));
    const char ** scan_result = result;
    while (scan < end)
      (*scan_result++) = (*scan++);
    obstack_free ((&scratch_obstack), elements);
    return (result);
  }
}

static void
free_parsed_path (const char ** path)
{
  const char ** scan = path;
  while (1)
    {
      const char * element = (*scan++);
      if (element == 0)
	break;
      xfree (element);
    }
  xfree (path);
}

const char *
search_for_library_file (const char * filename)
{
  unsigned int flen = (strlen (filename));
  const char ** scan_path = option_library_path;
  while (1)
    {
      const char * directory = (*scan_path++);
      unsigned int dlen;
      const char * fullname;
      if (directory == 0)
	return (0);
      dlen = (strlen (directory));
      if (dlen > 0)
	{
	  obstack_grow ((&scratch_obstack), directory, dlen);
	  obstack_1grow ((&scratch_obstack), SUB_DIRECTORY_DELIMITER);
	}
      obstack_grow ((&scratch_obstack), filename, flen);
      obstack_1grow ((&scratch_obstack), '\0');
      fullname = (obstack_finish (&scratch_obstack));
      if (FILE_READABLE (fullname))
	{
	  const char * result = (string_copy (fullname));
	  obstack_free ((&scratch_obstack), ((char *) fullname));
	  return (result);
	}
      obstack_free ((&scratch_obstack), ((char *) fullname));
    }
}

const char *
search_path_for_file (const char * option,
		      const char * filename,
		      bool default_p,
		      bool fail_p)
{
  const char * result = (search_for_library_file (filename));
  if (result != 0)
    return (result);
  if (!fail_p)
    return (filename);
  else
    {
      const char ** scan_path = option_library_path;
      outf_fatal ("%s: can't find a readable %s",
		  scheme_program_name,
		  (default_p ? "default" : "file"));
      if (option != 0)
	outf_fatal (" for option --%s", option);
      outf_fatal (".\n");
      outf_fatal ("\tsearched for file %s in these directories:\n", filename);
      if (!default_p)
	outf_fatal ("\t.\n");
      while (1)
	{
	  const char * element = (*scan_path++);
	  if (element == 0)
	    break;
	  outf_fatal ("\t%s\n", element);
	}
      termination_init_error ();
      /*NOTREACHED*/
      return (0);
    }
}

static const char *
standard_filename_option (const char * option,
			  const char * optval,
			  const char * variable,
			  const char * defval,
			  bool fail_p)
{
  if (optval != 0)
    {
      if (FILE_READABLE (optval))
	return (string_copy (optval));
      if (FILE_ABSOLUTE (optval))
	{
	  if (fail_p)
	    {
	      outf_fatal ("%s: can't read file %s for option --%s.\n",
			  scheme_program_name, optval, option);
	      termination_init_error ();
	    }
	  return (string_copy (optval));
	}
      return (search_path_for_file (option, optval, false, fail_p));
    }
  {
    const char * filename = (getenv (variable));
    if (filename == 0)
      filename = defval;
    if (FILE_ABSOLUTE (filename))
      {
	if ((! (FILE_READABLE (filename))) && fail_p)
	  {
	    outf_fatal ("%s: can't read default file %s for option --%s.\n",
			scheme_program_name, filename, option);
	    termination_init_error ();
	  }
	return (string_copy (filename));
      }
    else
      return (search_path_for_file (option, filename, true, fail_p));
  }
}

static void
conflicting_options (const char * option1, const char * option2)
{
  outf_fatal ("%s: can't specify both options --%s and --%s.\n",
	      scheme_program_name, option1, option2);
  termination_init_error ();
}

#define SCHEME_WORDS_TO_BLOCKS(n) (((n) + 1023) / 1024)

static int
read_band_header (const char * filename, SCHEME_OBJECT * header)
{
#ifdef __WIN32__

  HANDLE handle
    = (CreateFile (filename,
		   GENERIC_READ,
		   (FILE_SHARE_READ | FILE_SHARE_WRITE),
		   0,
		   OPEN_EXISTING,
		   (FILE_ATTRIBUTE_NORMAL | FILE_FLAG_SEQUENTIAL_SCAN),
		   0));
  DWORD bytes_to_read = ((sizeof (SCHEME_OBJECT)) * FASL_HEADER_LENGTH);
  DWORD bytes_read;
  if (handle == INVALID_HANDLE_VALUE)
    return (0);
  if (! ((ReadFile (handle, header, bytes_to_read, (&bytes_read), 0))
	 && (bytes_read == bytes_to_read)))
    {
      CloseHandle (handle);
      return (0);
    }
  CloseHandle (handle);
  return (1);

#else /* not __WIN32__ */

  FILE * stream = (fopen (filename, "r"));
  if (stream == 0)
    return (0);
  if ((fread (header, (sizeof (SCHEME_OBJECT)), FASL_HEADER_LENGTH, stream))
      != FASL_HEADER_LENGTH)
    {
      fclose (stream);
      return (0);
    }
  fclose (stream);
  return (1);

#endif /* not __WIN32__ */
}

static int
read_band_sizes (const char * filename,
		 unsigned long * constant_size,
		 unsigned long * heap_size)
{
  SCHEME_OBJECT header [FASL_HEADER_LENGTH];
  if (!read_band_header (filename, header))
    return (0);
  (*constant_size)
    = (SCHEME_WORDS_TO_BLOCKS
       (OBJECT_DATUM (header[FASL_OFFSET_CONST_SIZE])));
  (*heap_size)
    = (SCHEME_WORDS_TO_BLOCKS
       (OBJECT_DATUM (header[FASL_OFFSET_HEAP_SIZE])));
  return (1);
}

static void
describe_boolean_option (const char * name, int value)
{
  outf_fatal ("  %s: %s\n", name, (value ? "yes" : "no"));
}

static void
describe_string_option (const char * name, const char * value)
{
  outf_fatal ("  %s: %s\n", name, value);
}

static void
describe_numeric_option (const char * name, int value)
{
  outf_fatal ("  %s: %d\n", name, value);
}

static void
describe_size_option (const char * name, unsigned int value)
{
  outf_fatal ("  %s size: %d\n", name, value);
}

static void
describe_path_option (const char * name, const char ** value)
{
  outf_fatal ("  %s: ", name);
  {
    const char ** scan = value;
    outf_fatal ("%s", (*scan++));
    while (1)
      {
	const char * element = (*scan++);
	if (element == 0) break;
	outf_fatal (":%s", element);
      }
  }
  outf_fatal ("\n");
}

static void
describe_options (void)
{
  outf_fatal ("Summary of configuration options:\n");
  describe_size_option ("heap", option_heap_size);
  describe_size_option ("constant-space", option_constant_size);
  describe_size_option ("stack", option_stack_size);
  describe_path_option ("library path", option_library_path);
  if (option_fasl_file != 0)
    describe_string_option ("FASL file", option_fasl_file);
  else
    describe_string_option ("band", option_band_file);
  describe_string_option ("microcode tables", option_utabmd_file);
  {
    /* These are only relevant to bchscheme. */
    if (option_gc_directory != DEFAULT_GC_DIRECTORY)
      describe_string_option ("GC directory", option_gc_directory);
    if (option_gc_drone != DEFAULT_GC_DRONE)
      describe_string_option ("GC drone program", option_gc_drone);
    if (option_raw_gc_end_position)
      describe_numeric_option ("GC end position", option_gc_end_position);
    if (option_gc_file != DEFAULT_GC_FILE)
      describe_string_option ("GC file", option_gc_file);
    if (option_raw_gc_read_overlap)
      describe_numeric_option ("GC read overlap", option_gc_read_overlap);
    if (option_raw_gc_start_position)
      describe_numeric_option ("GC start position", option_gc_start_position);
    if (option_raw_gc_window_size)
      describe_size_option ("GC window size", option_gc_window_size);
    if (option_raw_gc_write_overlap)
      describe_numeric_option ("GC write overlap", option_gc_write_overlap);
    if (option_gc_keep)
      describe_boolean_option ("keep GC file", option_gc_keep);
  }
  describe_boolean_option ("emacs subprocess", option_emacs_subprocess);
  describe_boolean_option ("force interactive", option_force_interactive);
  describe_boolean_option ("disable core dump", option_disable_core_dump);
  describe_boolean_option ("suppress noise", option_batch_mode);
  if (option_unused_argc == 0)
    outf_fatal ("  no unused arguments\n");
  else
    {
      const char ** scan = option_unused_argv;
      const char ** end = (scan + option_unused_argc);
      outf_fatal ("  unused arguments:");
      while (scan < end)
	outf_fatal (" %s", (*scan++));
      outf_fatal ("\n");
    }
}

void
read_command_line_options (int argc, const char ** argv)
{
  bool band_sizes_valid = false;
  unsigned long band_constant_size = 0;
  unsigned long band_heap_size = 0;

  parse_standard_options (argc, argv);
  if (option_library_path != 0)
    free_parsed_path (option_library_path);
  option_library_path =
    (parse_path_string
     (standard_string_option (option_raw_library,
			      LIBRARY_PATH_VARIABLE,
			      DEFAULT_LIBRARY_PATH)));
  {
    const char * band_variable = BAND_VARIABLE;
    const char * default_band = DEFAULT_BAND;

    struct band_descriptor
      {
	const char * band;
	const char * envvar;
	int large_p;
	int compiler_support_p;
	int edwin_support_p;
      };
    struct band_descriptor available_bands [] =
      {
	{ DEFAULT_BAND, BAND_VARIABLE, 0, 0, 0 },
	{ COMPILER_DEFAULT_BAND, COMPILER_BAND_VARIABLE, 1, 1, 0 },
	{ EDWIN_DEFAULT_BAND, EDWIN_BAND_VARIABLE, 1, 0, 1 },
	{ ALL_DEFAULT_BAND, ALL_BAND_VARIABLE, 1, 1, 1 },
	{ "6001.com", EDWIN_BAND_VARIABLE, 1, 0, 1 },
	{ "mechanics.com", COMPILER_BAND_VARIABLE, 1, 1, 0 },
	{ "edwin-mechanics.com", ALL_BAND_VARIABLE, 1, 1, 1 },
	{ 0, 0, 0, 0, 0 }
      };
    struct band_descriptor * scan = available_bands;

    option_band_specified = 0;
    if (option_band_file != 0)
      xfree (option_band_file);

    while ((scan -> band) != 0)
      {
	if ((option_compiler_defaults ? (scan -> compiler_support_p) : 1)
	    && (option_edwin_defaults ? (scan -> edwin_support_p) : 1)
	    && (search_for_library_file (scan -> band)))
	  {
	    option_band_specified = 1;
	    band_variable = (scan -> envvar);
	    default_band = (scan -> band);
	    if (scan -> large_p)
	      option_large_sizes = 1;
	    break;
	  }
	scan += 1;
      }

    if (option_fasl_file != 0)
      {
	if (option_raw_band != 0)
	  conflicting_options ("fasl", "band");
	if (!FILE_READABLE (option_fasl_file))
	  {
	    outf_fatal ("%s: can't read option file: --fasl %s\n",
		     scheme_program_name, option_fasl_file);
	    termination_init_error ();
	  }
	option_large_sizes = 1;
	option_band_specified = 1;
	option_band_file = 0;
      }
    else
      {
	if (option_raw_band != 0)
	  option_band_specified = 1;
	option_band_file =
	  (standard_filename_option ("band",
				     option_raw_band,
				     band_variable,
				     default_band,
				     true));
      }
  }
  if (option_band_file != 0)
    band_sizes_valid
      = (read_band_sizes (option_band_file,
			  (&band_constant_size),
			  (&band_heap_size)));
  option_heap_size
    = ((standard_numeric_option ("heap",
				 option_raw_heap,
				 (option_large_sizes
				  ? LARGE_HEAP_VARIABLE
				  : SMALL_HEAP_VARIABLE),
				 (option_large_sizes
				  ? DEFAULT_LARGE_HEAP
				  : DEFAULT_SMALL_HEAP)))
       + (band_sizes_valid ? band_heap_size : 0));
  option_constant_size
    = (standard_numeric_option ("constant",
				option_raw_constant,
				(option_large_sizes
				 ? LARGE_CONSTANT_VARIABLE
				 : SMALL_CONSTANT_VARIABLE),
				(band_sizes_valid
				 ? band_constant_size
				 : option_large_sizes
				 ? DEFAULT_LARGE_CONSTANT
				 : DEFAULT_SMALL_CONSTANT)));
  option_stack_size
    = (standard_numeric_option ("stack",
				option_raw_stack,
				(option_large_sizes
				 ? LARGE_STACK_VARIABLE
				 : SMALL_STACK_VARIABLE),
				(option_large_sizes
				 ? DEFAULT_LARGE_STACK
				 : DEFAULT_SMALL_STACK)));
  if (option_utabmd_file != 0)
    xfree (option_utabmd_file);
  if (option_raw_utabmd != 0)
    {
      if (option_raw_utab != 0)
	conflicting_options ("utabmd", "utab");
      option_utabmd_file =
	(standard_filename_option ("utabmd",
				   option_raw_utabmd,
				   UTABMD_FILE_VARIABLE,
				   DEFAULT_UTABMD_FILE,
				   (option_fasl_file != 0)));
    }
  else
    option_utabmd_file =
      (standard_filename_option ("utab",
				 option_raw_utab,
				 UTABMD_FILE_VARIABLE,
				 DEFAULT_UTABMD_FILE,
				 (option_fasl_file != 0)));

  /* These are only meaningful for bchscheme. */

  if (option_raw_gc_file != ((char *) 0))
  {
    if (option_gc_file != ((char *) 0))
      conflicting_options ("gcfile", "gc-file");
    else
      option_gc_file = option_raw_gc_file;
  }

  {
    const char * dir = (environment_default (GC_DIRECTORY_VARIABLE, 0));
    if ((dir == 0) || (!OS_file_directory_p (dir)))
      dir = (environment_default ("TMPDIR", 0));
    if ((dir == 0) || (!OS_file_directory_p (dir)))
      dir = (environment_default ("TEMP", 0));
    if ((dir == 0) || (!OS_file_directory_p (dir)))
      dir = (environment_default ("TMP", 0));
    if ((dir == 0) || (!OS_file_directory_p (dir)))
      dir = (environment_default ("TMP", 0));
#ifdef __unix__
    if ((dir == 0) || (!OS_file_directory_p (dir)))
      {
	if (OS_file_directory_p ("/var/tmp"))
	  dir = "/var/tmp";
	if (OS_file_directory_p ("/usr/tmp"))
	  dir = "/usr/tmp";
	if (OS_file_directory_p ("/tmp"))
	  dir = "/tmp";
      }
#endif /* __unix__ */
    if ((dir == 0) || (!OS_file_directory_p (dir)))
      dir = DEFAULT_GC_DIRECTORY;
    option_gc_directory = (string_option (option_gc_directory, dir));
  }
  option_gc_drone =
    (standard_filename_option ("gc-drone",
			       option_gc_drone,
			       GC_DRONE_VARIABLE,
			       DEFAULT_GC_DRONE,
			       false));

  option_gc_end_position =
    (standard_numeric_option ("gc-end-position",
			      option_raw_gc_end_position,
			      GC_END_POSITION_VARIABLE,
			      DEFAULT_GC_END_POSITION));

  option_gc_file =
    (standard_string_option (option_gc_file,
			     GC_FILE_VARIABLE,
			     DEFAULT_GC_FILE));

  option_gc_read_overlap =
    (standard_numeric_option ("gc-read-overlap",
			      option_raw_gc_read_overlap,
			      GC_READ_OVERLAP_VARIABLE,
			      DEFAULT_GC_READ_OVERLAP));

  option_gc_start_position =
    (standard_numeric_option ("gc-start-position",
			      option_raw_gc_start_position,
			      GC_START_POSITION_VARIABLE,
			      DEFAULT_GC_START_POSITION));

  option_gc_window_size =
    (standard_numeric_option ("gc-window-size",
			      option_raw_gc_window_size,
			      GC_WINDOW_SIZE_VARIABLE,
			      DEFAULT_GC_WINDOW_SIZE));

  option_gc_write_overlap =
    (standard_numeric_option ("gc-write-overlap",
			      option_raw_gc_write_overlap,
			      GC_WRITE_OVERLAP_VARIABLE,
			      DEFAULT_GC_WRITE_OVERLAP));

  if (option_summary)
    describe_options ();

}

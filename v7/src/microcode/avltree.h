/* -*-C-*-

$Id: avltree.h,v 1.7.2.1 2005/08/22 18:05:57 cph Exp $

Copyright 1993,2000,2001,2005 Massachusetts Institute of Technology

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

#ifndef AVLTREE_H
#define AVLTREE_H

/* This file contains external declarations for a simple
   AVL tree library.
   It is used by the MIT/GNU Scheme microcode to quickly map
   names to indices into various tables.  */

#include "config.h"

extern const char * tree_error_message;
extern const char * tree_error_noise;

typedef struct tree_node_s * tree_node;

struct tree_node_s
{
  int height;
  tree_node left;
  tree_node rite;
  const char * name;
  unsigned long value;
};

extern tree_node tree_build (unsigned long, const char **, unsigned long);
extern tree_node tree_lookup (tree_node, const char *);
extern tree_node tree_insert (tree_node, const char *, unsigned long);
extern void tree_free (tree_node);

#endif /* AVLTREE_H */

/* -*-C-*-

$Id: svm1.h,v 1.1.2.7 2006/10/03 15:02:10 cph Exp $

Copyright 2005,2006 Massachusetts Institute of Technology

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

/* Compiled code interface macros for SVM v1. */

#ifndef SCM_CMPINTMD_SVM1_H
#define SCM_CMPINTMD_SVM1_H 1

#define COMPILER_PROCESSOR_TYPE COMPILER_SVM_TYPE

#define COMPILER_TEMP_SIZE 1	/* temporaries aren't used */
#define PC_ZERO_BITS 0		/* addressing is unaligned */
typedef byte_t insn_t;

#define COMPILER_REGBLOCK_N_FIXED 512
#define COMPILER_REGBLOCK_N_HOOKS 0
#define COMPILER_REGBLOCK_EXTRA_SIZE 0

/* Number of insn_t units preceding entry address in which header
   (type and offset info) is stored.  */
#define CC_ENTRY_HEADER_SIZE (CC_ENTRY_TYPE_SIZE + CC_ENTRY_OFFSET_SIZE)
#define CC_ENTRY_TYPE_SIZE 2
#define CC_ENTRY_OFFSET_SIZE 2

/* Number of insn_t units preceding entry header in which GC trap
   instructions are stored.  */
#define CC_ENTRY_GC_TRAP_SIZE 0

/* Size of execution cache in SCHEME_OBJECTS.  */
#define UUO_LINK_SIZE 2
#define READ_UUO_TARGET(a, r) read_uuo_target (a)

#define ASM_RESET_HOOK initialize_svm1

#define RETURN_TO_C(code) do						\
{									\
  (DSU_result->scheme_p) = false;					\
  ((DSU_result->arg) . interpreter_code) = (code);			\
  return;								\
} while (0)

#define RETURN_TO_SCHEME(ep) do						\
{									\
  (DSU_result->scheme_p) = true;					\
  ((DSU_result->arg) . new_pc) = (ep);					\
  return;								\
} while (0)

#define ENTER_SCHEME(ep) return (C_to_interface (ep))

extern long C_to_interface (void *);
extern void initialize_svm1 (void);
extern SCHEME_OBJECT read_uuo_target (SCHEME_OBJECT *);

#endif /* not SCM_CMPINTMD_SVM1_H */

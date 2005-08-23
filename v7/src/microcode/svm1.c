/* -*-C-*-

$Id: svm1.c,v 1.1.2.2 2005/08/23 02:55:13 cph Exp $

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

/* Scheme Virtual Machine version 1 */

#include "scheme.h"
#include "svm1-defns.h"

#define SVM1_REG_SP 0

typedef SCHEME_OBJECT word_t;	/* convenience abbreviation */

#define N_WORD_REGISTERS 0x100
#define N_FLOAT_REGISTERS 0x100

#if (N_WORD_REGISTERS < (UCHAR_MAX + 1))
#  define WORD_REGISTER_P(b) ((b) < N_WORD_REGISTERS)
#else
#  define WORD_REGISTER_P(b) true
#endif
#if (N_FLOAT_REGISTERS < (UCHAR_MAX + 1))
#  define FLOAT_REGISTER_P(b) ((b) < N_FLOAT_REGISTERS)
#else
#  define FLOAT_REGISTER_P(b) true
#endif

static byte_t * program_counter;
static word_t word_registers [N_WORD_REGISTERS];
static double float_registers [N_FLOAT_REGISTERS];

#define SBYTE (sizeof (byte_t))
#define SWORD (sizeof (word_t))
#define SFLOAT (sizeof (double))

#define PC program_counter
#define NEXT_BYTE (*program_counter++)

typedef byte_t wreg_t;
typedef byte_t freg_t;
typedef byte_t tc_t;

#define WREG_REF(wr) (word_registers [(wr)])
#define FREG_REF(fr) (float_registers [(fr)])

#define WREG_SET(wr, w) ((word_registers [(wr)]) = (w))
#define FREG_SET(fr, f) ((float_registers [(fr)]) = (f))

#define BYTE_ADDR(a) ((byte_t *) a)
#define WORD_ADDR(a) ((word_t *) a)
#define FLOAT_ADDR(a) ((double *) a)

#define BYTE_REF(a) (* (BYTE_ADDR (a)))
#define WORD_REF(a) (* (WORD_ADDR (a)))
#define FLOAT_REF(a) (* (FLOAT_ADDR (a)))


typedef byte_t * inst_defn_t (void);
static inst_defn_t * inst_defns [256];

#define DEFINE_INST(name) static byte_t * insn_##name (void)
#define NEXT_PC return (PC)
#define OFFSET_PC(o) return (PC + (o))
#define COND_OFFSET_PC(p, o) return ((p) ? (PC + (o)) : PC)
#define NEW_PC(addr) return (addr)
static void initialize_inst_defns (void);
static long svm1_result;

#define EXIT_VM(code) do						\
{									\
  svm1_result = (code);							\
  return (0);								\
} while (0)

typedef struct address_s address_t;
typedef word_t address_value_t (address_t *);
typedef void address_decoder_t (address_t *);
static address_decoder_t * address_decoders [256];

struct address_s
{
  wreg_t r1;
  wreg_t r2;
  word_t n1;
  long n2;
  address_value_t * value;
};
#define ADDRESS_VALUE(name) ((name.value) (&name))

#define DEFINE_ADDRESS_DECODER(name)					\
  static void decode_addr_##name (address_t * address)
#define DECODE_ADDRESS(name) address_t name; decode_address (&name)
static void decode_address (address_t *);
static void initialize_address_decoders (void);

typedef byte_t * trap_0_t (void);
typedef byte_t * trap_1_t (wreg_t);
typedef byte_t * trap_2_t (wreg_t, wreg_t);
typedef byte_t * trap_3_t (wreg_t, wreg_t, wreg_t);

static trap_0_t * traps_0 [256];
static trap_1_t * traps_1 [256];
static trap_2_t * traps_2 [256];
static trap_3_t * traps_3 [256];

#define DECODE_TRAP_0(name) byte_t name = NEXT_BYTE
#define DECODE_TRAP_1(name) byte_t name = NEXT_BYTE
#define DECODE_TRAP_2(name) byte_t name = NEXT_BYTE
#define DECODE_TRAP_3(name) byte_t name = NEXT_BYTE

static void initialize_traps (void);

static int initialized_p = 0;
static int little_endian_p;

static byte_t * execute_instruction (void);

static void
compute_little_endian_p (void)
{
  union
    {
      unsigned long n;
      char b [(sizeof (unsigned long))];
    } ue;
  (ue.n) = 1;
  little_endian_p = (((ue.b) [0]) == 1);
}

void
initialize_svm1 (void)
{
  unsigned int i;

  if (!initialized_p)
    {
      compute_little_endian_p ();
      initialize_inst_defns ();
      initialize_address_decoders ();
      initialize_traps ();
      initialized_p = 1;
    }
  for (i = 0; (i < N_WORD_REGISTERS); i += 1)
    WREG_SET (i, 0);
  for (i = 0; (i < N_FLOAT_REGISTERS); i += 1)
    WREG_SET (i, 0.0);
}

#define IMPORT_REGS() do						\
{									\
  WREG_SET (SVM1_REG_STACK_POINTER, ((SCHEME_OBJECT) stack_pointer));	\
  WREG_SET (SVM1_REG_FREE_POINTER, ((SCHEME_OBJECT) Free));		\
  WREG_SET (SVM1_REG_VALUE, GET_VAL);					\
} while (0)

#define EXPORT_REGS() do						\
{									\
  stack_pointer								\
    = ((SCHEME_OBJECT *) (WREG_REF (SVM1_REG_STACK_POINTER)));		\
  Free = ((SCHEME_OBJECT *) (WREG_REF (SVM1_REG_FREE_POINTER)));	\
  SET_VAL (WREG_REF (SVM1_REG_VALUE));					\
} while (0)

long
C_to_interface (void * address)
{
  IMPORT_REGS ();
  PC = address;
  while (1)
    {
      byte_t * new_pc = (execute_instruction ());
      if (new_pc == 0)
	break;
      PC = new_pc;
    }
  EXPORT_REGS ();
  return (svm1_result);
}

static jmp_buf k_execute_instruction;

static byte_t *
execute_instruction (void)
{
  byte_t b = NEXT_BYTE;
  inst_defn_t * defn = (inst_defns[b]);
  if (defn == 0)
    {
      svm1_result = ERR_COMPILED_CODE_ERROR;
      return (0);
    }
  if ((setjmp (k_execute_instruction)) != 0)
    return (0);
  return ((*defn) ());
}

static void
signal_illegal_instruction (void)
{
  svm1_result = ERR_COMPILED_CODE_ERROR;
  longjmp (k_execute_instruction, 1);
}

#define TO_SIGNED(n) ((long) (n))
#define FROM_SIGNED(n) ((word_t) (n))

#define SIGNED_UNARY(op, a1)						\
  (FROM_SIGNED (op (TO_SIGNED (a1))))

#define SIGNED_BINARY(op, a1, a2)					\
  (FROM_SIGNED ((TO_SIGNED (a1)) op (TO_SIGNED (a2))))

#if 0
/* The above definition isn't guaranteed to work in ANSI C, but in
   practice it usually does.  Here's an alternative that should always
   work (in machines that use 2's complement).  */
#define TO_SIGNED(n) (to_signed (n))

static long
to_signed (word_t n)
{
  union { unsigned long n1; long n2; } us;
  (us.n1) = n;
  return (us.n2);
}
#endif

/* Primitive decoders */

#define DECODE_WORD_REGISTER(name)  wreg_t name = (decode_wreg ())
#define DECODE_FLOAT_REGISTER(name) freg_t name = (decode_freg ())
#define DECODE_TYPE_WORD(name)        tc_t name = (decode_type_word ())
#define DECODE_UNSIGNED_8(name)     word_t name = (decode_unsigned_8 ())
#define DECODE_UNSIGNED_16(name)    word_t name = (decode_unsigned_16 ())
#define DECODE_UNSIGNED_32(name)    word_t name = (decode_unsigned_32 ())
#define DECODE_SIGNED_8(name)         long name = (decode_signed_8 ())
#define DECODE_SIGNED_16(name)        long name = (decode_signed_16 ())
#define DECODE_SIGNED_32(name)        long name = (decode_signed_32 ())
#define DECODE_FLOAT(name)          double name = (decode_float ())

static wreg_t
decode_wreg (void)
{
  byte_t b = NEXT_BYTE;
  if (!WORD_REGISTER_P (b))
    signal_illegal_instruction ();
  return (b);
}

static freg_t
decode_freg (void)
{
  byte_t b = NEXT_BYTE;
  if (!FLOAT_REGISTER_P (b))
    signal_illegal_instruction ();
  return (b);
}

static tc_t
decode_type_word (void)
{
  byte_t b = NEXT_BYTE;
  if (b >= N_TYPE_CODES)
    signal_illegal_instruction ();
  return (b);
}

static word_t
decode_unsigned_8 (void)
{
  return (NEXT_BYTE);
}

static word_t
decode_unsigned_16 (void)
{
  word_t b0 = NEXT_BYTE;
  word_t b1 = NEXT_BYTE;
  return ((b1 << 8) | b0);
}

static word_t
decode_unsigned_32 (void)
{
  word_t b0 = NEXT_BYTE;
  word_t b1 = NEXT_BYTE;
  word_t b2 = NEXT_BYTE;
  word_t b3 = NEXT_BYTE;
  return ((b3 << 24) | (b2 << 16) | (b1 << 8) | b0);
}

static long
decode_signed_8 (void)
{
  long b = NEXT_BYTE;
  return ((b < 0x80) ? b : (b - 0x100));
}

static long
decode_signed_16 (void)
{
  long n = (decode_unsigned_16 ());
  return ((n < 0x8000) ? n : (n - 0x10000));
}

static long
decode_signed_32 (void)
{
  word_t n = (decode_unsigned_32 ());
  if (n < 0x80000000UL)
    return ((long) n);
#if (LONG_MAX > 0x7FFFFFFFUL)
  return (((long) n) - 0x100000000L);
#else
  n -= 0x80000000UL;
  {
    long r = ((long) n);
    r -= 0x40000000L;
    r -= 0x40000000L;
    return (r);
  }
#endif
}

static double
decode_float (void)
{
  union { double f; byte_t b [(sizeof (double))]; } x;

  if (little_endian_p)
    {
      unsigned int i = 0;
      while (i < (sizeof (double)))
	{
	  ((x.b) [i]) = NEXT_BYTE;
	  i += 1;
	}
    }
  else
    {
      unsigned int i = (sizeof (double));
      while (i > 0)
	{
	  i -= 1;
	  ((x.b) [i]) = NEXT_BYTE;
	}
    }
  return (x.f);
}

/* Instruction definitions */

DEFINE_INST (store_b_wr_addr)
{
  DECODE_SVM1_INST_STORE_B_WR_ADDR (source, address);
  (BYTE_REF (ADDRESS_VALUE (address))) = ((WREG_REF (source)) & 0xFF);
  NEXT_PC;
}

DEFINE_INST (store_w_wr_addr)
{
  DECODE_SVM1_INST_STORE_W_WR_ADDR (source, address);
  (WORD_REF (ADDRESS_VALUE (address))) = (WREG_REF (source));
  NEXT_PC;
}

DEFINE_INST (store_f_fr_addr)
{
  DECODE_SVM1_INST_STORE_F_FR_ADDR (source, address);
  (FLOAT_REF (ADDRESS_VALUE (address))) = (FREG_REF (source));
  NEXT_PC;
}

DEFINE_INST (load_b_wr_addr)
{
  DECODE_SVM1_INST_LOAD_B_WR_ADDR (target, address);
  WREG_SET (target, (BYTE_REF (ADDRESS_VALUE (address))));
  NEXT_PC;
}

DEFINE_INST (load_w_wr_addr)
{
  DECODE_SVM1_INST_LOAD_W_WR_ADDR (target, address);
  WREG_SET (target, (WORD_REF (ADDRESS_VALUE (address))));
  NEXT_PC;
}

DEFINE_INST (load_f_fr_addr)
{
  DECODE_SVM1_INST_LOAD_F_FR_ADDR (target, address);
  FREG_SET (target, (FLOAT_REF (ADDRESS_VALUE (address))));
  NEXT_PC;
}

DEFINE_INST (load_address_addr)
{
  DECODE_SVM1_INST_LOAD_ADDRESS_ADDR (target, address);
  WREG_SET (target, (ADDRESS_VALUE (address)));
  NEXT_PC;
}

DEFINE_INST (load_immediate_wr_s8)
{
  DECODE_SVM1_INST_LOAD_IMMEDIATE_WR_S8 (target, value);
  WREG_SET (target, value);
  NEXT_PC;
}

DEFINE_INST (load_immediate_wr_s16)
{
  DECODE_SVM1_INST_LOAD_IMMEDIATE_WR_S16 (target, value);
  WREG_SET (target, value);
  NEXT_PC;
}

DEFINE_INST (load_immediate_wr_s32)
{
  DECODE_SVM1_INST_LOAD_IMMEDIATE_WR_S32 (target, value);
  WREG_SET (target, value);
  NEXT_PC;
}

DEFINE_INST (load_immediate_wr_u8)
{
  DECODE_SVM1_INST_LOAD_IMMEDIATE_WR_U8 (target, value);
  WREG_SET (target, value);
  NEXT_PC;
}

DEFINE_INST (load_immediate_wr_u16)
{
  DECODE_SVM1_INST_LOAD_IMMEDIATE_WR_U16 (target, value);
  WREG_SET (target, value);
  NEXT_PC;
}

DEFINE_INST (load_immediate_wr_u32)
{
  DECODE_SVM1_INST_LOAD_IMMEDIATE_WR_U32 (target, value);
  WREG_SET (target, value);
  NEXT_PC;
}

DEFINE_INST (load_immediate_fr_flt)
{
  DECODE_SVM1_INST_LOAD_IMMEDIATE_FR_FLT (target, value);
  FREG_SET (target, value);
  NEXT_PC;
}

#define X_MAKE_OBJECT(t, d)						\
  (MAKE_OBJECT (((t) & TYPE_CODE_MASK), ((d) & DATUM_MASK)))

#define X_MAKE_PTR(t, a) (X_MAKE_OBJECT (t, (ADDRESS_TO_DATUM (a))))

DEFINE_INST (load_non_pointer_tc_u8)
{
  DECODE_SVM1_INST_LOAD_NON_POINTER_TC_U8 (target, type, datum);
  WREG_SET (target, (X_MAKE_OBJECT (type, datum)));
  NEXT_PC;
}

DEFINE_INST (load_non_pointer_tc_u16)
{
  DECODE_SVM1_INST_LOAD_NON_POINTER_TC_U16 (target, type, datum);
  WREG_SET (target, (X_MAKE_OBJECT (type, datum)));
  NEXT_PC;
}

DEFINE_INST (load_non_pointer_tc_u32)
{
  DECODE_SVM1_INST_LOAD_NON_POINTER_TC_U32 (target, type, datum);
  WREG_SET (target, (X_MAKE_OBJECT (type, datum)));
  NEXT_PC;
}

DEFINE_INST (load_non_pointer_wr_u8)
{
  DECODE_SVM1_INST_LOAD_NON_POINTER_WR_U8 (target, type, datum);
  WREG_SET (target, (X_MAKE_OBJECT ((WREG_REF (type)), datum)));
  NEXT_PC;
}

DEFINE_INST (load_non_pointer_wr_u16)
{
  DECODE_SVM1_INST_LOAD_NON_POINTER_WR_U16 (target, type, datum);
  WREG_SET (target, (X_MAKE_OBJECT ((WREG_REF (type)), datum)));
  NEXT_PC;
}

DEFINE_INST (load_non_pointer_wr_u32)
{
  DECODE_SVM1_INST_LOAD_NON_POINTER_WR_U32 (target, type, datum);
  WREG_SET (target, (X_MAKE_OBJECT ((WREG_REF (type)), datum)));
  NEXT_PC;
}

DEFINE_INST (load_non_pointer_tc_wr)
{
  DECODE_SVM1_INST_LOAD_NON_POINTER_TC_WR (target, type, datum);
  WREG_SET (target, (X_MAKE_OBJECT (type, (WREG_REF (datum)))));
  NEXT_PC;
}

DEFINE_INST (load_non_pointer)
{
  DECODE_SVM1_INST_LOAD_NON_POINTER (target, type, datum);
  WREG_SET (target, (X_MAKE_OBJECT ((WREG_REF (type)), (WREG_REF (datum)))));
  NEXT_PC;
}

DEFINE_INST (load_pointer_tc_wr)
{
  DECODE_SVM1_INST_LOAD_POINTER_TC_WR (target, type, address);
  WREG_SET (target, (X_MAKE_PTR (type, (WREG_REF (address)))));
  NEXT_PC;
}

DEFINE_INST (load_pointer)
{
  DECODE_SVM1_INST_LOAD_POINTER (target, type, address);
  WREG_SET (target, (X_MAKE_PTR ((WREG_REF (type)), (WREG_REF (address)))));
  NEXT_PC;
}

static void
copy_block (word_t * to, word_t * from, word_t n_words)
{
  if (to > from)
    {
      word_t * p1 = (to + n_words);
      word_t * p2 = (from + n_words);
      while (p2 > from)
	(*--p1) = (*--p2);
    }
  else if (to < from)
    {
      word_t * p2 = (from + n_words);
      while (from < p2)
	(*to++) = (*from++);
    }
}

DEFINE_INST (copy_block_u8_w)
{
  DECODE_SVM1_INST_COPY_BLOCK_U8_W (r_to, r_from, n_words);
  copy_block ((WORD_ADDR (WREG_REF (r_to))),
	      (WORD_ADDR (WREG_REF (r_from))),
	      n_words);
  NEXT_PC;
}

DEFINE_INST (copy_block_wr_w)
{
  DECODE_SVM1_INST_COPY_BLOCK_WR_W (r_to, r_from, r_n_words);
  copy_block ((WORD_ADDR (WREG_REF (r_to))),
	      (WORD_ADDR (WREG_REF (r_from))),
	      (WREG_REF (r_n_words)));
  NEXT_PC;
}

DEFINE_INST (jump_pcr_s8)
{
  DECODE_SVM1_INST_JUMP_PCR_S8 (offset);
  OFFSET_PC (offset);
}

DEFINE_INST (jump_pcr_s16)
{
  DECODE_SVM1_INST_JUMP_PCR_S16 (offset);
  OFFSET_PC (offset);
}

DEFINE_INST (jump_pcr_s32)
{
  DECODE_SVM1_INST_JUMP_PCR_S32 (offset);
  OFFSET_PC (offset);
}

DEFINE_INST (jump_indir_wr)
{
  DECODE_SVM1_INST_JUMP_INDIR_WR (address);
  NEW_PC (BYTE_ADDR (WREG_REF (address)));
}

#define IJUMP(offset)							\
  NEW_PC (BYTE_ADDR (OBJECT_ADDRESS (* ((SCHEME_OBJECT *) (PC + (offset))))))

DEFINE_INST (ijump_u8)
{
  DECODE_SVM1_INST_IJUMP_U8 (offset);
  IJUMP (offset);
}

DEFINE_INST (ijump_u16)
{
  DECODE_SVM1_INST_IJUMP_U16 (offset);
  IJUMP (offset);
}

DEFINE_INST (ijump_u32)
{
  DECODE_SVM1_INST_IJUMP_U32 (offset);
  IJUMP (offset);
}

static void
push_icall_entry (void * entry)
{
  stack_pointer = ((SCHEME_OBJECT *) (WREG_REF (SVM1_REG_STACK_POINTER)));
  STACK_PUSH (MAKE_POINTER_OBJECT (TC_COMPILED_ENTRY, entry));
  WREG_SET (SVM1_REG_STACK_POINTER, ((SCHEME_OBJECT) stack_pointer));
}

DEFINE_INST (icall_u8)
{
  DECODE_SVM1_INST_ICALL_U8 (offset);
  push_icall_entry (PC - 2);
  IJUMP (offset);
}

DEFINE_INST (icall_u16)
{
  DECODE_SVM1_INST_ICALL_U16 (offset);
  push_icall_entry (PC - 3);
  IJUMP (offset);
}

DEFINE_INST (icall_u32)
{
  DECODE_SVM1_INST_ICALL_U32 (offset);
  push_icall_entry (PC - 5);
  IJUMP (offset);
}

/* Conditional jumps */

#define DEFINE_CJ_1(pl, pu, rl, ru, z, sl, su)				\
DEFINE_INST (cjump_##pl##_##rl##_##rl##_pcr_##sl)			\
{									\
  DECODE_SVM1_INST_CJUMP_##pu##_##ru##_##ru##_PCR_##su			\
    (source1, source2, offset);						\
  CJ_PCR (CMP_##pu ((WREG_REF (source1)), (WREG_REF (source2))));	\
}

#define DEFINE_CJ_2(pl, pu, rl, ru, z, sl, su)				\
DEFINE_INST (cjump_##pl##_##rl##_pcr_##sl)				\
{									\
  DECODE_SVM1_INST_CJUMP_##pu##_##ru##_PCR_##su (source, offset);	\
  CJ_PCR (CMP_##pu ((WREG_REF (source)), z));				\
}

#define CJ_PCR(p) COND_OFFSET_PC (p, offset)

#define DEFINE_CJ_3(pl, pu, rl, ru, z, sl, su)				\
DEFINE_CJ_1 (pl, pu, rl, ru, z, sl, su)					\
DEFINE_CJ_2 (pl, pu, rl, ru, z, sl, su)

#define DEFINE_CJ_4(pl, pu, rl, ru, z)					\
DEFINE_CJ_3 (pl, pu, rl, ru, z, s8, S8)					\
DEFINE_CJ_3 (pl, pu, rl, ru, z, s16, S16)				\
DEFINE_CJ_3 (pl, pu, rl, ru, z, s32, S32)

#define DEFINE_CJ_WR(pl, pu) DEFINE_CJ_4 (pl, pu, wr, WR, 0)
#define DEFINE_CJ_FR(pl, pu) DEFINE_CJ_4 (pl, pu, fr, FR, 0.0)

#define CMP_EQ(a, b) ((a) == (b))
#define CMP_NEQ(a, b) ((a) != (b))
#define CMP_LT(a, b) ((a) < (b))
#define CMP_LE(a, b) ((a) <= (b))
#define CMP_GT(a, b) ((a) > (b))
#define CMP_GE(a, b) ((a) >= (b))
#define CMP_SLT(a, b) ((TO_SIGNED (a)) < (TO_SIGNED (b)))
#define CMP_SLE(a, b) ((TO_SIGNED (a)) <= (TO_SIGNED (b)))
#define CMP_SGT(a, b) ((TO_SIGNED (a)) > (TO_SIGNED (b)))
#define CMP_SGE(a, b) ((TO_SIGNED (a)) >= (TO_SIGNED (b)))
#define CMP_CMP(a, b) (cmp_cmp ((a), (b)))
#define CMP_NCMP(a, b) (!cmp_cmp ((a), (b)))

static int
cmp_cmp (double a, double b)
{
  return ((a < b) || (a > b) || (a == b));
}

DEFINE_CJ_WR (eq, EQ)
DEFINE_CJ_WR (neq, NEQ)
DEFINE_CJ_WR (lt, LT)		/* compare to zero always false */
DEFINE_CJ_WR (le, LE)
DEFINE_CJ_WR (gt, GT)
DEFINE_CJ_WR (ge, GE)		/* compare to zero always true */
DEFINE_CJ_WR (slt, SLT)
DEFINE_CJ_WR (sle, SLE)
DEFINE_CJ_WR (sgt, SGT)
DEFINE_CJ_WR (sge, SGE)

DEFINE_CJ_FR (eq, EQ)
DEFINE_CJ_FR (neq, NEQ)
DEFINE_CJ_FR (lt, LT)
DEFINE_CJ_FR (le, LE)
DEFINE_CJ_FR (gt, GT)
DEFINE_CJ_FR (ge, GE)
DEFINE_CJ_FR (cmp, CMP)
DEFINE_CJ_FR (ncmp, NCMP)

#define DEFINE_CJF_1(pl, pu, sl, su)					\
DEFINE_INST (cjump_##pl##_wr_pcr_##sl)					\
{									\
  DECODE_SVM1_INST_CJUMP_##pu##_WR_PCR_##su (source, offset);		\
  CJ_PCR (CMP_##pu (WREG_REF (source)));				\
}

#define DEFINE_CJF(pl, pu)						\
DEFINE_CJF_1 (pl, pu, s8, S8)						\
DEFINE_CJF_1 (pl, pu, s16, S16)						\
DEFINE_CJF_1 (pl, pu, s32, S32)

#define CMP_FIX(a) (LONG_TO_FIXNUM_P (a))
#define CMP_NFIX(a) (!CMP_FIX (a))
#define CMP_IFIX(a) (((a) & SIGN_MASK) == (TC_FIXNUM * 2))
#define CMP_NIFIX(a) (!CMP_IFIX (a))

DEFINE_CJF (fix, FIX)
DEFINE_CJF (nfix, NFIX)
DEFINE_CJF (ifix, IFIX)
DEFINE_CJF (nifix, NIFIX)

DEFINE_INST (trap_0)
{
  DECODE_SVM1_INST_TRAP_TRAP_0 (code);
  trap_0_t * proc = (traps_0[code]);
  if (proc == 0)
    signal_illegal_instruction ();
  return ((*proc) ());
}

DEFINE_INST (trap_1)
{
  DECODE_SVM1_INST_TRAP_TRAP_1_WR (code, r1);
  trap_1_t * proc = (traps_1[code]);
  if (proc == 0)
    signal_illegal_instruction ();
  return ((*proc) (r1));
}

DEFINE_INST (trap_2)
{
  DECODE_SVM1_INST_TRAP_TRAP_2_WR (code, r1, r2);
  trap_2_t * proc = (traps_2[code]);
  if (proc == 0)
    signal_illegal_instruction ();
  return ((*proc) (r1, r2));
}

DEFINE_INST (trap_3)
{
  DECODE_SVM1_INST_TRAP_TRAP_3_WR (code, r1, r2, r3);
  trap_3_t * proc = (traps_3[code]);
  if (proc == 0)
    signal_illegal_instruction ();
  return ((*proc) (r1, r2, r3));
}

#define TRAP_PREFIX(result)						\
  utility_result_t result;						\
  EXPORT_REGS ()

#define TRAP_SUFFIX(result)						\
  if ((result).scheme_p)						\
    {									\
      IMPORT_REGS ();							\
      NEW_PC ((result).arg.new_pc);					\
    }									\
  else									\
    EXIT_VM ((result).arg.interpreter_code)

#define DEFINE_TRAP_0(nl, util_name)					\
byte_t *								\
trap_##nl (void)							\
{									\
  TRAP_PREFIX (result);							\
  comutil_##util_name ((&result),					\
		       0,						\
		       0,						\
		       0,						\
		       0);						\
  TRAP_SUFFIX (result);							\
}

#define DEFINE_TRAP_1(nl, util_name)					\
byte_t *								\
trap_##nl (wreg_t source1)						\
{									\
  TRAP_PREFIX (result);							\
  comutil_##util_name ((&result),					\
		       ((long) (WREG_REF (source1))),			\
		       0,						\
		       0,						\
		       0);						\
  TRAP_SUFFIX (result);							\
}

#define DEFINE_TRAP_2(nl, util_name)					\
byte_t *								\
trap_##nl (wreg_t source1, wreg_t source2)				\
{									\
  TRAP_PREFIX (result);							\
  comutil_##util_name ((&result),					\
		       ((long) (WREG_REF (source1))),			\
		       ((long) (WREG_REF (source2))),			\
		       0,						\
		       0);						\
  TRAP_SUFFIX (result);							\
}

#define DEFINE_TRAP_3(nl, util_name)					\
byte_t *								\
trap_##nl (wreg_t source1, wreg_t source2, wreg_t source3)		\
{									\
  TRAP_PREFIX (result);							\
  comutil_##util_name ((&result),					\
		       ((long) (WREG_REF (source1))),			\
		       ((long) (WREG_REF (source2))),			\
		       ((long) (WREG_REF (source3))),			\
		       0);						\
  TRAP_SUFFIX (result);							\
}

#define DEFINE_TRAP_R0(nl, util_name)					\
byte_t *								\
trap_##nl (void)							\
{									\
  TRAP_PREFIX (result);							\
  comutil_##util_name ((&result),					\
		       ((long) (PC + CC_ENTRY_HEADER_SIZE)),		\
		       0,						\
		       0,						\
		       0);						\
  TRAP_SUFFIX (result);							\
}

#define DEFINE_TRAP_R1(nl, util_name)					\
byte_t *								\
trap_##nl (wreg_t source1)						\
{									\
  TRAP_PREFIX (result);							\
  comutil_##util_name ((&result),					\
		       ((long) (PC + CC_ENTRY_HEADER_SIZE)),		\
		       ((long) (WREG_REF (source1))),			\
		       0,						\
		       0);						\
  TRAP_SUFFIX (result);							\
}

#define DEFINE_TRAP_R2(nl, util_name)					\
byte_t *								\
trap_##nl (wreg_t source1, wreg_t source2)				\
{									\
  TRAP_PREFIX (result);							\
  comutil_##util_name ((&result),					\
		       ((long) (PC + CC_ENTRY_HEADER_SIZE)),		\
		       ((long) (WREG_REF (source1))),			\
		       ((long) (WREG_REF (source2))),			\
		       0);						\
  TRAP_SUFFIX (result);							\
}

#define DEFINE_TRAP_R3(nl, util_name)					\
byte_t *								\
trap_##nl (wreg_t source1, wreg_t source2, wreg_t source3)		\
{									\
  TRAP_PREFIX (result);							\
  comutil_##util_name ((&result),					\
		       ((long) (PC + CC_ENTRY_HEADER_SIZE)),		\
		       ((long) (WREG_REF (source1))),			\
		       ((long) (WREG_REF (source2))),			\
		       ((long) (WREG_REF (source3))));			\
  TRAP_SUFFIX (result);							\
}

#define DEFINE_TRAMPOLINE(nl, util_name)				\
byte_t *								\
trap_##nl (void)							\
{									\
  TRAP_PREFIX (result);							\
  comutil_##util_name							\
    ((&result),								\
     ((long)								\
      (trampoline_storage						\
       (cc_entry_address_to_block_address (PC - 2)))),			\
     0,									\
     0,									\
     0);								\
  TRAP_SUFFIX (result);							\
}

DEFINE_TRAP_0 (add, plus)
DEFINE_TRAP_0 (decrement, decrement)
DEFINE_TRAP_0 (divide, divide)
DEFINE_TRAP_0 (equal_p, equal)
DEFINE_TRAP_0 (greater_p, greater)
DEFINE_TRAP_0 (increment, increment)
DEFINE_TRAP_0 (less_p, less)
DEFINE_TRAP_0 (modulo, modulo)
DEFINE_TRAP_0 (multiply, multiply)
DEFINE_TRAP_0 (negative_p, negative)
DEFINE_TRAP_0 (positive_p, positive)
DEFINE_TRAP_0 (quotient, quotient)
DEFINE_TRAP_0 (remainder, remainder)
DEFINE_TRAP_0 (subtract, minus)
DEFINE_TRAP_0 (zero_p, zero)

DEFINE_TRAP_1 (error, error)
DEFINE_TRAP_1 (primitive_apply, primitive_apply)
DEFINE_TRAP_1 (primitive_lexpr_apply, primitive_lexpr_apply)

DEFINE_TRAP_R1 (lookup, lookup_trap)
DEFINE_TRAP_R1 (safe_lookup, safe_lookup_trap)
DEFINE_TRAP_R1 (unassigned_p, unassigned_p_trap)

DEFINE_TRAP_2 (apply, apply)
DEFINE_TRAP_2 (lexpr_apply, lexpr_apply)

DEFINE_TRAP_R2 (assignment, assignment_trap)
DEFINE_TRAP_R2 (primitive_error, primitive_error)

DEFINE_TRAP_3 (cache_reference_apply, cache_lookup_apply)

DEFINE_TRAP_R3 (link, link)

DEFINE_TRAMPOLINE (operator_1_0, operator_1_0_trap)
DEFINE_TRAMPOLINE (operator_2_0, operator_2_0_trap)
DEFINE_TRAMPOLINE (operator_2_1, operator_2_1_trap)
DEFINE_TRAMPOLINE (operator_3_0, operator_3_0_trap)
DEFINE_TRAMPOLINE (operator_3_1, operator_3_1_trap)
DEFINE_TRAMPOLINE (operator_3_2, operator_3_2_trap)
DEFINE_TRAMPOLINE (operator_4_0, operator_4_0_trap)
DEFINE_TRAMPOLINE (operator_4_1, operator_4_1_trap)
DEFINE_TRAMPOLINE (operator_4_2, operator_4_2_trap)
DEFINE_TRAMPOLINE (operator_4_3, operator_4_3_trap)
DEFINE_TRAMPOLINE (operator_apply, operator_apply_trap)
DEFINE_TRAMPOLINE (operator_lexpr, operator_lexpr_trap)
DEFINE_TRAMPOLINE (operator_lookup, operator_lookup_trap)
DEFINE_TRAMPOLINE (operator_primitive, operator_primitive_trap)
DEFINE_TRAMPOLINE (reflect_to_interface, reflect_to_interface)
DEFINE_TRAMPOLINE (return_to_interpreter, return_to_interpreter)

#define BIND_TRAP_0(index, name)					\
  (traps_0[SVM1_TRAP_0_##index]) = trap_##name

#define BIND_TRAP_1(index, name)					\
  (traps_1[SVM1_TRAP_1_##index]) = trap_##name

#define BIND_TRAP_2(index, name)					\
  (traps_2[SVM1_TRAP_2_##index]) = trap_##name

#define BIND_TRAP_3(index, name)					\
  (traps_3[SVM1_TRAP_3_##index]) = trap_##name

static void
initialize_traps (void)
{
  memset (traps_0, 0, (sizeof (traps_0)));
  BIND_TRAP_0 (ADD, add);
  BIND_TRAP_0 (DECREMENT, decrement);
  BIND_TRAP_0 (DIVIDE, divide);
  BIND_TRAP_0 (EQUAL_P, equal_p);
  BIND_TRAP_0 (GREATER_P, greater_p);
  BIND_TRAP_0 (INCREMENT, increment);
  BIND_TRAP_0 (LESS_P, less_p);
  BIND_TRAP_0 (MODULO, modulo);
  BIND_TRAP_0 (MULTIPLY, multiply);
  BIND_TRAP_0 (NEGATIVE_P, negative_p);
  BIND_TRAP_0 (OPERATOR_1_0, operator_1_0);
  BIND_TRAP_0 (OPERATOR_2_0, operator_2_0);
  BIND_TRAP_0 (OPERATOR_2_1, operator_2_1);
  BIND_TRAP_0 (OPERATOR_3_0, operator_3_0);
  BIND_TRAP_0 (OPERATOR_3_1, operator_3_1);
  BIND_TRAP_0 (OPERATOR_3_2, operator_3_2);
  BIND_TRAP_0 (OPERATOR_4_0, operator_4_0);
  BIND_TRAP_0 (OPERATOR_4_1, operator_4_1);
  BIND_TRAP_0 (OPERATOR_4_2, operator_4_2);
  BIND_TRAP_0 (OPERATOR_4_3, operator_4_3);
  BIND_TRAP_0 (OPERATOR_APPLY, operator_apply);
  BIND_TRAP_0 (OPERATOR_LEXPR, operator_lexpr);
  BIND_TRAP_0 (OPERATOR_LOOKUP, operator_lookup);
  BIND_TRAP_0 (OPERATOR_PRIMITIVE, operator_primitive);
  BIND_TRAP_0 (POSITIVE_P, positive_p);
  BIND_TRAP_0 (QUOTIENT, quotient);
  BIND_TRAP_0 (REFLECT_TO_INTERFACE, reflect_to_interface);
  BIND_TRAP_0 (REMAINDER, remainder);
  BIND_TRAP_0 (RETURN_TO_INTERPRETER, return_to_interpreter);
  BIND_TRAP_0 (SUBTRACT, subtract);
  BIND_TRAP_0 (ZERO_P, zero_p);

  memset (traps_1, 0, (sizeof (traps_1)));
  BIND_TRAP_1 (ERROR, error);
  BIND_TRAP_1 (LOOKUP, lookup);
  BIND_TRAP_1 (PRIMITIVE_APPLY, primitive_apply);
  BIND_TRAP_1 (PRIMITIVE_LEXPR_APPLY, primitive_lexpr_apply);
  BIND_TRAP_1 (SAFE_LOOKUP, safe_lookup);
  BIND_TRAP_1 (UNASSIGNED_P, unassigned_p);

  memset (traps_2, 0, (sizeof (traps_2)));
  BIND_TRAP_2 (APPLY, apply);
  BIND_TRAP_2 (ASSIGNMENT, assignment);
  BIND_TRAP_2 (LEXPR_APPLY, lexpr_apply);
  BIND_TRAP_2 (PRIMITIVE_ERROR, primitive_error);

  memset (traps_3, 0, (sizeof (traps_3)));
  BIND_TRAP_3 (CACHE_REFERENCE_APPLY, cache_reference_apply);
  BIND_TRAP_3 (LINK, link);
}

#define DEFINE_INTERRUPT_TEST(name, a1, a2)				\
DEFINE_INST (interrupt_test_##name)					\
{									\
  if ((((SCHEME_OBJECT *) (WREG_REF (SVM1_REG_FREE_POINTER)))		\
       >= GET_MEMTOP)							\
      || (((SCHEME_OBJECT *) (WREG_REF (SVM1_REG_STACK_POINTER)))	\
	  >= GET_STACK_GUARD))						\
    {									\
      utility_result_t result;						\
									\
      EXPORT_REGS ();							\
      compiler_interrupt_common ((&result), (a1), (a2));		\
      TRAP_SUFFIX (result);						\
    }									\
  NEXT_PC;								\
}

DEFINE_INTERRUPT_TEST (procedure, (PC - 1), SHARP_F)
DEFINE_INTERRUPT_TEST (closure, 0, SHARP_F)
DEFINE_INTERRUPT_TEST (ic_procedure, (PC - 1), GET_ENV)
DEFINE_INTERRUPT_TEST (continuation, (PC - 1), GET_VAL)

DEFINE_INTERRUPT_TEST (dynamic_link,
		       (PC - 1),
		       (MAKE_CC_STACK_ENV (WREG_REF (SVM1_REG_DYNAMIC_LINK))))

DEFINE_INST (flonum_header_u8)
{
  DECODE_SVM1_INST_FLONUM_HEADER_U8 (target, value);
  WREG_SET (target,
	    (MAKE_OBJECT (TC_MANIFEST_NM_VECTOR, (FLONUM_SIZE * value))));
  NEXT_PC;
}

DEFINE_INST (flonum_header_u16)
{
  DECODE_SVM1_INST_FLONUM_HEADER_U16 (target, value);
  WREG_SET (target,
	    (MAKE_OBJECT (TC_MANIFEST_NM_VECTOR, (FLONUM_SIZE * value))));
  NEXT_PC;
}

DEFINE_INST (flonum_header_u32)
{
  DECODE_SVM1_INST_FLONUM_HEADER_U32 (target, value);
  WREG_SET (target,
	    (X_MAKE_OBJECT (TC_MANIFEST_NM_VECTOR, (FLONUM_SIZE * value))));
  NEXT_PC;
}

DEFINE_INST (flonum_header)
{
  DECODE_SVM1_INST_FLONUM_HEADER (target, source);
  WREG_SET (target,
	    (X_MAKE_OBJECT (TC_MANIFEST_NM_VECTOR,
			    (FLONUM_SIZE * (WREG_REF (source))))));
  NEXT_PC;
}

DEFINE_INST (copy_wr)
{
  DECODE_SVM1_INST_COPY_WR (target, source);
  WREG_SET (target, (WREG_REF (source)));
  NEXT_PC;
}

DEFINE_INST (copy_fr)
{
  DECODE_SVM1_INST_COPY_FR (target, source);
  FREG_SET (target, (FREG_REF (source)));
  NEXT_PC;
}

DEFINE_INST (negate_wr)
{
  DECODE_SVM1_INST_NEGATE_WR (target, source);
  WREG_SET (target, (SIGNED_UNARY (-, (WREG_REF (source)))));
  NEXT_PC;
}

DEFINE_INST (negate_fr)
{
  DECODE_SVM1_INST_NEGATE_FR (target, source);
  FREG_SET (target, (- (FREG_REF (source))));
  NEXT_PC;
}

DEFINE_INST (increment_wr)
{
  DECODE_SVM1_INST_INCREMENT_WR (target, source);
  WREG_SET (target, ((WREG_REF (source)) + 1));
  NEXT_PC;
}

DEFINE_INST (increment_fr)
{
  DECODE_SVM1_INST_INCREMENT_FR (target, source);
  FREG_SET (target, ((FREG_REF (source)) + 1.0));
  NEXT_PC;
}

DEFINE_INST (decrement_wr)
{
  DECODE_SVM1_INST_DECREMENT_WR (target, source);
  WREG_SET (target, ((WREG_REF (source)) - 1));
  NEXT_PC;
}

DEFINE_INST (decrement_fr)
{
  DECODE_SVM1_INST_DECREMENT_FR (target, source);
  FREG_SET (target, ((FREG_REF (source)) - 1.0));
  NEXT_PC;
}

DEFINE_INST (abs_wr)
{
  DECODE_SVM1_INST_ABS_WR (target, source);
  WREG_SET (target, (SIGNED_UNARY (labs, (WREG_REF (source)))));
  NEXT_PC;
}

DEFINE_INST (abs_fr)
{
  DECODE_SVM1_INST_ABS_FR (target, source);
  FREG_SET (target, (fabs (FREG_REF (source))));
  NEXT_PC;
}

#define BIND_INST(index, defn) (inst_defns[SVM1_INST_##index]) = insn_##defn

static void
initialize_inst_defns (void)
{
  memset (inst_defns, 0, (sizeof (inst_defns)));
  BIND_INST (STORE_B_WR_ADDR, store_b_wr_addr);
  BIND_INST (STORE_W_WR_ADDR, store_w_wr_addr);
  BIND_INST (STORE_F_FR_ADDR, store_f_fr_addr);
  BIND_INST (LOAD_B_WR_ADDR, load_b_wr_addr);
  BIND_INST (LOAD_W_WR_ADDR, load_w_wr_addr);
  BIND_INST (LOAD_F_FR_ADDR, load_f_fr_addr);
  BIND_INST (LOAD_ADDRESS_ADDR, load_address_addr);
  BIND_INST (LOAD_IMMEDIATE_WR_S8, load_immediate_wr_s8);
  BIND_INST (LOAD_IMMEDIATE_WR_S16, load_immediate_wr_s16);
  BIND_INST (LOAD_IMMEDIATE_WR_S32, load_immediate_wr_s32);
  BIND_INST (LOAD_IMMEDIATE_WR_U8, load_immediate_wr_u8);
  BIND_INST (LOAD_IMMEDIATE_WR_U16, load_immediate_wr_u16);
  BIND_INST (LOAD_IMMEDIATE_WR_U32, load_immediate_wr_u32);
  BIND_INST (LOAD_IMMEDIATE_FR_FLT, load_immediate_fr_flt);
  BIND_INST (COPY_BLOCK_U8_W, copy_block_u8_w);
  BIND_INST (COPY_BLOCK_WR_W, copy_block_wr_w);
  BIND_INST (LOAD_NON_POINTER_TC_U8, load_non_pointer_tc_u8);
  BIND_INST (LOAD_NON_POINTER_WR_U8, load_non_pointer_wr_u8);
  BIND_INST (LOAD_NON_POINTER_TC_U16, load_non_pointer_tc_u16);
  BIND_INST (LOAD_NON_POINTER_WR_U16, load_non_pointer_wr_u16);
  BIND_INST (LOAD_NON_POINTER_TC_U32, load_non_pointer_tc_u32);
  BIND_INST (LOAD_NON_POINTER_WR_U32, load_non_pointer_wr_u32);
  BIND_INST (LOAD_NON_POINTER_TC_WR, load_non_pointer_tc_wr);
  BIND_INST (LOAD_NON_POINTER, load_non_pointer);
  BIND_INST (LOAD_POINTER_TC_WR, load_pointer_tc_wr);
  BIND_INST (LOAD_POINTER, load_pointer);
  BIND_INST (JUMP_PCR_S8, jump_pcr_s8);
  BIND_INST (JUMP_PCR_S16, jump_pcr_s16);
  BIND_INST (JUMP_PCR_S32, jump_pcr_s32);
  BIND_INST (JUMP_INDIR_WR, jump_indir_wr);
  BIND_INST (IJUMP_U8, ijump_u8);
  BIND_INST (IJUMP_U16, ijump_u16);
  BIND_INST (IJUMP_U32, ijump_u32);
  BIND_INST (ICALL_U8, icall_u8);
  BIND_INST (ICALL_U16, icall_u16);
  BIND_INST (ICALL_U32, icall_u32);
  BIND_INST (CJUMP_EQ_WR_WR_PCR_S8, cjump_eq_wr_wr_pcr_s8);
  BIND_INST (CJUMP_NEQ_WR_WR_PCR_S8, cjump_neq_wr_wr_pcr_s8);
  BIND_INST (CJUMP_LT_WR_WR_PCR_S8, cjump_lt_wr_wr_pcr_s8);
  BIND_INST (CJUMP_GE_WR_WR_PCR_S8, cjump_ge_wr_wr_pcr_s8);
  BIND_INST (CJUMP_GT_WR_WR_PCR_S8, cjump_gt_wr_wr_pcr_s8);
  BIND_INST (CJUMP_LE_WR_WR_PCR_S8, cjump_le_wr_wr_pcr_s8);
  BIND_INST (CJUMP_SLT_WR_WR_PCR_S8, cjump_slt_wr_wr_pcr_s8);
  BIND_INST (CJUMP_SGE_WR_WR_PCR_S8, cjump_sge_wr_wr_pcr_s8);
  BIND_INST (CJUMP_SGT_WR_WR_PCR_S8, cjump_sgt_wr_wr_pcr_s8);
  BIND_INST (CJUMP_SLE_WR_WR_PCR_S8, cjump_sle_wr_wr_pcr_s8);
  BIND_INST (CJUMP_EQ_WR_WR_PCR_S16, cjump_eq_wr_wr_pcr_s16);
  BIND_INST (CJUMP_NEQ_WR_WR_PCR_S16, cjump_neq_wr_wr_pcr_s16);
  BIND_INST (CJUMP_LT_WR_WR_PCR_S16, cjump_lt_wr_wr_pcr_s16);
  BIND_INST (CJUMP_GE_WR_WR_PCR_S16, cjump_ge_wr_wr_pcr_s16);
  BIND_INST (CJUMP_GT_WR_WR_PCR_S16, cjump_gt_wr_wr_pcr_s16);
  BIND_INST (CJUMP_LE_WR_WR_PCR_S16, cjump_le_wr_wr_pcr_s16);
  BIND_INST (CJUMP_SLT_WR_WR_PCR_S16, cjump_slt_wr_wr_pcr_s16);
  BIND_INST (CJUMP_SGE_WR_WR_PCR_S16, cjump_sge_wr_wr_pcr_s16);
  BIND_INST (CJUMP_SGT_WR_WR_PCR_S16, cjump_sgt_wr_wr_pcr_s16);
  BIND_INST (CJUMP_SLE_WR_WR_PCR_S16, cjump_sle_wr_wr_pcr_s16);
  BIND_INST (CJUMP_EQ_WR_WR_PCR_S32, cjump_eq_wr_wr_pcr_s32);
  BIND_INST (CJUMP_NEQ_WR_WR_PCR_S32, cjump_neq_wr_wr_pcr_s32);
  BIND_INST (CJUMP_LT_WR_WR_PCR_S32, cjump_lt_wr_wr_pcr_s32);
  BIND_INST (CJUMP_GE_WR_WR_PCR_S32, cjump_ge_wr_wr_pcr_s32);
  BIND_INST (CJUMP_GT_WR_WR_PCR_S32, cjump_gt_wr_wr_pcr_s32);
  BIND_INST (CJUMP_LE_WR_WR_PCR_S32, cjump_le_wr_wr_pcr_s32);
  BIND_INST (CJUMP_SLT_WR_WR_PCR_S32, cjump_slt_wr_wr_pcr_s32);
  BIND_INST (CJUMP_SGE_WR_WR_PCR_S32, cjump_sge_wr_wr_pcr_s32);
  BIND_INST (CJUMP_SGT_WR_WR_PCR_S32, cjump_sgt_wr_wr_pcr_s32);
  BIND_INST (CJUMP_SLE_WR_WR_PCR_S32, cjump_sle_wr_wr_pcr_s32);
  BIND_INST (CJUMP_EQ_WR_PCR_S8, cjump_eq_wr_pcr_s8);
  BIND_INST (CJUMP_NEQ_WR_PCR_S8, cjump_neq_wr_pcr_s8);
  BIND_INST (CJUMP_LT_WR_PCR_S8, cjump_lt_wr_pcr_s8);
  BIND_INST (CJUMP_GE_WR_PCR_S8, cjump_ge_wr_pcr_s8);
  BIND_INST (CJUMP_GT_WR_PCR_S8, cjump_gt_wr_pcr_s8);
  BIND_INST (CJUMP_LE_WR_PCR_S8, cjump_le_wr_pcr_s8);
  BIND_INST (CJUMP_SLT_WR_PCR_S8, cjump_slt_wr_pcr_s8);
  BIND_INST (CJUMP_SGE_WR_PCR_S8, cjump_sge_wr_pcr_s8);
  BIND_INST (CJUMP_SGT_WR_PCR_S8, cjump_sgt_wr_pcr_s8);
  BIND_INST (CJUMP_SLE_WR_PCR_S8, cjump_sle_wr_pcr_s8);
  BIND_INST (CJUMP_EQ_WR_PCR_S16, cjump_eq_wr_pcr_s16);
  BIND_INST (CJUMP_NEQ_WR_PCR_S16, cjump_neq_wr_pcr_s16);
  BIND_INST (CJUMP_LT_WR_PCR_S16, cjump_lt_wr_pcr_s16);
  BIND_INST (CJUMP_GE_WR_PCR_S16, cjump_ge_wr_pcr_s16);
  BIND_INST (CJUMP_GT_WR_PCR_S16, cjump_gt_wr_pcr_s16);
  BIND_INST (CJUMP_LE_WR_PCR_S16, cjump_le_wr_pcr_s16);
  BIND_INST (CJUMP_SLT_WR_PCR_S16, cjump_slt_wr_pcr_s16);
  BIND_INST (CJUMP_SGE_WR_PCR_S16, cjump_sge_wr_pcr_s16);
  BIND_INST (CJUMP_SGT_WR_PCR_S16, cjump_sgt_wr_pcr_s16);
  BIND_INST (CJUMP_SLE_WR_PCR_S16, cjump_sle_wr_pcr_s16);
  BIND_INST (CJUMP_EQ_WR_PCR_S32, cjump_eq_wr_pcr_s32);
  BIND_INST (CJUMP_NEQ_WR_PCR_S32, cjump_neq_wr_pcr_s32);
  BIND_INST (CJUMP_LT_WR_PCR_S32, cjump_lt_wr_pcr_s32);
  BIND_INST (CJUMP_GE_WR_PCR_S32, cjump_ge_wr_pcr_s32);
  BIND_INST (CJUMP_GT_WR_PCR_S32, cjump_gt_wr_pcr_s32);
  BIND_INST (CJUMP_LE_WR_PCR_S32, cjump_le_wr_pcr_s32);
  BIND_INST (CJUMP_SLT_WR_PCR_S32, cjump_slt_wr_pcr_s32);
  BIND_INST (CJUMP_SGE_WR_PCR_S32, cjump_sge_wr_pcr_s32);
  BIND_INST (CJUMP_SGT_WR_PCR_S32, cjump_sgt_wr_pcr_s32);
  BIND_INST (CJUMP_SLE_WR_PCR_S32, cjump_sle_wr_pcr_s32);
  BIND_INST (CJUMP_EQ_FR_FR_PCR_S8, cjump_eq_fr_fr_pcr_s8);
  BIND_INST (CJUMP_NEQ_FR_FR_PCR_S8, cjump_neq_fr_fr_pcr_s8);
  BIND_INST (CJUMP_LT_FR_FR_PCR_S8, cjump_lt_fr_fr_pcr_s8);
  BIND_INST (CJUMP_GT_FR_FR_PCR_S8, cjump_gt_fr_fr_pcr_s8);
  BIND_INST (CJUMP_LE_FR_FR_PCR_S8, cjump_le_fr_fr_pcr_s8);
  BIND_INST (CJUMP_GE_FR_FR_PCR_S8, cjump_ge_fr_fr_pcr_s8);
  BIND_INST (CJUMP_CMP_FR_FR_PCR_S8, cjump_cmp_fr_fr_pcr_s8);
  BIND_INST (CJUMP_NCMP_FR_FR_PCR_S8, cjump_ncmp_fr_fr_pcr_s8);
  BIND_INST (CJUMP_EQ_FR_FR_PCR_S16, cjump_eq_fr_fr_pcr_s16);
  BIND_INST (CJUMP_NEQ_FR_FR_PCR_S16, cjump_neq_fr_fr_pcr_s16);
  BIND_INST (CJUMP_LT_FR_FR_PCR_S16, cjump_lt_fr_fr_pcr_s16);
  BIND_INST (CJUMP_GT_FR_FR_PCR_S16, cjump_gt_fr_fr_pcr_s16);
  BIND_INST (CJUMP_LE_FR_FR_PCR_S16, cjump_le_fr_fr_pcr_s16);
  BIND_INST (CJUMP_GE_FR_FR_PCR_S16, cjump_ge_fr_fr_pcr_s16);
  BIND_INST (CJUMP_CMP_FR_FR_PCR_S16, cjump_cmp_fr_fr_pcr_s16);
  BIND_INST (CJUMP_NCMP_FR_FR_PCR_S16, cjump_ncmp_fr_fr_pcr_s16);
  BIND_INST (CJUMP_EQ_FR_FR_PCR_S32, cjump_eq_fr_fr_pcr_s32);
  BIND_INST (CJUMP_NEQ_FR_FR_PCR_S32, cjump_neq_fr_fr_pcr_s32);
  BIND_INST (CJUMP_LT_FR_FR_PCR_S32, cjump_lt_fr_fr_pcr_s32);
  BIND_INST (CJUMP_GT_FR_FR_PCR_S32, cjump_gt_fr_fr_pcr_s32);
  BIND_INST (CJUMP_LE_FR_FR_PCR_S32, cjump_le_fr_fr_pcr_s32);
  BIND_INST (CJUMP_GE_FR_FR_PCR_S32, cjump_ge_fr_fr_pcr_s32);
  BIND_INST (CJUMP_CMP_FR_FR_PCR_S32, cjump_cmp_fr_fr_pcr_s32);
  BIND_INST (CJUMP_NCMP_FR_FR_PCR_S32, cjump_ncmp_fr_fr_pcr_s32);
  BIND_INST (CJUMP_EQ_FR_PCR_S8, cjump_eq_fr_pcr_s8);
  BIND_INST (CJUMP_NEQ_FR_PCR_S8, cjump_neq_fr_pcr_s8);
  BIND_INST (CJUMP_LT_FR_PCR_S8, cjump_lt_fr_pcr_s8);
  BIND_INST (CJUMP_GT_FR_PCR_S8, cjump_gt_fr_pcr_s8);
  BIND_INST (CJUMP_LE_FR_PCR_S8, cjump_le_fr_pcr_s8);
  BIND_INST (CJUMP_GE_FR_PCR_S8, cjump_ge_fr_pcr_s8);
  BIND_INST (CJUMP_CMP_FR_PCR_S8, cjump_cmp_fr_pcr_s8);
  BIND_INST (CJUMP_NCMP_FR_PCR_S8, cjump_ncmp_fr_pcr_s8);
  BIND_INST (CJUMP_EQ_FR_PCR_S16, cjump_eq_fr_pcr_s16);
  BIND_INST (CJUMP_NEQ_FR_PCR_S16, cjump_neq_fr_pcr_s16);
  BIND_INST (CJUMP_LT_FR_PCR_S16, cjump_lt_fr_pcr_s16);
  BIND_INST (CJUMP_GT_FR_PCR_S16, cjump_gt_fr_pcr_s16);
  BIND_INST (CJUMP_LE_FR_PCR_S16, cjump_le_fr_pcr_s16);
  BIND_INST (CJUMP_GE_FR_PCR_S16, cjump_ge_fr_pcr_s16);
  BIND_INST (CJUMP_CMP_FR_PCR_S16, cjump_cmp_fr_pcr_s16);
  BIND_INST (CJUMP_NCMP_FR_PCR_S16, cjump_ncmp_fr_pcr_s16);
  BIND_INST (CJUMP_EQ_FR_PCR_S32, cjump_eq_fr_pcr_s32);
  BIND_INST (CJUMP_NEQ_FR_PCR_S32, cjump_neq_fr_pcr_s32);
  BIND_INST (CJUMP_LT_FR_PCR_S32, cjump_lt_fr_pcr_s32);
  BIND_INST (CJUMP_GT_FR_PCR_S32, cjump_gt_fr_pcr_s32);
  BIND_INST (CJUMP_LE_FR_PCR_S32, cjump_le_fr_pcr_s32);
  BIND_INST (CJUMP_GE_FR_PCR_S32, cjump_ge_fr_pcr_s32);
  BIND_INST (CJUMP_CMP_FR_PCR_S32, cjump_cmp_fr_pcr_s32);
  BIND_INST (CJUMP_NCMP_FR_PCR_S32, cjump_ncmp_fr_pcr_s32);
  BIND_INST (CJUMP_FIX_WR_PCR_S8, cjump_fix_wr_pcr_s8);
  BIND_INST (CJUMP_FIX_WR_PCR_S16, cjump_fix_wr_pcr_s16);
  BIND_INST (CJUMP_FIX_WR_PCR_S32, cjump_fix_wr_pcr_s32);
  BIND_INST (CJUMP_NFIX_WR_PCR_S8, cjump_nfix_wr_pcr_s8);
  BIND_INST (CJUMP_NFIX_WR_PCR_S16, cjump_nfix_wr_pcr_s16);
  BIND_INST (CJUMP_NFIX_WR_PCR_S32, cjump_nfix_wr_pcr_s32);
  BIND_INST (CJUMP_IFIX_WR_PCR_S8, cjump_ifix_wr_pcr_s8);
  BIND_INST (CJUMP_IFIX_WR_PCR_S16, cjump_ifix_wr_pcr_s16);
  BIND_INST (CJUMP_IFIX_WR_PCR_S32, cjump_ifix_wr_pcr_s32);
  BIND_INST (CJUMP_NIFIX_WR_PCR_S8, cjump_nifix_wr_pcr_s8);
  BIND_INST (CJUMP_NIFIX_WR_PCR_S16, cjump_nifix_wr_pcr_s16);
  BIND_INST (CJUMP_NIFIX_WR_PCR_S32, cjump_nifix_wr_pcr_s32);
  BIND_INST (TRAP_TRAP_0, trap_0);
  BIND_INST (TRAP_TRAP_1_WR, trap_1);
  BIND_INST (TRAP_TRAP_2_WR, trap_2);
  BIND_INST (TRAP_TRAP_3_WR, trap_3);
  BIND_INST (INTERRUPT_TEST_PROCEDURE, interrupt_test_procedure);
  BIND_INST (INTERRUPT_TEST_CLOSURE, interrupt_test_closure);
  BIND_INST (INTERRUPT_TEST_DYNAMIC_LINK, interrupt_test_dynamic_link);
  BIND_INST (INTERRUPT_TEST_IC_PROCEDURE, interrupt_test_ic_procedure);
  BIND_INST (INTERRUPT_TEST_CONTINUATION, interrupt_test_continuation);
  BIND_INST (FLONUM_HEADER_U8, flonum_header_u8);
  BIND_INST (FLONUM_HEADER_U16, flonum_header_u16);
  BIND_INST (FLONUM_HEADER_U32, flonum_header_u32);
  BIND_INST (FLONUM_HEADER, flonum_header);
  BIND_INST (COPY_WR, copy_wr);
  BIND_INST (COPY_FR, copy_fr);
  BIND_INST (NEGATE_WR, negate_wr);
  BIND_INST (NEGATE_FR, negate_fr);
  BIND_INST (INCREMENT_WR, increment_wr);
  BIND_INST (INCREMENT_FR, increment_fr);
  BIND_INST (DECREMENT_WR, decrement_wr);
  BIND_INST (DECREMENT_FR, decrement_fr);
  BIND_INST (ABS_WR, abs_wr);
  BIND_INST (ABS_FR, abs_fr);
}

/* Address decoders */

static void
decode_address (address_t * address)
{
  (* (address_decoders[NEXT_BYTE])) (address);
}

static word_t
offset_address_value (address_t * address)
{
  return ((WREG_REF (address->r1)) + (address->n1));
}

DEFINE_ADDRESS_DECODER (indir)
{
  DECODE_SVM1_ADDR_INDIR (base);
  (address->r1) = base;
  (address->n1) = 0;
  (address->value) = offset_address_value;
}

#define MAKE_OFFSET_ADDRESS(base, offset, scale)			\
{									\
  (address->r1) = (base);						\
  (address->n1) = ((offset) * (scale));					\
  (address->value) = offset_address_value;				\
}

DEFINE_ADDRESS_DECODER (offset_b)
{
  DECODE_SVM1_ADDR_OFFSET_B (base, offset);
  MAKE_OFFSET_ADDRESS (base, offset, SBYTE);
}

DEFINE_ADDRESS_DECODER (offset_w)
{
  DECODE_SVM1_ADDR_OFFSET_W (base, offset);
  MAKE_OFFSET_ADDRESS (base, offset, SWORD);
}

DEFINE_ADDRESS_DECODER (offset_f)
{
  DECODE_SVM1_ADDR_OFFSET_F (base, offset);
  MAKE_OFFSET_ADDRESS (base, offset, SFLOAT);
}

static word_t
indexed_address_value (address_t * address)
{
  return
    ((WREG_REF (address->r1))
     + (address->n1)
     + ((WREG_REF (address->r2)) * (address->n2)));
}

#define MAKE_INDEXED_ADDRESS(base, offset, oscale, index, iscale)	\
{									\
  (address->r1) = (base);						\
  (address->n1) = ((offset) * (oscale));				\
  (address->r2) = (index);						\
  (address->n2) = (iscale);						\
  (address->value) = indexed_address_value;				\
}

DEFINE_ADDRESS_DECODER (index_b_b)
{
  DECODE_SVM1_ADDR_INDEX_B_B (base, offset, index);
  MAKE_INDEXED_ADDRESS (base, offset, SBYTE, index, SBYTE);
}

DEFINE_ADDRESS_DECODER (index_b_w)
{
  DECODE_SVM1_ADDR_INDEX_B_W (base, offset, index);
  MAKE_INDEXED_ADDRESS (base, offset, SBYTE, index, SWORD);
}

DEFINE_ADDRESS_DECODER (index_b_f)
{
  DECODE_SVM1_ADDR_INDEX_B_F (base, offset, index);
  MAKE_INDEXED_ADDRESS (base, offset, SBYTE, index, SFLOAT);
}

DEFINE_ADDRESS_DECODER (index_w_b)
{
  DECODE_SVM1_ADDR_INDEX_W_B (base, offset, index);
  MAKE_INDEXED_ADDRESS (base, offset, SWORD, index, SBYTE);
}

DEFINE_ADDRESS_DECODER (index_w_w)
{
  DECODE_SVM1_ADDR_INDEX_W_W (base, offset, index);
  MAKE_INDEXED_ADDRESS (base, offset, SWORD, index, SWORD);
}

DEFINE_ADDRESS_DECODER (index_w_f)
{
  DECODE_SVM1_ADDR_INDEX_W_F (base, offset, index);
  MAKE_INDEXED_ADDRESS (base, offset, SWORD, index, SFLOAT);
}

DEFINE_ADDRESS_DECODER (index_f_b)
{
  DECODE_SVM1_ADDR_INDEX_F_B (base, offset, index);
  MAKE_INDEXED_ADDRESS (base, offset, SFLOAT, index, SBYTE);
}

DEFINE_ADDRESS_DECODER (index_f_w)
{
  DECODE_SVM1_ADDR_INDEX_F_W (base, offset, index);
  MAKE_INDEXED_ADDRESS (base, offset, SFLOAT, index, SWORD);
}

DEFINE_ADDRESS_DECODER (index_f_f)
{
  DECODE_SVM1_ADDR_INDEX_F_F (base, offset, index);
  MAKE_INDEXED_ADDRESS (base, offset, SFLOAT, index, SFLOAT);
}

static word_t
preinc_address_value (address_t * address)
{
  WREG_SET ((address->r1), ((WREG_REF (address->r1)) + (address->n2)));
  return (WREG_REF (address->r1));
}

#define MAKE_PREINC_ADDRESS(base, scale)				\
{									\
  (address->r1) = (base);						\
  (address->n2) = (scale);						\
  (address->value) = preinc_address_value;				\
}

DEFINE_ADDRESS_DECODER (predec_b)
{
  DECODE_SVM1_ADDR_PREDEC_B (base);
  MAKE_PREINC_ADDRESS (base, (- SBYTE));
}

DEFINE_ADDRESS_DECODER (predec_w)
{
  DECODE_SVM1_ADDR_PREDEC_W (base);
  MAKE_PREINC_ADDRESS (base, (- SWORD));
}

DEFINE_ADDRESS_DECODER (predec_f)
{
  DECODE_SVM1_ADDR_PREDEC_F (base);
  MAKE_PREINC_ADDRESS (base, (- SFLOAT));
}

DEFINE_ADDRESS_DECODER (preinc_b)
{
  DECODE_SVM1_ADDR_PREINC_B (base);
  MAKE_PREINC_ADDRESS (base, SBYTE);
}

DEFINE_ADDRESS_DECODER (preinc_w)
{
  DECODE_SVM1_ADDR_PREINC_W (base);
  MAKE_PREINC_ADDRESS (base, SWORD);
}

DEFINE_ADDRESS_DECODER (preinc_f)
{
  DECODE_SVM1_ADDR_PREINC_F (base);
  MAKE_PREINC_ADDRESS (base, SFLOAT);
}

static word_t
postinc_address_value (address_t * address)
{
  word_t value = (WREG_REF (address->r1));
  WREG_SET ((address->r1), ((WREG_REF (address->r1)) + (address->n2)));
  return (value);
}

#define MAKE_POSTINC_ADDRESS(base, scale)				\
{									\
  (address->r1) = (base);						\
  (address->n2) = (scale);						\
  (address->value) = postinc_address_value;				\
}

DEFINE_ADDRESS_DECODER (postdec_b)
{
  DECODE_SVM1_ADDR_POSTDEC_B (base);
  MAKE_POSTINC_ADDRESS (base, (- SBYTE));
}

DEFINE_ADDRESS_DECODER (postdec_w)
{
  DECODE_SVM1_ADDR_POSTDEC_W (base);
  MAKE_POSTINC_ADDRESS (base, (- SWORD));
}

DEFINE_ADDRESS_DECODER (postdec_f)
{
  DECODE_SVM1_ADDR_POSTDEC_F (base);
  MAKE_POSTINC_ADDRESS (base, (- SFLOAT));
}

DEFINE_ADDRESS_DECODER (postinc_b)
{
  DECODE_SVM1_ADDR_POSTINC_B (base);
  MAKE_POSTINC_ADDRESS (base, SBYTE);
}

DEFINE_ADDRESS_DECODER (postinc_w)
{
  DECODE_SVM1_ADDR_POSTINC_W (base);
  MAKE_POSTINC_ADDRESS (base, SWORD);
}

DEFINE_ADDRESS_DECODER (postinc_f)
{
  DECODE_SVM1_ADDR_POSTINC_F (base);
  MAKE_POSTINC_ADDRESS (base, SFLOAT);
}

static word_t
pcr_value (address_t * address)
{
  return (((word_t) PC) + (address->n2));
}

#define MAKE_PCR_ADDRESS(offset)					\
{									\
  (address->n2) = (offset);						\
  (address->value) = pcr_value;						\
}

DEFINE_ADDRESS_DECODER (pcr_s8)
{
  DECODE_SVM1_ADDR_PCR_S8 (offset);
  MAKE_PCR_ADDRESS (offset);
}

DEFINE_ADDRESS_DECODER (pcr_s16)
{
  DECODE_SVM1_ADDR_PCR_S16 (offset);
  MAKE_PCR_ADDRESS (offset);
}

DEFINE_ADDRESS_DECODER (pcr_s32)
{
  DECODE_SVM1_ADDR_PCR_S32 (offset);
  MAKE_PCR_ADDRESS (offset);
}

static void
initialize_address_decoders (void)
{
  memset (address_decoders, 0, (sizeof (address_decoders)));
  (address_decoders[SVM1_ADDR_INDIR]) = decode_addr_indir;
  (address_decoders[SVM1_ADDR_OFFSET_B]) = decode_addr_offset_b;
  (address_decoders[SVM1_ADDR_OFFSET_W]) = decode_addr_offset_w;
  (address_decoders[SVM1_ADDR_OFFSET_F]) = decode_addr_offset_f;
  (address_decoders[SVM1_ADDR_INDEX_B_B]) = decode_addr_index_b_b;
  (address_decoders[SVM1_ADDR_INDEX_B_W]) = decode_addr_index_b_w;
  (address_decoders[SVM1_ADDR_INDEX_B_F]) = decode_addr_index_b_f;
  (address_decoders[SVM1_ADDR_INDEX_W_B]) = decode_addr_index_w_b;
  (address_decoders[SVM1_ADDR_INDEX_W_W]) = decode_addr_index_w_w;
  (address_decoders[SVM1_ADDR_INDEX_W_F]) = decode_addr_index_w_f;
  (address_decoders[SVM1_ADDR_INDEX_F_B]) = decode_addr_index_f_b;
  (address_decoders[SVM1_ADDR_INDEX_F_W]) = decode_addr_index_f_w;
  (address_decoders[SVM1_ADDR_INDEX_F_F]) = decode_addr_index_f_f;
  (address_decoders[SVM1_ADDR_PREDEC_B]) = decode_addr_predec_b;
  (address_decoders[SVM1_ADDR_PREDEC_W]) = decode_addr_predec_w;
  (address_decoders[SVM1_ADDR_PREDEC_F]) = decode_addr_predec_f;
  (address_decoders[SVM1_ADDR_PREINC_B]) = decode_addr_preinc_b;
  (address_decoders[SVM1_ADDR_PREINC_W]) = decode_addr_preinc_w;
  (address_decoders[SVM1_ADDR_PREINC_F]) = decode_addr_preinc_f;
  (address_decoders[SVM1_ADDR_POSTDEC_B]) = decode_addr_postdec_b;
  (address_decoders[SVM1_ADDR_POSTDEC_W]) = decode_addr_postdec_w;
  (address_decoders[SVM1_ADDR_POSTDEC_F]) = decode_addr_postdec_f;
  (address_decoders[SVM1_ADDR_POSTINC_B]) = decode_addr_postinc_b;
  (address_decoders[SVM1_ADDR_POSTINC_W]) = decode_addr_postinc_w;
  (address_decoders[SVM1_ADDR_POSTINC_F]) = decode_addr_postinc_f;
  (address_decoders[SVM1_ADDR_PCR_S8]) = decode_addr_pcr_s8;
  (address_decoders[SVM1_ADDR_PCR_S16]) = decode_addr_pcr_s16;
  (address_decoders[SVM1_ADDR_PCR_S32]) = decode_addr_pcr_s32;
}

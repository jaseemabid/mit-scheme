#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/machines/bobcat/lapgen.scm,v 1.179.1.1 1987/06/25 10:24:51 jinx Exp $

Copyright (c) 1987 Massachusetts Institute of Technology

This material was developed by the Scheme project at the Massachusetts
Institute of Technology, Department of Electrical Engineering and
Computer Science.  Permission to copy this software, to redistribute
it, and to use it for any purpose is granted, subject to the following
restrictions and understandings.

1. Any copy made of this software must include this copyright notice
in full.

2. Users of this software agree to make their best efforts (a) to
return to the MIT Scheme project any improvements or extensions that
they make, so that these may be included in future releases; and (b)
to inform MIT of noteworthy uses of this software.

3. All materials developed as a consequence of the use of this
software shall duly acknowledge such use, in accordance with the usual
standards of acknowledging credit in academic research.

4. MIT has made no warrantee or representation that the operation of
this software will be error-free, and MIT is under no obligation to
provide any services, by way of maintenance, update, or otherwise.

5. In conjunction with products arising from the use of this material,
there shall be no use of the name of the Massachusetts Institute of
Technology nor of any adaptation thereof in any advertising,
promotional, or sales literature without prior written consent from
MIT in each case. |#

;;;; RTL Rules for 68020

(declare (usual-integrations))

;;;; Basic machine instructions

(define (register->register-transfer source target)
  (LAP ,(machine->machine-register source target)))

(define (home->register-transfer source target)
  (LAP ,(pseudo->machine-register source target)))

(define (register->home-transfer source target)
  (LAP ,(machine->pseudo-register source target)))

(define-integrable (pseudo->machine-register source target)
  (memory->machine-register (pseudo-register-home source) target))

(define-integrable (machine->pseudo-register source target)
  (machine-register->memory source (pseudo-register-home target)))

(define-integrable (pseudo-register-home register)
  (offset-reference regnum:regs-pointer
		    (+ #x000A (register-renumber register))))

(define-integrable (machine->machine-register source target)
  (INST (MOVE L
	      ,(register-reference source)
	      ,(register-reference target))))

(define-integrable (machine-register->memory source target)
  (INST (MOVE L
	      ,(register-reference source)
	      ,target)))

(define-integrable (memory->machine-register source target)
  (INST (MOVE L
	      ,source
	      ,(register-reference target))))

(define (offset-reference register offset)
  (if (zero? offset)
      (if (< register 8)
	  (INST-EA (@D ,register))
	  (INST-EA (@A ,(- register 8))))
      (if (< register 8)
	  (INST-EA (@DO ,register ,(* 4 offset)))
	  (INST-EA (@AO ,(- register 8) ,(* 4 offset))))))

(define (load-dnw n d)
  (cond ((zero? n)
	 (INST (CLR W (D ,d))))
	((<= -128 n 127)
	 (INST (MOVEQ (& ,n) (D ,d))))
	(else
	 (INST (MOVE W (& ,n) (D ,d))))))

(define (test-dnw n d)
  (if (zero? n)
      (INST (TST W (D ,d)))
      (INST (CMP W (& ,n) (D ,d)))))

(define (increment-anl an n)
  (case n
    ((0) (LAP))
    ((1 2) (LAP (ADDQ L (& ,(* 4 n)) (A ,an))))
    ((-1 -2) (LAP (SUBQ L (& ,(* -4 n)) (A ,an))))
    (else (LAP (LEA (@AO ,an ,(* 4 n)) (A ,an))))))

(define (load-constant constant target)
  (if (non-pointer-object? constant)
      (load-non-pointer (primitive-type constant)
			(primitive-datum constant)
			target)
      (INST (MOVE L
		  (@PCR ,(constant->label constant))
		  ,target))))

(define (load-non-pointer type datum target)
  (cond ((not (zero? type))
	 (INST (MOVE L
		     (& ,(make-non-pointer-literal type datum))
		     ,target)))
	((and (zero? datum)
	      (memq (lap:ea-keyword target) '(D @D @A @A+ @-A @AO @DO @AOX W L)))
	 (INST (CLR L ,target)))
	((and (<= -128 datum 127) (eq? (lap:ea-keyword target) 'D))
	 (INST (MOVEQ (& ,datum) ,target)))
	(else (INST (MOVE L (& ,datum) ,target)))))

(define (test-byte n effective-address)
  (if (and (zero? n) (TSTable-effective-address? effective-address))
      (INST (TST B ,effective-address))
      (INST (CMP B (& ,n) ,effective-address))))

(define (test-non-pointer type datum effective-address)
  (if (and (zero? type) (zero? datum)
	   (TSTable-effective-address? effective-address))
      (INST (TST L ,effective-address))
      (INST (CMP L
		 (& ,(make-non-pointer-literal type datum))
		 ,effective-address))))

(define make-non-pointer-literal
  (let ((type-scale-factor (expt 2 24)))
    (lambda (type datum)
      (+ (* (if (negative? datum) (1+ type) type)
	    type-scale-factor)
	 datum))))

(define (set-standard-branches! cc)
  (set-current-branches!
   (lambda (label)
     (LAP (B ,cc L (@PCR ,label))))
   (lambda (label)
     (LAP (B ,(invert-cc cc) L (@PCR ,label))))))

(define (invert-cc cc)
  (cdr (or (assq cc
		 '((T . F) (F . T)
		   (HI . LS) (LS . HI)
		   (HS . LO) (LO . HS)
		   (CC . CS) (CS . CC)
		   (NE . EQ) (EQ . NE)
		   (VC . VS) (VS . VC)
		   (PL . MI) (MI . PL)
		   (GE . LT) (LT . GE)
		   (GT . LE) (LE . GT)
		   ))
	   (error "INVERT-CC: Not a known CC" cc))))

(define (expression->machine-register! expression register)
  (let ((target (register-reference register)))
    (let ((result
	   (case (car expression)
	     ((REGISTER)
	      (LAP (MOVE L ,(coerce->any (cadr expression)) ,target)))
	     ((OFFSET)
	      (LAP
	       (MOVE L
		     ,(indirect-reference! (cadadr expression)
					   (caddr expression))
		     ,target)))
	     ((CONSTANT)
	      (LAP ,(load-constant (cadr expression) target)))
	     ((UNASSIGNED)
	      (LAP ,(load-non-pointer type-code:unassigned 0 target)))
	     (else
	      (error "Unknown expression type" (car expression))))))
      (delete-machine-register! register)
      result)))

(define-integrable (TSTable-effective-address? effective-address)
  (memq (lap:ea-keyword effective-address) '(D @D @A @A+ @-A @DO @AO @AOX W L)))

(define-integrable (register-effective-address? effective-address)
  (memq (lap:ea-keyword effective-address) '(A D)))

(define (indirect-reference! register offset)
  (if (= register regnum:frame-pointer)
      (offset-reference regnum:stack-pointer (+ offset (frame-pointer-offset)))
      (offset-reference
       (if (machine-register? register)
	   register
	   (or (register-alias register false)
	       ;; This means that someone has written an address out
	       ;; to memory, something that should happen only when the
	       ;; register block spills something.
	       (begin (warn "Needed to load indirect register!" register)
		      ;; Should specify preference for ADDRESS but will
		      ;; accept DATA if no ADDRESS registers available.
		      (allocate-alias-register! register 'ADDRESS))))
       offset)))

(define (coerce->any register)
  (if (machine-register? register)
      (register-reference register)
      (let ((alias (register-alias register false)))
	(if alias
	    (register-reference alias)
	    (pseudo-register-home register)))))

(define (coerce->machine-register register)
  (if (machine-register? register)
      (register-reference register)
      (reference-alias-register! register false)))

(define (code-object-label-initialize code-object)
  false)

(define (generate-n-times n limit instruction with-counter)
  (if (<= n limit)
      (let loop ((n n))
	(if (zero? n)
	    (LAP)
	    (LAP ,instruction
		 ,@(loop (-1+ n)))))
      (let ((loop (generate-label 'LOOP)))
	(with-counter
	 (lambda (counter)
	   (LAP ,(load-dnw (-1+ n) counter)
		(LABEL ,loop)
		,instruction
		(DB F (D ,counter) (@PCR ,loop))))))))

(define-integrable (data-register? register)
  (< register 8))

(define (address-register? register)
  (and (< register 16)
       (>= register 8)))

(define-integrable (lap:ea-keyword expression)
  (car expression))

(define-export (lap:make-label-statement label)
  (INST (LABEL ,label)))

(define-export (lap:make-unconditional-branch label)
  (INST (BRA L (@PCR ,label))))

(define-export (lap:make-entry-point label block-start-label)
  (LAP (ENTRY-POINT ,label)
       (DC W (- ,label ,block-start-label))
       (LABEL ,label)))

;;;; Registers/Entries

(let-syntax ((define-entries
	       (macro (start . names)
		 (define (loop names index)
		   (if (null? names)
		       '()
		       (cons `(DEFINE-INTEGRABLE
				,(symbol-append 'ENTRY:COMPILER-
						(car names))
				(INST-EA (@AO 6 ,index)))
			     (loop (cdr names) (+ index 6)))))
		 `(BEGIN ,@(loop names start)))))
  (define-entries #x00F0 apply error wrong-number-of-arguments
    interrupt-procedure interrupt-continuation lookup-apply lookup access
    unassigned? unbound? set! define primitive-apply enclose setup-lexpr
    return-to-interpreter safe-lookup cache-variable reference-trap
    assignment-trap)
  (define-entries #x0228 uuo-link uuo-link-trap cache-reference-apply
    safe-reference-trap unassigned?-trap cache-variable-multiple
    uuo-link-multiple))

(define-integrable reg:compiled-memtop (INST-EA (@A 6)))
(define-integrable reg:environment (INST-EA (@AO 6 #x000C)))
(define-integrable reg:temp (INST-EA (@AO 6 #x0010)))
(define-integrable reg:enclose-result (INST-EA (@AO 6 #x0014)))

(define-integrable popper:apply-closure (INST-EA (@AO 6 #x0168)))
(define-integrable popper:apply-stack (INST-EA (@AO 6 #x01A8)))
(define-integrable popper:value (INST-EA (@AO 6 #x01E8)))

;;;; Transfers to Registers

;;; All assignments to pseudo registers are required to delete the
;;; dead registers BEFORE performing the assignment.  This is because
;;; the register being assigned may be PSEUDO-REGISTER=? to one of the
;;; dead registers, and thus would be flushed if the deletions
;;; happened after the assignment.

(define-rule statement
  (ASSIGN (REGISTER 12) (REGISTER 15))
  (enable-frame-pointer-offset! 0)
  (LAP))

(define-rule statement
  (ASSIGN (REGISTER 15) (OFFSET-ADDRESS (REGISTER 15) (? n)))
  (decrement-frame-pointer-offset! n (increment-anl 7 n)))

(define-rule statement
  (ASSIGN (REGISTER (? target)) (OFFSET-ADDRESS (REGISTER 15) (? n)))
  (QUALIFIER (pseudo-register? target))
  (LAP
   (LEA (@AO 7 ,(* 4 n))
	,(reference-assignment-alias! target 'ADDRESS))))

(define-rule statement
  (ASSIGN (REGISTER 15) (REGISTER (? source)))
  (disable-frame-pointer-offset!
   (LAP (MOVE L ,(coerce->any source) (A 7)))))

(define-rule statement
  (ASSIGN (REGISTER (? target)) (CONSTANT (? source)))
  (QUALIFIER (pseudo-register? target))
  (LAP ,(load-constant source (coerce->any target))))

(define-rule statement
  (ASSIGN (REGISTER (? target)) (VARIABLE-CACHE (? name)))
  (QUALIFIER (pseudo-register? target))
  (LAP (MOVE L
	     (@PCR ,(free-reference-label name))
	     ,(reference-assignment-alias! target 'DATA))))

(define-rule statement
  (ASSIGN (REGISTER (? target)) (REGISTER (? source)))
  (QUALIFIER (pseudo-register? target))
  (move-to-alias-register! source 'DATA target)
  (LAP))

(define-rule statement
  (ASSIGN (REGISTER (? target)) (OBJECT->ADDRESS (REGISTER (? source))))
  (QUALIFIER (pseudo-register? target))
  (let ((target (move-to-alias-register! source 'DATA target)))
    (LAP (AND L ,mask-reference ,target))))

(define-rule statement
  (ASSIGN (REGISTER (? target)) (OBJECT->TYPE (REGISTER (? source))))
  (QUALIFIER (pseudo-register? target))
  (let ((target (move-to-alias-register! source 'DATA target)))
    (LAP (RO L L (& 8) ,target))))

(define-rule statement
  (ASSIGN (REGISTER (? target)) (OFFSET (REGISTER (? address)) (? offset)))
  (QUALIFIER (pseudo-register? target))
  (let ((source (indirect-reference! address offset)))
    (delete-dead-registers!)
    ;; The fact that the target register here is a data register is a
    ;; heuristic that works reasonably well since if the value is a
    ;; pointer, we will probably want to dereference it, which
    ;; requires that we first mask it.
    (LAP (MOVE L
	       ,source
	       ,(register-reference (allocate-alias-register! target 'DATA))))))

(define-rule statement
  (ASSIGN (REGISTER (? target)) (POST-INCREMENT (REGISTER 15) 1))
  (QUALIFIER (pseudo-register? target))
  (record-pop!)
  (delete-dead-registers!)
  (LAP (MOVE L
	     (@A+ 7)
	     ,(register-reference (allocate-alias-register! target 'DATA)))))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (CONS-POINTER (CONSTANT (? type)) (REGISTER (? datum))))
  (QUALIFIER (pseudo-register? target))
  (let ((target* (coerce->any target))
	(datum (coerce->any datum)))
    (delete-dead-registers!)
    (if (register-effective-address? target*)
	(LAP (MOVE L ,datum ,reg:temp)
	     (MOVE B (& ,type) ,reg:temp)
	     (MOVE L ,reg:temp ,target*))
	(LAP (MOVE L ,datum ,target*)
	     (MOVE B (& ,type) ,target*)))))

;;;; Transfers to Memory

(define-rule statement
  (ASSIGN (OFFSET (REGISTER (? a)) (? n))
	  (CONSTANT (? object)))
  (LAP ,(load-constant object (indirect-reference! a n))))

(define-rule statement
  (ASSIGN (OFFSET (REGISTER (? a)) (? n))
	  (REGISTER (? r)))
  (LAP (MOVE L
	     ,(coerce->any r)
	     ,(indirect-reference! a n))))

(define-rule statement
  (ASSIGN (OFFSET (REGISTER (? a)) (? n))
	  (POST-INCREMENT (REGISTER 15) 1))
  (record-pop!)
  (LAP (MOVE L
	     (@A+ 7)
	     ,(indirect-reference! a n))))

(define-rule statement
  (ASSIGN (OFFSET (REGISTER (? a)) (? n))
	  (CONS-POINTER (CONSTANT (? type)) (REGISTER (? r))))
  (let ((target (indirect-reference! a n)))
    (LAP (MOVE L ,(coerce->any r) (EVALUATE target))
	 (MOVE B (& ,type) ,target))))

(define-rule statement
  (ASSIGN (OFFSET (REGISTER (? a0)) (? n0))
	  (OFFSET (REGISTER (? a1)) (? n1)))
  (let ((source (indirect-reference! a1 n1)))
    (LAP (MOVE L
	       ,source
	       ,(indirect-reference! a0 n0)))))

;;;; Consing

(define-rule statement
  (ASSIGN (POST-INCREMENT (REGISTER 13) 1) (CONSTANT (? object)))
  (LAP ,(load-constant object (INST-EA (@A+ 5)))))

(define-rule statement
  (ASSIGN (POST-INCREMENT (REGISTER 13) 1) (UNASSIGNED))
  (LAP ,(load-non-pointer type-code:unassigned 0 (INST-EA (@A+ 5)))))

(define-rule statement
  (ASSIGN (POST-INCREMENT (REGISTER 13) 1) (REGISTER (? r)))
  (LAP (MOVE L ,(coerce->any r) (@A+ 5))))

(define-rule statement
  (ASSIGN (POST-INCREMENT (REGISTER 13) 1) (OFFSET (REGISTER (? r)) (? n)))
  (LAP (MOVE L ,(indirect-reference! r n) (@A+ 5))))

(define-rule statement
  (ASSIGN (POST-INCREMENT (REGISTER 13) 1) (ENTRY:PROCEDURE (? label)))
  (let ((temporary
	 (register-reference (allocate-temporary-register! 'ADDRESS))))
    (LAP (LEA (@PCR ,(procedure-external-label (label->procedure label)))
	      ,temporary)
	 (MOVE L ,temporary (@A+ 5))
	 (MOVE B (& ,type-code:return-address) (@AO 5 -4)))))

;;;; Pushes

(define-rule statement
  (ASSIGN (PRE-INCREMENT (REGISTER 15) -1) (CONSTANT (? object)))
  (record-push!
   (LAP ,(load-constant object (INST-EA (@-A 7))))))

(define-rule statement
  (ASSIGN (PRE-INCREMENT (REGISTER 15) -1) (UNASSIGNED))
  (record-push!
   (LAP ,(load-non-pointer type-code:unassigned 0 (INST-EA (@-A 7))))))

(define-rule statement
  (ASSIGN (PRE-INCREMENT (REGISTER 15) -1) (REGISTER (? r)))
  (record-push!
   (if (= r regnum:frame-pointer)
       (LAP (PEA ,(offset-reference regnum:stack-pointer
				    (frame-pointer-offset)))
	    (MOVE B (& ,type-code:stack-environment) (@A 7)))
       (LAP (MOVE L ,(coerce->any r) (@-A 7))))))

(define-rule statement
  (ASSIGN (PRE-INCREMENT (REGISTER 15) -1)
	  (CONS-POINTER (CONSTANT (? type)) (REGISTER (? r))))
  (record-push!
   (LAP (MOVE L ,(coerce->any r) (@-A 7))
	(MOVE B (& ,type) (@A 7)))))

(define-rule statement
  (ASSIGN (PRE-INCREMENT (REGISTER 15) -1) (OFFSET (REGISTER (? r)) (? n)))
  (record-push!
   (LAP (MOVE L ,(indirect-reference! r n) (@-A 7)))))

(define-rule statement
  (ASSIGN (PRE-INCREMENT (REGISTER 15) -1)
	  (OFFSET-ADDRESS (REGISTER 12) (? n)))
  (record-push!
   (LAP (PEA ,(offset-reference regnum:stack-pointer
				(+ n (frame-pointer-offset))))
	(MOVE B (& ,type-code:stack-environment) (@A 7)))))

(define-rule statement
  (ASSIGN (PRE-INCREMENT (REGISTER 15) -1) (ENTRY:CONTINUATION (? label)))
  (record-continuation-frame-pointer-offset! label)
  (record-push!
   (LAP (PEA (@PCR ,label))
	(MOVE B (& ,type-code:return-address) (@A 7)))))

;;;; Predicates

(define-rule predicate
  (TRUE-TEST (REGISTER (? register)))
  (set-standard-branches! 'NE)
  (LAP ,(test-non-pointer (ucode-type false) 0 (coerce->any register))))

(define-rule predicate
  (TRUE-TEST (OFFSET (REGISTER (? register)) (? offset)))
  (set-standard-branches! 'NE)
  (LAP ,(test-non-pointer (ucode-type false) 0
			  (indirect-reference! register offset))))

(define-rule predicate
  (TYPE-TEST (REGISTER (? register)) (? type))
  (QUALIFIER (pseudo-register? register))
  (set-standard-branches! 'EQ)
  (LAP ,(test-byte type
		   (register-reference (load-alias-register! register 'DATA)))))

(define-rule predicate
  (TYPE-TEST (OBJECT->TYPE (REGISTER (? register))) (? type))
  (QUALIFIER (pseudo-register? register))
  (set-standard-branches! 'EQ)
  (let ((reference (move-to-temporary-register! register 'DATA)))
    (LAP (RO L L (& 8) ,reference)
	 ,(test-byte type reference))))

(define-rule predicate
  (UNASSIGNED-TEST (REGISTER (? register)))
  (set-standard-branches! 'EQ)
  (LAP ,(test-non-pointer (ucode-type unassigned) 0
			  (coerce->any register))))

(define-rule predicate
  (UNASSIGNED-TEST (OFFSET (REGISTER (? register)) (? offset)))
  (set-standard-branches! 'EQ)
  (LAP ,(test-non-pointer (ucode-type unassigned) 0
			  (indirect-reference! register offset))))

(define (eq-test/constant*register constant register)
  (set-standard-branches! 'EQ)
  (if (non-pointer-object? constant)
      (LAP ,(test-non-pointer (primitive-type constant)
			      (primitive-datum constant)
			      (coerce->any register)))
      (LAP (CMP L
		(@PCR ,(constant->label constant))
		,(coerce->machine-register register)))))

(define (eq-test/constant*memory constant memory-reference)
  (set-standard-branches! 'EQ)
  (if (non-pointer-object? constant)
      (LAP ,(test-non-pointer (primitive-type constant)
			      (primitive-datum constant)
			      memory-reference))
      (let ((temp (reference-temporary-register! false)))
	(LAP (MOVE L
		   ,memory-reference
		   ,temp)
	     (CMP L
		  (@PCR ,(constant->label constant))
		  ,temp)))))

(define (eq-test/register*register register-1 register-2)
  (set-standard-branches! 'EQ)
  (let ((finish
	 (lambda (register-1 register-2)
	   (LAP (CMP L
		     ,(coerce->any register-2)
		     ,(coerce->machine-register register-1))))))
    (if (or (and (not (register-has-alias? register-1 'DATA))
		 (register-has-alias? register-2 'DATA))
	    (and (not (register-has-alias? register-1 'ADDRESS))
		 (register-has-alias? register-2 'ADDRESS)))
	(finish register-2 register-1)
	(finish register-1 register-2))))

(define (eq-test/register*memory register memory-reference)
  (set-standard-branches! 'EQ)
  (LAP (CMP L
	    ,memory-reference
	    ,(coerce->machine-register register))))

(define (eq-test/memory*memory register-1 offset-1 register-2 offset-2)
  (set-standard-branches! 'EQ)
  (let ((temp (reference-temporary-register! false)))
    (let ((finish
	   (lambda (register-1 offset-1 register-2 offset-2)
	     (LAP (MOVE L
			,(indirect-reference! register-1 offset-1)
			,temp)
		  (CMP L
		       ,(indirect-reference! register-2 offset-2)
		       ,temp)))))
      (if (or (and (not (register-has-alias? register-1 'ADDRESS))
		   (register-has-alias? register-2 'ADDRESS))
	      (and (not (register-has-alias? register-1 'DATA))
		   (register-has-alias? register-2 'DATA)))
	  (finish register-2 offset-2 register-1 offset-1)
	  (finish register-1 offset-1 register-2 offset-2)))))

(define-rule predicate
  (EQ-TEST (REGISTER (? register)) (CONSTANT (? constant)))
  (eq-test/constant*register constant register))

(define-rule predicate
  (EQ-TEST (CONSTANT (? constant)) (REGISTER (? register)))
  (eq-test/constant*register constant register))

(define-rule predicate
  (EQ-TEST (OFFSET (REGISTER (? register)) (? offset)) (CONSTANT (? constant)))
  (eq-test/constant*memory constant (indirect-reference! register offset)))

(define-rule predicate
  (EQ-TEST (CONSTANT (? constant)) (OFFSET (REGISTER (? register)) (? offset)))
  (eq-test/constant*memory constant (indirect-reference! register offset)))

(define-rule predicate
  (EQ-TEST (CONSTANT (? constant)) (POST-INCREMENT (REGISTER 15) 1))
  (eq-test/constant*memory constant (INST-EA (@A+ 7))))

(define-rule predicate
  (EQ-TEST (POST-INCREMENT (REGISTER 15) 1) (CONSTANT (? constant)))
  (eq-test/constant*memory constant (INST-EA (@A+ 7))))

(define-rule predicate
  (EQ-TEST (REGISTER (? register-1)) (REGISTER (? register-2)))
  (eq-test/register*register register-1 register-2))

(define-rule predicate
  (EQ-TEST (OFFSET (REGISTER (? register-1)) (? offset-1))
	   (REGISTER (? register-2)))
  (eq-test/register*memory register-2
			   (indirect-reference! register-1 offset-1)))

(define-rule predicate
  (EQ-TEST (REGISTER (? register-1))
	   (OFFSET (REGISTER (? register-2)) (? offset-2)))
  (eq-test/register*memory register-1
			   (indirect-reference! register-2 offset-2)))

(define-rule predicate
  (EQ-TEST (POST-INCREMENT (REGISTER 15) 1) (REGISTER (? register)))
  (record-pop!)
  (eq-test/register*memory register (INST-EA (@A+ 7))))

(define-rule predicate
  (EQ-TEST (REGISTER (? register)) (POST-INCREMENT (REGISTER 15) 1))
  (record-pop!)
  (eq-test/register*memory register (INST-EA (@A+ 7))))

(define-rule predicate
  (EQ-TEST (OFFSET (REGISTER (? register-1)) (? offset-1))
	   (OFFSET (REGISTER (? register-2)) (? offset-2)))
  (eq-test/memory*memory register-1 offset-1register-2 offset-2))

;;;; Invocations

(define-rule statement
  (INVOCATION:APPLY (? number-pushed) (? prefix) (? continuation))
  (disable-frame-pointer-offset!
   (LAP ,@(generate-invocation-prefix prefix)
	,(load-dnw number-pushed 0)
	(JMP ,entry:compiler-apply))))

(define-rule statement
  (INVOCATION:JUMP (? n)
		   (APPLY-CLOSURE (? frame-size) (? receiver-offset))
		   (? continuation) (? label))
  (disable-frame-pointer-offset!
   (LAP ,@(clear-map!)
	,@(apply-closure-sequence frame-size receiver-offset label))))

(define-rule statement
  (INVOCATION:JUMP (? n)
		   (APPLY-STACK (? frame-size) (? receiver-offset)
				(? n-levels))
		   (? continuation) (? label))
  (disable-frame-pointer-offset!
   (LAP ,@(clear-map!)
	,@(apply-stack-sequence frame-size receiver-offset n-levels label))))

(define-rule statement
  (INVOCATION:JUMP (? number-pushed) (? prefix) (? continuation) (? label))
  (QUALIFIER (not (memq (car prefix) '(APPLY-CLOSURE APPLY-STACK))))
  (disable-frame-pointer-offset!
   (LAP ,@(generate-invocation-prefix prefix)
	(BRA L (@PCR ,label)))))

(define-rule statement
  (INVOCATION:LEXPR (? number-pushed) (? prefix) (? continuation)
		    (? label))
  (disable-frame-pointer-offset!
   (LAP ,@(generate-invocation-prefix prefix)
	,(load-dnw number-pushed 0)
	(BRA L (@PCR ,label)))))

(define-rule statement
  (INVOCATION:CACHE-REFERENCE (? frame-size) (? prefix) (? continuation)
			      (? extension))
  (disable-frame-pointer-offset!
   (let ((set-extension (expression->machine-register! extension a3)))
     (delete-dead-registers!)
     (LAP ,@set-extension
	  ,@(generate-invocation-prefix prefix)
	  ,(load-dnw frame-size 0)
	  (LEA (@PCR ,*block-start-label*) (A 1))
	  (JMP ,entry:compiler-cache-reference-apply)))))

(define-rule statement
  (INVOCATION:LOOKUP (? frame-size) (? prefix) (? continuation)
		     (? environment) (? name))
  (disable-frame-pointer-offset!
   (let ((set-environment (expression->machine-register! environment d4)))
     (delete-dead-registers!)
     (LAP ,@set-environment
	  ,@(generate-invocation-prefix prefix)
	  ,(load-constant name (INST-EA (D 5)))
	  ,(load-dnw (1+ frame-size) 0)
	  (JMP ,entry:compiler-lookup-apply)))))

(define-rule statement
  (INVOCATION:PRIMITIVE (? number-pushed) (? prefix) (? continuation)
			(? primitive))
  (disable-frame-pointer-offset!
   (LAP ,@(generate-invocation-prefix prefix)
	,@(if (eq? primitive compiled-error-procedure)
	      (LAP ,(load-dnw (1+ number-pushed) 0)
		   (JMP ,entry:compiler-error))
	      (LAP ,(load-dnw (primitive-datum primitive) 6)
		   (JMP ,entry:compiler-primitive-apply))))))

(define-rule statement
  (RETURN)
  (disable-frame-pointer-offset!
   (LAP ,@(clear-map!)
	(CLR B (@A 7))
	(RTS))))

(define (generate-invocation-prefix prefix)
  (LAP ,@(clear-map!)
       ,@(case (car prefix)
	   ((NULL) (LAP))
	   ((MOVE-FRAME-UP)
	    (apply generate-invocation-prefix:move-frame-up (cdr prefix)))
	   ((APPLY-CLOSURE)
	    (apply generate-invocation-prefix:apply-closure (cdr prefix)))
	   ((APPLY-STACK)
	    (apply generate-invocation-prefix:apply-stack (cdr prefix)))
	   (else
	    (error "GENERATE-INVOCATION-PREFIX: bad prefix type" prefix)))))

(define (generate-invocation-prefix:move-frame-up frame-size how-far)
  (cond ((or (zero? frame-size) (zero? how-far))
	 (LAP))
	((= frame-size 1)
	 (LAP (MOVE L (@A+ 7) ,(offset-reference a7 (-1+ how-far)))
	      ,@(increment-anl 7 (-1+ how-far))))
	((= frame-size 2)
	 (if (= how-far 1)
	     (LAP (MOVE L (@AO 7 4) (@AO 7 8))
		  (MOVE L (@A+ 7) (@A 7)))
	     (let ((i
		    (INST (MOVE L
				(@A+ 7)
				,(offset-reference a7 (-1+ how-far))))))
	       (LAP ,i
		    ,i
		    ,@(increment-anl 7 (- how-far 2))))))
	(else
	 (let ((temp-0 (allocate-temporary-register! 'ADDRESS))
	       (temp-1 (allocate-temporary-register! 'ADDRESS)))
	   (LAP (LEA ,(offset-reference a7 frame-size)
		     ,(register-reference temp-0))
		(LEA ,(offset-reference a7 (+ frame-size how-far))
		     ,(register-reference temp-1))
	    
	    ,@(generate-n-times
	       frame-size 5
	       (INST (MOVE L
			   (@-A ,(- temp-0 8))
			   (@-A ,(- temp-1 8))))
	       (lambda (generator)
		 (generator (allocate-temporary-register! 'DATA))))
	    (MOVE L ,(register-reference temp-1) (A 7)))))))

(define (generate-invocation-prefix:apply-closure frame-size receiver-offset)
  (let ((label (generate-label)))
    (LAP ,@(apply-closure-sequence frame-size receiver-offset label)
	 (LABEL ,label))))

(define (generate-invocation-prefix:apply-stack frame-size receiver-offset
						n-levels)
  (let ((label (generate-label)))
    (LAP ,@(apply-stack-sequence frame-size receiver-offset n-levels label)
	 (LABEL ,label))))

;;;; Interpreter Calls

(define-rule statement
  (INTERPRETER-CALL:ACCESS (? environment) (? name))
  (lookup-call entry:compiler-access environment name))

(define-rule statement
  (INTERPRETER-CALL:LOOKUP (? environment) (? name) (? safe?))
  (lookup-call (if safe? entry:compiler-safe-lookup entry:compiler-lookup)
	       environment name))

(define-rule statement
  (INTERPRETER-CALL:UNASSIGNED? (? environment) (? name))
  (lookup-call entry:compiler-unassigned? environment name))

(define-rule statement
  (INTERPRETER-CALL:UNBOUND? (? environment) (? name))
  (lookup-call entry:compiler-unbound? environment name))

(define (lookup-call entry environment name)
  (let ((set-environment (expression->machine-register! environment a0)))
    (let ((clear-map (clear-map!)))
      (LAP ,@set-environment
	   ,@clear-map
	   ,(load-constant name (INST-EA (A 1)))
	   (JSR ,entry)
	   ,@(make-external-label (generate-label))))))

(define-rule statement
  (INTERPRETER-CALL:ENCLOSE (? number-pushed))
  (decrement-frame-pointer-offset!
   number-pushed
   (LAP (MOVE L (A 5) ,reg:enclose-result)
	(MOVE B (& ,(ucode-type vector)) ,reg:enclose-result)
	,(load-non-pointer (ucode-type manifest-vector) number-pushed
			   (INST-EA (@A+ 5)))
     
	,@(generate-n-times number-pushed 5
			    (INST (MOVE L (@A+ 7) (@A+ 5)))
			    (lambda (generator)
			      (generator (allocate-temporary-register! 'DATA)))))
   #| Alternate sequence which minimizes code size. ;
   DO NOT USE THIS!  The `clear-registers!' call does not distinguish between
   registers containing objects and registers containing unboxed things, and
   as a result can write unboxed stuff to memory.
   (LAP ,@(clear-registers! a0 a1 d0)
	(MOVE W (& ,number-pushed) (D 0))
	(JSR ,entry:compiler-enclose))
   |#
   ))

(define-rule statement
  (INTERPRETER-CALL:DEFINE (? environment) (? name) (? value))
  (QUALIFIER (not (eq? 'CONS-POINTER (car value))))
  (assignment-call:default entry:compiler-define environment name value))

(define-rule statement
  (INTERPRETER-CALL:SET! (? environment) (? name) (? value))
  (QUALIFIER (not (eq? 'CONS-POINTER (car value))))
  (assignment-call:default entry:compiler-set! environment name value))

(define (assignment-call:default entry environment name value)
  (let ((set-environment (expression->machine-register! environment a0)))
    (let ((set-value (expression->machine-register! value a2)))
      (let ((clear-map (clear-map!)))
	(LAP ,@set-environment
	     ,@set-value
	     ,@clear-map
	     ,(load-constant name (INST-EA (A 1)))
	     (JSR ,entry)
	     ,@(make-external-label (generate-label)))))))

(define-rule statement
  (INTERPRETER-CALL:DEFINE (? environment) (? name)
			   (CONS-POINTER (CONSTANT (? type))
					 (REGISTER (? datum))))
  (assignment-call:cons-pointer entry:compiler-define environment name type
				datum))

(define-rule statement
  (INTERPRETER-CALL:SET! (? environment) (? name)
			 (CONS-POINTER (CONSTANT (? type))
				       (REGISTER (? datum))))
  (assignment-call:cons-pointer entry:compiler-set! environment name type
				datum))

(define (assignment-call:cons-pointer entry environment name type datum)
  (let ((set-environment (expression->machine-register! environment a0)))
    (let ((datum (coerce->any datum)))
      (let ((clear-map (clear-map!)))
	(LAP ,@set-environment
	     (MOVE L ,datum ,reg:temp)
	     (MOVE B (& ,type) ,reg:temp)
	     ,@clear-map
	     (MOVE L ,reg:temp (A 2))
	     ,(load-constant name (INST-EA (A 1)))
	     (JSR ,entry)
	     ,@(make-external-label (generate-label)))))))

(define-rule statement
  (INTERPRETER-CALL:CACHE-REFERENCE (? extension) (? safe?))
  (let ((set-extension (expression->machine-register! extension a0)))
    (let ((clear-map (clear-map!)))
      (LAP ,@set-extension
	   ,@clear-map
	   (JSR ,(if safe?
		     entry:compiler-safe-reference-trap
		     entry:compiler-reference-trap))
	   ,@(make-external-label (generate-label))))))

(define-rule statement
  (INTERPRETER-CALL:CACHE-ASSIGNMENT (? extension) (? value))
  (QUALIFIER (not (eq? 'CONS-POINTER (car value))))
  (let ((set-extension (expression->machine-register! extension a0)))
    (let ((set-value (expression->machine-register! value a1)))
      (let ((clear-map (clear-map!)))
	(LAP ,@set-extension
	     ,@set-value
	     ,@clear-map
	     (JSR ,entry:compiler-assignment-trap)
	     ,@(make-external-label (generate-label)))))))

(define-rule statement
  (INTERPRETER-CALL:CACHE-ASSIGNMENT (? extension)
				     (CONS-POINTER (CONSTANT (? type))
						   (REGISTER (? datum))))
  (let ((set-extension (expression->machine-register! extension a0)))
    (let ((datum (coerce->any datum)))
      (let ((clear-map (clear-map!)))
	(LAP ,@set-extension
	     (MOVE L ,datum ,reg:temp)
	     (MOVE B (& ,type) ,reg:temp)
	     ,@clear-map
	     (MOVE L ,reg:temp (A 1))
	     (JSR ,entry:compiler-assignment-trap)
	     ,@(make-external-label (generate-label)))))))

;;; This is invoked by the top level of the LAP GENERATOR.

(define generate/quotation-header
  (let ()
    (define (declare-constants constants code)
      (define (inner constants)
	(if (null? constants)
	    code
	    (let ((entry (car constants)))
	      (LAP (SCHEME-OBJECT ,(cdr entry) ,(car entry))
		   ,@(inner (cdr constants))))))
      (inner constants))

    (lambda (block-label constants references uuo-links)
      (declare-constants references
       (declare-constants uuo-links
	(declare-constants constants
	 (if (or (not (null? references))
		 (not (null? uuo-links)))
	     (LAP ,@(let ((environment-label (allocate-constant-label)))
		      (LAP
		       (SCHEME-OBJECT ,environment-label ENVIRONMENT)
		       (LEA (@PCR ,environment-label) (A 0))))
		  (MOVE L ,reg:environment (@A 0))
		  (LEA (@PCR ,block-label) (A 0))
		  ,@(if (null? references)
			(LAP)
			(LAP
			 (LEA (@PCR ,(cdar references)) (A 1))
			 ,@(if (null? (cdr references))
			       (LAP (JSR ,entry:compiler-cache-variable))
			       (LAP ,(load-dnw (length references) 1)
				    (JSR 
				     ,entry:compiler-cache-variable-multiple)))
			 ,@(make-external-label (generate-label))))
		  ,@(if (null? uuo-links)
			(LAP)
			(LAP (LEA (@PCR ,(cdar uuo-links)) (A 1))
			     ,@(if (null? (cdr uuo-links))
				   (LAP (JSR ,entry:compiler-uuo-link))
				   (LAP ,(load-dnw (length uuo-links) 1)
					(JSR ,entry:compiler-uuo-link-multiple)))
			     ,@(make-external-label (generate-label)))))
	     (LAP))))))))

;;;; Procedure/Continuation Entries

;;; The following calls MUST appear as the first thing at the entry
;;; point of a procedure.  They assume that the register map is clear
;;; and that no register contains anything of value.

;;; **** The only reason that this is true is that no register is live
;;; across calls.  If that were not true, then we would have to save
;;; any such registers on the stack so that they would be GC'ed
;;; appropriately.

(define-rule statement
  (PROCEDURE-HEAP-CHECK (? label))
  (disable-frame-pointer-offset!
   (let ((gc-label (generate-label)))
     (LAP ,@(procedure-header (label->procedure label) gc-label)
	  (CMP L ,reg:compiled-memtop (A 5))
	  (B GE S (@PCR ,gc-label))))))

;;; Note: do not change the MOVE.W in the setup-lexpr call to a MOVEQ.
;;; The setup-lexpr code assumes a fixed calling sequence to compute
;;; the GC address if that is needed.  This could be changed so that
;;; the microcode determined how far to back up based on the argument,
;;; or by examining the calling sequence.

(define-rule statement
  (SETUP-LEXPR (? label))
  (disable-frame-pointer-offset!
   (let ((procedure (label->procedure label)))
     (LAP ,@(procedure-header procedure false)
	  (MOVE W
		(& ,(+ (procedure-required procedure)
		       (procedure-optional procedure)
		       (if (procedure/closure? procedure) 1 0)))
		(D 1))
	  (MOVEQ (& ,(if (procedure-rest procedure) 1 0)) (D 2))
	  (JSR ,entry:compiler-setup-lexpr)))))

(define-rule statement
  (CONTINUATION-HEAP-CHECK (? internal-label))
  (enable-frame-pointer-offset!
   (continuation-frame-pointer-offset (label->continuation internal-label)))
  (let ((gc-label (generate-label)))
    (LAP (LABEL ,gc-label)
	 (JSR ,entry:compiler-interrupt-continuation)
	 ,@(make-external-label internal-label)
	 (CMP L ,reg:compiled-memtop (A 5))
	 (B GE S (@PCR ,gc-label)))))

(define (procedure-header procedure gc-label)
  (let ((internal-label (procedure-label procedure)))
    (LAP ,@(if (procedure/closure? procedure)
	       (let ((required (1+ (procedure-required procedure)))
		     (optional (procedure-optional procedure))
		     (label (procedure-external-label procedure)))
		 (if (and (procedure-rest procedure)
			  (zero? required))
		     (begin (set-procedure-external-label! procedure
							   internal-label)
			    (LAP (ENTRY-POINT ,internal-label)))
		     (LAP (ENTRY-POINT ,label)
			  ,@(make-external-label label)
			  ,(test-dnw required 0)
			  ,@(cond ((procedure-rest procedure)
				   (LAP (B GE S (@PCR ,internal-label))))
				  ((zero? optional)
				   (LAP (B EQ S (@PCR ,internal-label))))
				  (else
				   (let ((wna-label (generate-label)))
				     (LAP (B LT S (@PCR ,wna-label))
					  ,(test-dnw (+ required optional) 0)
					  (B LE S (@PCR ,internal-label))
					  (LABEL ,wna-label)))))
			  (JMP ,entry:compiler-wrong-number-of-arguments))))
	       (LAP))
	 ,@(if gc-label
	       (LAP (LABEL ,gc-label)
		    (JSR ,entry:compiler-interrupt-procedure))
	       (LAP))
	 ,@(make-external-label internal-label))))

(define (make-external-label label)
  (LAP (DC W (- ,label ,*block-start-label*))
       (LABEL ,label)))

;;;; Poppers

(define-rule statement
  (MESSAGE-RECEIVER:CLOSURE (? frame-size))
  (record-push!
   (LAP (MOVE L (& ,(* frame-size 4)) (@-A 7)))))

(define-rule statement
  (MESSAGE-RECEIVER:STACK (? frame-size))
  (record-push!
   (LAP (MOVE L
	      (& ,(+ #x00100000 (* frame-size 4)))
	      (@-A 7)))))

(define-rule statement
  (MESSAGE-RECEIVER:SUBPROBLEM (? label))
  (record-continuation-frame-pointer-offset! label)
  (increment-frame-pointer-offset!
   2
   (LAP (PEA (@PCR ,label))
	(MOVE B (& ,type-code:return-address) (@A 7))
	(MOVE L (& #x00200000) (@-A 7)))))

(define (apply-closure-sequence frame-size receiver-offset label)
  (LAP ,(load-dnw frame-size 1)
       (LEA (@AO 7 ,(* (+ receiver-offset (frame-pointer-offset)) 4))
	    (A 0))
       (LEA (@PCR ,label) (A 1))
       (JMP ,popper:apply-closure)))

(define (apply-stack-sequence frame-size receiver-offset n-levels label)
  (LAP (MOVEQ (& ,n-levels) (D 0))
       ,(load-dnw frame-size 1)
       (LEA (@AO 7 ,(* (+ receiver-offset (frame-pointer-offset)) 4))
	    (A 0))
       (LEA (@PCR ,label) (A 1))
       (JMP ,popper:apply-stack)))

(define-rule statement
  (MESSAGE-SENDER:VALUE (? receiver-offset))
  (disable-frame-pointer-offset!
   (LAP ,@(clear-map!)
	,@(increment-anl 7 (+ receiver-offset (frame-pointer-offset)))
	(JMP ,popper:value))))
#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/machines/bobcat/lapgen.scm,v 1.178.1.1 1987/06/10 19:42:24 cph Exp $

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
  (INSTRUCTIONS (EVALUATE (machine->machine-register source target))))

(define (home->register-transfer source target)
  (INSTRUCTIONS (EVALUATE (pseudo->machine-register source target))))

(define (register->home-transfer source target)
  (INSTRUCTIONS (EVALUATE (machine->pseudo-register source target))))

(define-integrable (pseudo->machine-register source target)
  (memory->machine-register (pseudo-register-home source) target))

(define-integrable (machine->pseudo-register source target)
  (machine-register->memory source (pseudo-register-home target)))

(define-integrable (pseudo-register-home register)
  (offset-reference regnum:regs-pointer
		    (+ #x000A (register-renumber register))))

(define-integrable (machine->machine-register source target)
  (INSTRUCTION
   (MOVE L
	 (EVALUATE (register-reference source))
	 (EVALUATE (register-reference target)))))

(define-integrable (machine-register->memory source target)
  (INSTRUCTION
   (MOVE L
	 (EVALUATE (register-reference source))
	 (EVALUATE target))))

(define-integrable (memory->machine-register source target)
  (INSTRUCTION
   (MOVE L
	 (EVALUATE source)
	 (EVALUATE (register-reference target)))))

(define (offset-reference register offset)
  (if (zero? offset)
      (if (< register 8)
	  `(@D ,register)
	  `(@A ,(- register 8)))
      (if (< register 8)
	  `(@DO ,register ,(* 4 offset))
	  `(@AO ,(- register 8) ,(* 4 offset)))))

(define (load-dnw n d)
  (cond ((zero? n)
	 (INSTRUCTION
	  (CLR W (D (EVALUATE d)))))
	((<= -128 n 127)
	 (INSTRUCTION
	  (MOVEQ (& (EVALUATE n)) (D (EVALUATE d)))))
	(else
	 (INSTRUCTION
	  (MOVE W (& (EVALUATE n)) (D (EVALUATE d)))))))

(define (test-dnw n d)
  (if (zero? n)
      (INSTRUCTION
       (TST W (D (EVALUATE d))))
      (INSTRUCTION
       (CMP W (& (EVALUATE n)) (D (EVALUATE d))))))

(define (increment-anl an n)
  (case n
    ((0) (INSTRUCTIONS))
    ((1 2)
     (INSTRUCTIONS
      (ADDQ L
	    (& (EVALUATE (* 4 n)))
	    (A (EVALUATE an)))))
    ((-1 -2)
     (INSTRUCTIONS
      (SUBQ L
	    (& (EVALUATE (* -4 n)))
	    (A (EVALUATE an)))))
    (else
     (INSTRUCTIONS
      (LEA (@AO (EVALUATE an) (EVALUATE (* 4 n)))
	   (A (EVALUATE an)))))))

(define (load-constant constant target)
  (if (non-pointer-object? constant)
      (load-non-pointer (primitive-type constant)
			(primitive-datum constant)
			target)
      (INSTRUCTION
       (MOVE L
	     (@PCR (EVALUATE (constant->label constant)))
	     (EVALUATE target)))))

(define (load-non-pointer type datum target)
  (cond ((not (zero? type))
	 (INSTRUCTION
	  (MOVE L
		(& (EVALUATE (make-non-pointer-literal type datum)))
		(EVALUATE target))))
	((and (zero? datum)
	      (memq (car target) '(D @D @A @A+ @-A @AO @DO @AOX W L)))
	 (INSTRUCTION (CLR L (EVALUATE target))))
	((and (<= -128 datum 127) (eq? (car target) 'D))
	 (INSTRUCTION (MOVEQ (& (EVALUATE datum)) (EVALUATE target))))
	(else
	 (INSTRUCTION (MOVE L (& (EVALUATE datum)) (EVALUATE target))))))

(define (test-byte n expression)
  (if (and (zero? n) (TSTable-expression? expression))
      (INSTRUCTION (TST B (EVALUATE expression)))
      (INSTRUCTION (CMP B (& (EVALUATE n)) (EVALUATE expression)))))

(define (test-non-pointer type datum expression)
  (if (and (zero? type) (zero? datum) (TSTable-expression? expression))
      (INSTRUCTION
       (TST L (EVALUATE expression)))
      (INSTRUCTION
       (CMP L (& (EVALUATE (make-non-pointer-literal type datum))) (EVALUATE expression)))))

(define make-non-pointer-literal
  (let ((type-scale-factor (expt 2 24)))
    (lambda (type datum)
      (+ (* (if (negative? datum) (1+ type) type)
	    type-scale-factor)
	 datum))))

(define (set-standard-branches! cc)
  (set-current-branches!
   (lambda (label)
     (INSTRUCTIONS (B (EVALUATE cc) L (@PCR (EVALUATE label)))))
   (lambda (label)
     (INSTRUCTIONS (B (EVALUATE (invert-cc cc)) L (@PCR (EVALUATE label)))))))

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
	      (INSTRUCTIONS
	       (MOVE L (EVALUATE (coerce->any (cadr expression))) (EVALUATE target))))
	     ((OFFSET)
	      (INSTRUCTIONS
	       (MOVE L
		     (EVALUATE (indirect-reference! (cadadr expression)
					   (caddr expression)))
		     (EVALUATE target))))
	     ((CONSTANT)
	      (INSTRUCTIONS
	       (EVALUATE (load-constant (cadr expression) target))))
	     ((UNASSIGNED)
	      (INSTRUCTIONS
	       (EVALUATE (load-non-pointer type-code:unassigned 0 target))))
	     (else
	      (error "Unknown expression type" (car expression))))))
      (delete-machine-register! register)
      result)))

(define-integrable (TSTable-expression? expression)
  (memq (car expression) '(D @D @A @A+ @-A @DO @AO @AOX W L)))

(define-integrable (register-expression? expression)
  (memq (car expression) '(A D)))

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
	    (INSTRUCTIONS)
	    (INSTRUCTIONS (EVALUATE instruction)
			  (EVALUATE-SPLICE (loop (-1+ n))))))
      (let ((loop (generate-label 'LOOP)))
	(with-counter
	 (lambda (counter)
	   (INSTRUCTIONS (EVALUATE (load-dnw (-1+ n) counter))
			 (LABEL (EVALUATE loop))
			 (EVALUATE instruction)
			 (DB F (D (EVALUATE counter)) (@PCR (EVALUATE loop)))))))))

(define-integrable (data-register? register)
  (< register 8))

(define (address-register? register)
  (and (< register 16)
       (>= register 8)))

;;;; Registers/Entries

(let-syntax ((define-entries
	       (macro (start . names)
		 (define (loop names index)
		   (if (null? names)
		       '()
		       (cons `(DEFINE ,(symbol-append 'ENTRY:COMPILER-
						      (car names))
				'(@AO 6 ,index))
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

(define reg:compiled-memtop '(@A 6))
(define reg:environment '(@AO 6 #x000C))
(define reg:temp '(@AO 6 #x0010))
(define reg:enclose-result '(@AO 6 #x0014))

(define popper:apply-closure '(@AO 6 #x0168))
(define popper:apply-stack '(@AO 6 #x01A8))
(define popper:value '(@AO 6 #x01E8))

;;;; Transfers to Registers

;;; All assignments to pseudo registers are required to delete the
;;; dead registers BEFORE performing the assignment.  This is because
;;; the register being assigned may be PSEUDO-REGISTER=? to one of the
;;; dead registers, and thus would be flushed if the deletions
;;; happened after the assignment.

(define-rule statement
  (ASSIGN (REGISTER 12) (REGISTER 15))
  (enable-frame-pointer-offset! 0)
  (INSTRUCTIONS))

(define-rule statement
  (ASSIGN (REGISTER 15) (OFFSET-ADDRESS (REGISTER 15) (? n)))
  (decrement-frame-pointer-offset! n (increment-anl 7 n)))

(define-rule statement
  (ASSIGN (REGISTER (? target)) (OFFSET-ADDRESS (REGISTER 15) (? n)))
  (QUALIFIER (pseudo-register? target))
  (INSTRUCTIONS
   (LEA (@AO 7 (EVALUATE (* 4 n)))
	(EVALUATE (reference-assignment-alias! target 'ADDRESS)))))

(define-rule statement
  (ASSIGN (REGISTER 15) (REGISTER (? source)))
  (disable-frame-pointer-offset!
   (INSTRUCTIONS (MOVE L (EVALUATE (coerce->any source)) (A 7)))))

(define-rule statement
  (ASSIGN (REGISTER (? target)) (CONSTANT (? source)))
  (QUALIFIER (pseudo-register? target))
  (INSTRUCTIONS (EVALUATE (load-constant source (coerce->any target)))))

(define-rule statement
  (ASSIGN (REGISTER (? target)) (VARIABLE-CACHE (? name)))
  (QUALIFIER (pseudo-register? target))
  (INSTRUCTIONS (MOVE L
		      (@PCR (EVALUATE (free-reference-label name)))
		      (EVALUATE (reference-assignment-alias! target 'DATA)))))

(define-rule statement
  (ASSIGN (REGISTER (? target)) (REGISTER (? source)))
  (QUALIFIER (pseudo-register? target))
  (move-to-alias-register! source 'DATA target)
  (INSTRUCTIONS))

(define-rule statement
  (ASSIGN (REGISTER (? target)) (OBJECT->ADDRESS (REGISTER (? source))))
  (QUALIFIER (pseudo-register? target))
  (let ((target (move-to-alias-register! source 'DATA target)))
    (INSTRUCTIONS (AND L (EVALUATE mask-reference) (EVALUATE target)))))

(define-rule statement
  (ASSIGN (REGISTER (? target)) (OBJECT->TYPE (REGISTER (? source))))
  (QUALIFIER (pseudo-register? target))
  (let ((target (move-to-alias-register! source 'DATA target)))
    (INSTRUCTIONS (RO L L (& 8) (EVALUATE target)))))

(define-rule statement
  (ASSIGN (REGISTER (? target)) (OFFSET (REGISTER (? address)) (? offset)))
  (QUALIFIER (pseudo-register? target))
  (let ((source (indirect-reference! address offset)))
    (delete-dead-registers!)
    ;; The fact that the target register here is a data register is a
    ;; heuristic that works reasonably well since if the value is a
    ;; pointer, we will probably want to dereference it, which
    ;; requires that we first mask it.
    (INSTRUCTIONS
     (MOVE L
	   (EVALUATE source)
	   (EVALUATE
	    (register-reference (allocate-alias-register! target 'DATA)))))))

(define-rule statement
  (ASSIGN (REGISTER (? target)) (POST-INCREMENT (REGISTER 15) 1))
  (QUALIFIER (pseudo-register? target))
  (record-pop!)
  (delete-dead-registers!)
  (INSTRUCTIONS
   (MOVE L
	 (@A+ 7)
	 (EVALUATE
	  (register-reference (allocate-alias-register! target 'DATA))))))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (CONS-POINTER (CONSTANT (? type)) (REGISTER (? datum))))
  (QUALIFIER (pseudo-register? target))
  (let ((target* (coerce->any target))
	(datum (coerce->any datum)))
    (delete-dead-registers!)
    (if (register-expression? target*)
	(INSTRUCTIONS (MOVE L (EVALUATE datum) (EVALUATE reg:temp))
		      (MOVE B (& (EVALUATE type)) (EVALUATE reg:temp))
		      (MOVE L (EVALUATE reg:temp) (EVALUATE target*)))
	(INSTRUCTIONS (MOVE L (EVALUATE datum) (EVALUATE target*))
		      (MOVE B (& (EVALUATE type)) (EVALUATE target*))))))

;;;; Transfers to Memory

(define-rule statement
  (ASSIGN (OFFSET (REGISTER (? a)) (? n))
	  (CONSTANT (? object)))
  (INSTRUCTIONS (EVALUATE (load-constant object (indirect-reference! a n)))))

(define-rule statement
  (ASSIGN (OFFSET (REGISTER (? a)) (? n))
	  (REGISTER (? r)))
  (INSTRUCTIONS
   (MOVE L
	 (EVALUATE (coerce->any r))
	 (EVALUATE (indirect-reference! a n)))))

(define-rule statement
  (ASSIGN (OFFSET (REGISTER (? a)) (? n))
	  (POST-INCREMENT (REGISTER 15) 1))
  (record-pop!)
  (INSTRUCTIONS
   (MOVE L
	 (@A+ 7)
	 (EVALUATE (indirect-reference! a n)))))

(define-rule statement
  (ASSIGN (OFFSET (REGISTER (? a)) (? n))
	  (CONS-POINTER (CONSTANT (? type)) (REGISTER (? r))))
  (let ((target (indirect-reference! a n)))
    (INSTRUCTIONS (MOVE L (EVALUATE (coerce->any r)) (EVALUATE target))
		  (MOVE B (& (EVALUATE type)) (EVALUATE target)))))

(define-rule statement
  (ASSIGN (OFFSET (REGISTER (? a0)) (? n0))
	  (OFFSET (REGISTER (? a1)) (? n1)))
  (let ((source (indirect-reference! a1 n1)))
    (INSTRUCTIONS
     (MOVE L
	   (EVALUATE source)
	   (EVALUATE (indirect-reference! a0 n0))))))

;;;; Consing

(define-rule statement
  (ASSIGN (POST-INCREMENT (REGISTER 13) 1) (CONSTANT (? object)))
  (INSTRUCTIONS (EVALUATE (load-constant object '(@A+ 5)))))

(define-rule statement
  (ASSIGN (POST-INCREMENT (REGISTER 13) 1) (UNASSIGNED))
  (INSTRUCTIONS (EVALUATE (load-non-pointer type-code:unassigned 0 '(@A+ 5)))))

(define-rule statement
  (ASSIGN (POST-INCREMENT (REGISTER 13) 1) (REGISTER (? r)))
  (INSTRUCTIONS (MOVE L (EVALUATE (coerce->any r)) (@A+ 5))))

(define-rule statement
  (ASSIGN (POST-INCREMENT (REGISTER 13) 1) (OFFSET (REGISTER (? r)) (? n)))
  (INSTRUCTIONS (MOVE L (EVALUATE (indirect-reference! r n)) (@A+ 5))))

(define-rule statement
  (ASSIGN (POST-INCREMENT (REGISTER 13) 1) (ENTRY:PROCEDURE (? label)))
  (let ((temporary
	 (register-reference (allocate-temporary-register! 'ADDRESS))))
    (INSTRUCTIONS
     (LEA (@PCR (EVALUATE (procedure-external-label (label->procedure label))))
	  (EVALUATE temporary))
     (MOVE L (EVALUATE temporary) (@A+ 5))
     (MOVE B (& (EVALUATE type-code:return-address)) (@AO 5 -4)))))

;;;; Pushes

(define-rule statement
  (ASSIGN (PRE-INCREMENT (REGISTER 15) -1) (CONSTANT (? object)))
  (record-push!
   (INSTRUCTIONS (EVALUATE (load-constant object '(@-A 7))))))

(define-rule statement
  (ASSIGN (PRE-INCREMENT (REGISTER 15) -1) (UNASSIGNED))
  (record-push!
   (INSTRUCTIONS
    (EVALUATE (load-non-pointer type-code:unassigned 0 '(@-A 7))))))

(define-rule statement
  (ASSIGN (PRE-INCREMENT (REGISTER 15) -1) (REGISTER (? r)))
  (record-push!
   (if (= r regnum:frame-pointer)
       (INSTRUCTIONS
	(PEA (EVALUATE (offset-reference regnum:stack-pointer
					 (frame-pointer-offset))))
	(MOVE B (& (EVALUATE type-code:stack-environment)) (@A 7)))
       (INSTRUCTIONS (MOVE L (EVALUATE (coerce->any r)) (@-A 7))))))

(define-rule statement
  (ASSIGN (PRE-INCREMENT (REGISTER 15) -1)
	  (CONS-POINTER (CONSTANT (? type)) (REGISTER (? r))))
  (record-push!
   (INSTRUCTIONS (MOVE L (EVALUATE (coerce->any r)) (@-A 7))
		 (MOVE B (& (EVALUATE type)) (@A 7)))))

(define-rule statement
  (ASSIGN (PRE-INCREMENT (REGISTER 15) -1) (OFFSET (REGISTER (? r)) (? n)))
  (record-push!
   (INSTRUCTIONS (MOVE L (EVALUATE (indirect-reference! r n)) (@-A 7)))))

(define-rule statement
  (ASSIGN (PRE-INCREMENT (REGISTER 15) -1)
	  (OFFSET-ADDRESS (REGISTER 12) (? n)))
  (record-push!
   (INSTRUCTIONS (PEA (EVALUATE (offset-reference regnum:stack-pointer
					 (+ n (frame-pointer-offset)))))
		 (MOVE B (& (EVALUATE type-code:stack-environment)) (@A 7)))))

(define-rule statement
  (ASSIGN (PRE-INCREMENT (REGISTER 15) -1) (ENTRY:CONTINUATION (? label)))
  (record-continuation-frame-pointer-offset! label)
  (record-push!
   (INSTRUCTIONS (PEA (@PCR (EVALUATE label)))
		 (MOVE B (& (EVALUATE type-code:return-address)) (@A 7)))))

;;;; Predicates

(define-rule predicate
  (TRUE-TEST (REGISTER (? register)))
  (set-standard-branches! 'NE)
  (INSTRUCTIONS
   (EVALUATE (test-non-pointer (ucode-type false) 0 (coerce->any register)))))

(define-rule predicate
  (TRUE-TEST (OFFSET (REGISTER (? register)) (? offset)))
  (set-standard-branches! 'NE)
  (INSTRUCTIONS
   (EVALUATE (test-non-pointer (ucode-type false) 0
			       (indirect-reference! register offset)))))

(define-rule predicate
  (TYPE-TEST (REGISTER (? register)) (? type))
  (QUALIFIER (pseudo-register? register))
  (set-standard-branches! 'EQ)
  (INSTRUCTIONS
   (EVALUATE
    (test-byte type
	       (register-reference (load-alias-register! register 'DATA))))))

(define-rule predicate
  (TYPE-TEST (OBJECT->TYPE (REGISTER (? register))) (? type))
  (QUALIFIER (pseudo-register? register))
  (set-standard-branches! 'EQ)
  (let ((reference (move-to-temporary-register! register 'DATA)))
    (INSTRUCTIONS (RO L L (& 8) (EVALUATE reference))
		  (EVALUATE (test-byte type reference)))))

(define-rule predicate
  (UNASSIGNED-TEST (REGISTER (? register)))
  (set-standard-branches! 'EQ)
  (INSTRUCTIONS
   (EVALUATE (test-non-pointer (ucode-type unassigned) 0
			       (coerce->any register)))))

(define-rule predicate
  (UNASSIGNED-TEST (OFFSET (REGISTER (? register)) (? offset)))
  (set-standard-branches! 'EQ)
  (INSTRUCTIONS
   (EVALUATE (test-non-pointer (ucode-type unassigned) 0
			       (indirect-reference! register offset)))))

(define (eq-test/constant*register constant register)
  (set-standard-branches! 'EQ)
  (if (non-pointer-object? constant)
      (INSTRUCTIONS
       (EVALUATE (test-non-pointer (primitive-type constant)
				   (primitive-datum constant)
				   (coerce->any register))))
      (INSTRUCTIONS
       (CMP L
	    (@PCR (EVALUATE (constant->label constant)))
	    (EVALUATE (coerce->machine-register register))))))

(define (eq-test/constant*memory constant memory-reference)
  (set-standard-branches! 'EQ)
  (if (non-pointer-object? constant)
      (INSTRUCTIONS
       (EVALUATE (test-non-pointer (primitive-type constant)
				   (primitive-datum constant)
				   memory-reference)))
      (let ((temp (reference-temporary-register! false)))
	(INSTRUCTIONS (MOVE L
			    (EVALUATE memory-reference)
			    (EVALUATE temp))
		      (CMP L
			   (@PCR (EVALUATE (constant->label constant)))
			   (EVALUATE temp))))))

(define (eq-test/register*register register-1 register-2)
  (set-standard-branches! 'EQ)
  (let ((finish
	 (lambda (register-1 register-2)
	   (INSTRUCTIONS
	    (CMP L
		 (EVALUATE (coerce->any register-2))
		 (EVALUATE (coerce->machine-register register-1)))))))
    (if (or (and (not (register-has-alias? register-1 'DATA))
		 (register-has-alias? register-2 'DATA))
	    (and (not (register-has-alias? register-1 'ADDRESS))
		 (register-has-alias? register-2 'ADDRESS)))
	(finish register-2 register-1)
	(finish register-1 register-2))))

(define (eq-test/register*memory register memory-reference)
  (set-standard-branches! 'EQ)
  (INSTRUCTIONS
   (CMP L
	(EVALUATE memory-reference)
	(EVALUATE (coerce->machine-register register)))))

(define (eq-test/memory*memory register-1 offset-1 register-2 offset-2)
  (set-standard-branches! 'EQ)
  (let ((temp (reference-temporary-register! false)))
    (let ((finish
	   (lambda (register-1 offset-1 register-2 offset-2)
	     (INSTRUCTIONS
	      (MOVE L
		    (EVALUATE (indirect-reference! register-1 offset-1))
		    (EVALUATE temp))
	      (CMP L
		   (EVALUATE (indirect-reference! register-2 offset-2))
		   (EVALUATE temp))))))
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
  (eq-test/constant*memory constant '(@A+ 7)))

(define-rule predicate
  (EQ-TEST (POST-INCREMENT (REGISTER 15) 1) (CONSTANT (? constant)))
  (eq-test/constant*memory constant '(@A+ 7)))

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
  (eq-test/register*memory register '(@A+ 7)))

(define-rule predicate
  (EQ-TEST (REGISTER (? register)) (POST-INCREMENT (REGISTER 15) 1))
  (record-pop!)
  (eq-test/register*memory register '(@A+ 7)))

(define-rule predicate
  (EQ-TEST (OFFSET (REGISTER (? register-1)) (? offset-1))
	   (OFFSET (REGISTER (? register-2)) (? offset-2)))
  (eq-test/memory*memory register-1 offset-1register-2 offset-2))

;;;; Invocations

(define-rule statement
  (INVOCATION:APPLY (? number-pushed) (? prefix) (? continuation))
  (disable-frame-pointer-offset!
   (INSTRUCTIONS (EVALUATE-SPLICE (generate-invocation-prefix prefix))
		 (EVALUATE (load-dnw number-pushed 0))
		 (JMP (EVALUATE entry:compiler-apply)))))

(define-rule statement
  (INVOCATION:JUMP (? n)
		   (APPLY-CLOSURE (? frame-size) (? receiver-offset))
		   (? continuation) (? label))
  (disable-frame-pointer-offset!
   (INSTRUCTIONS
    (EVALUATE-SPLICE (clear-map!))
    (EVALUATE-SPLICE
     (apply-closure-sequence frame-size receiver-offset label)))))

(define-rule statement
  (INVOCATION:JUMP (? n)
		   (APPLY-STACK (? frame-size) (? receiver-offset)
				(? n-levels))
		   (? continuation) (? label))
  (disable-frame-pointer-offset!
   (INSTRUCTIONS
    (EVALUATE-SPLICE (clear-map!))
    (EVALUATE-SPLICE
     (apply-stack-sequence frame-size receiver-offset n-levels label)))))

(define-rule statement
  (INVOCATION:JUMP (? number-pushed) (? prefix) (? continuation) (? label))
  (QUALIFIER (not (memq (car prefix) '(APPLY-CLOSURE APPLY-STACK))))
  (disable-frame-pointer-offset!
   (INSTRUCTIONS (EVALUATE-SPLICE (generate-invocation-prefix prefix))
		 (BRA L (@PCR (EVALUATE label))))))

(define-rule statement
  (INVOCATION:LEXPR (? number-pushed) (? prefix) (? continuation)
		    (? label))
  (disable-frame-pointer-offset!
   (INSTRUCTIONS (EVALUATE-SPLICE (generate-invocation-prefix prefix))
		 (EVALUATE (load-dnw number-pushed 0))
		 (BRA L (@PCR (EVALUATE label))))))

(define-rule statement
  (INVOCATION:CACHE-REFERENCE (? frame-size) (? prefix) (? continuation)
			      (? extension))
  (disable-frame-pointer-offset!
   (let ((set-extension (expression->machine-register! extension a3)))
     (delete-dead-registers!)
     (INSTRUCTIONS (EVALUATE-SPLICE set-extension)
		   (EVALUATE-SPLICE (generate-invocation-prefix prefix))
		   (EVALUATE (load-dnw frame-size 0))
		   (LEA (@PCR (EVALUATE *block-start-label*)) (A 1))
		   (JMP (EVALUATE entry:compiler-cache-reference-apply))))))

(define-rule statement
  (INVOCATION:LOOKUP (? frame-size) (? prefix) (? continuation)
		     (? environment) (? name))
  (disable-frame-pointer-offset!
   (let ((set-environment (expression->machine-register! environment d4)))
     (delete-dead-registers!)
     (INSTRUCTIONS (EVALUATE-SPLICE set-environment)
		   (EVALUATE-SPLICE (generate-invocation-prefix prefix))
		   (EVALUATE (load-constant name '(D 5)))
		   (EVALUATE (load-dnw (1+ frame-size) 0))
		   (JMP (EVALUATE entry:compiler-lookup-apply))))))

(define-rule statement
  (INVOCATION:PRIMITIVE (? number-pushed) (? prefix) (? continuation)
			(? primitive))
  (disable-frame-pointer-offset!
   (INSTRUCTIONS
    (EVALUATE-SPLICE (generate-invocation-prefix prefix))
    (EVALUATE-SPLICE
     (if (eq? primitive compiled-error-procedure)
	 (INSTRUCTIONS (EVALUATE (load-dnw (1+ number-pushed) 0))
		       (JMP (EVALUATE entry:compiler-error)))
	 (INSTRUCTIONS (EVALUATE (load-dnw (primitive-datum primitive) 6))
		       (JMP (EVALUATE entry:compiler-primitive-apply))))))))

(define-rule statement
  (RETURN)
  (disable-frame-pointer-offset!
   (INSTRUCTIONS (EVALUATE-SPLICE (clear-map!))
		 (CLR B (@A 7))
		 (RTS))))

(define (generate-invocation-prefix prefix)
  (INSTRUCTIONS
   (EVALUATE-SPLICE (clear-map!))
   (EVALUATE-SPLICE
    (case (car prefix)
      ((NULL) (INSTRUCTIONS))
      ((MOVE-FRAME-UP)
       (apply generate-invocation-prefix:move-frame-up (cdr prefix)))
      ((APPLY-CLOSURE)
       (apply generate-invocation-prefix:apply-closure (cdr prefix)))
      ((APPLY-STACK)
       (apply generate-invocation-prefix:apply-stack (cdr prefix)))
      (else
       (error "GENERATE-INVOCATION-PREFIX: bad prefix type" prefix))))))

(define (generate-invocation-prefix:move-frame-up frame-size how-far)
  (cond ((or (zero? frame-size) (zero? how-far))
	 (INSTRUCTIONS))
	((= frame-size 1)
	 (INSTRUCTIONS
	  (MOVE L (@A+ 7) (EVALUATE (offset-reference a7 (-1+ how-far))))
	  (EVALUATE-SPLICE (increment-anl 7 (-1+ how-far)))))
	((= frame-size 2)
	 (if (= how-far 1)
	     (INSTRUCTIONS (MOVE L (@AO 7 4) (@AO 7 8))
			   (MOVE L (@A+ 7) (@A 7)))
	     (let ((i
		    (INSTRUCTION
		     (MOVE L
			   (@A+ 7)
			   (EVALUATE (offset-reference a7 (-1+ how-far)))))))
	       (INSTRUCTIONS
		(EVALUATE i) (EVALUATE i)
		(EVALUATE-SPLICE (increment-anl 7 (- how-far 2)))))))
	(else
	 (let ((temp-0 (allocate-temporary-register! 'ADDRESS))
	       (temp-1 (allocate-temporary-register! 'ADDRESS)))
	   (INSTRUCTIONS
	    (LEA (EVALUATE (offset-reference a7 frame-size))
		 (EVALUATE (register-reference temp-0)))
	    (LEA (EVALUATE (offset-reference a7 (+ frame-size how-far)))
		 (EVALUATE (register-reference temp-1)))
	    (EVALUATE-SPLICE
	     (generate-n-times
	      frame-size 5
	      (INSTRUCTION (MOVE L
				 (@-A (EVALUATE (- temp-0 8)))
				 (@-A (EVALUATE (- temp-1 8)))))
	      (lambda (generator)
		(generator (allocate-temporary-register! 'DATA)))))
	    (MOVE L (EVALUATE (register-reference temp-1)) (A 7)))))))

(define (generate-invocation-prefix:apply-closure frame-size receiver-offset)
  (let ((label (generate-label)))
    (INSTRUCTIONS (EVALUATE-SPLICE
		   (apply-closure-sequence frame-size receiver-offset label))
		  (LABEL (EVALUATE label)))))

(define (generate-invocation-prefix:apply-stack frame-size receiver-offset
						n-levels)
  (let ((label (generate-label)))
    (INSTRUCTIONS
     (EVALUATE-SPLICE
      (apply-stack-sequence frame-size receiver-offset n-levels label))
     (LABEL (EVALUATE label)))))

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
      (INSTRUCTIONS
       (EVALUATE-SPLICE set-environment)
       (EVALUATE-SPLICE clear-map)
       (EVALUATE (load-constant name '(A 1)))
       (JSR (EVALUATE entry))
       (EVALUATE-SPLICE (make-external-label (generate-label)))))))

(define-rule statement
  (INTERPRETER-CALL:ENCLOSE (? number-pushed))
  (decrement-frame-pointer-offset! number-pushed
    (INSTRUCTIONS
     (MOVE L (A 5) (EVALUATE reg:enclose-result))
     (MOVE B (& (EVALUATE (ucode-type vector))) (EVALUATE reg:enclose-result))
     (EVALUATE (load-non-pointer (ucode-type manifest-vector) number-pushed
			'(@A+ 5)))
     (EVALUATE-SPLICE
      (generate-n-times number-pushed 5 '(MOVE L (@A+ 7) (@A+ 5))
			(lambda (generator)
			  (generator (allocate-temporary-register! 'DATA))))))
#| Alternate sequence which minimizes code size.
   DO NOT USE THIS!  The `clear-registers!' call does not distinguish between
   registers containing objects and registers containing unboxed things, and
   as a result can write unboxed stuff to memory.
    (INSTRUCTIONS
     (EVALUATE-SPLICE (clear-registers! a0 a1 d0))
     (MOVE W (& (EVALUATE number-pushed)) (D 0))
     (JSR (EVALUATE entry:compiler-enclose)))
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
	(INSTRUCTIONS
	 (EVALUATE-SPLICE set-environment)
	 (EVALUATE-SPLICE set-value)
	 (EVALUATE-SPLICE clear-map)
	 (EVALUATE (load-constant name '(A 1)))
	 (JSR (EVALUATE entry))
	 (EVALUATE-SPLICE (make-external-label (generate-label))))))))

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
	(INSTRUCTIONS
	 (EVALUATE-SPLICE set-environment)
	 (MOVE L (EVALUATE datum) (EVALUATE reg:temp))
	 (MOVE B (& (EVALUATE type)) (EVALUATE reg:temp))
	 (EVALUATE-SPLICE clear-map)
	 (MOVE L (EVALUATE reg:temp) (A 2))
	 (EVALUATE (load-constant name '(A 1)))
	 (JSR (EVALUATE entry))
	 (EVALUATE-SPLICE (make-external-label (generate-label))))))))

(define-rule statement
  (INTERPRETER-CALL:CACHE-REFERENCE (? extension) (? safe?))
  (let ((set-extension (expression->machine-register! extension a0)))
    (let ((clear-map (clear-map!)))
      (INSTRUCTIONS
       (EVALUATE-SPLICE set-extension)
       (EVALUATE-SPLICE clear-map)
       (JSR (EVALUATE (if safe?
		 entry:compiler-safe-reference-trap
		 entry:compiler-reference-trap)))
       (EVALUATE-SPLICE (make-external-label (generate-label)))))))

(define-rule statement
  (INTERPRETER-CALL:CACHE-ASSIGNMENT (? extension) (? value))
  (QUALIFIER (not (eq? 'CONS-POINTER (car value))))
  (let ((set-extension (expression->machine-register! extension a0)))
    (let ((set-value (expression->machine-register! value a1)))
      (let ((clear-map (clear-map!)))
	(INSTRUCTIONS
	 (EVALUATE-SPLICE set-extension)
	 (EVALUATE-SPLICE set-value)
	 (EVALUATE-SPLICE clear-map)
	 (JSR (EVALUATE entry:compiler-assignment-trap))
	 (EVALUATE-SPLICE (make-external-label (generate-label))))))))

(define-rule statement
  (INTERPRETER-CALL:CACHE-ASSIGNMENT (? extension)
				     (CONS-POINTER (CONSTANT (? type))
						   (REGISTER (? datum))))
  (let ((set-extension (expression->machine-register! extension a0)))
    (let ((datum (coerce->any datum)))
      (let ((clear-map (clear-map!)))
	(INSTRUCTIONS
	 (EVALUATE-SPLICE set-extension)
	 (MOVE L (EVALUATE datum) (EVALUATE reg:temp))
	 (MOVE B (& (EVALUATE type)) (EVALUATE reg:temp))
	 (EVALUATE-SPLICE clear-map)
	 (MOVE L (EVALUATE reg:temp) (A 1))
	 (JSR (EVALUATE entry:compiler-assignment-trap))
	 (EVALUATE-SPLICE (make-external-label (generate-label))))))))

;;; This is invoked by the top level of the LAP GENERATOR.

(define generate/quotation-header
  (let ()
    (define (declare-constants constants code)
      (define (inner constants)
	(if (null? constants)
	    code
	    (let ((entry (car constants)))
	      (INSTRUCTIONS
	       (SCHEME-OBJECT (EVALUATE (cdr entry)) (EVALUATE (car entry)))
	       (EVALUATE-SPLICE (inner (cdr constants)))))))
      (inner constants))

    (lambda (block-label constants references uuo-links)
      (declare-constants references
       (declare-constants uuo-links
	(declare-constants constants
	 (if (or (not (null? references))
		 (not (null? uuo-links)))
	     (INSTRUCTIONS
	      (EVALUATE-SPLICE
	       (let ((environment-label (allocate-constant-label)))
		 (INSTRUCTIONS
		  (SCHEME-OBJECT (EVALUATE environment-label) ENVIRONMENT)
		  (LEA (@PCR (EVALUATE environment-label)) (A 0)))))
	      (MOVE L (EVALUATE reg:environment) (@A 0))
	      (LEA (@PCR (EVALUATE block-label)) (A 0))
	      (EVALUATE-SPLICE
	       (if (null? references)
		   (INSTRUCTIONS)
		   (INSTRUCTIONS
		    (LEA (@PCR (EVALUATE (cdar references))) (A 1))
		    (EVALUATE-SPLICE
		     (if (null? (cdr references))
			 (INSTRUCTIONS
			  (JSR (EVALUATE entry:compiler-cache-variable)))
			 (INSTRUCTIONS
			  (EVALUATE (load-dnw (length references) 1))
			  (JSR (EVALUATE
				entry:compiler-cache-variable-multiple)))))
		    (EVALUATE-SPLICE
		     (make-external-label (generate-label))))))
	      (EVALUATE-SPLICE
	       (if (null? uuo-links)
		   (INSTRUCTIONS)
		   (INSTRUCTIONS
		    (LEA (@PCR (EVALUATE (cdar uuo-links))) (A 1))
		    (EVALUATE-SPLICE
		     (if (null? (cdr uuo-links))
			 (INSTRUCTIONS (JSR ,entry:compiler-uuo-link))
			 (INSTRUCTIONS
			  (EVALUATE (load-dnw (length uuo-links) 1))
			  (JSR (EVALUATE entry:compiler-uuo-link-multiple)))))
		    (EVALUATE-SPLICE
		     (make-external-label (generate-label)))))))
	     (INSTRUCTIONS))))))))

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
     (INSTRUCTIONS
      (EVALUATE-SPLICE (procedure-header (label->procedure label) gc-label))
      (CMP L (EVALUATE reg:compiled-memtop) (A 5))
      (B GE S (@PCR (EVALUATE gc-label)))))))

;;; Note: do not change the MOVE.W in the setup-lexpr call to a MOVEQ.
;;; The setup-lexpr code assumes a fixed calling sequence to compute
;;; the GC address if that is needed.  This could be changed so that
;;; the microcode determined how far to back up based on the argument,
;;; or by examining the calling sequence.

(define-rule statement
  (SETUP-LEXPR (? label))
  (disable-frame-pointer-offset!
   (let ((procedure (label->procedure label)))
     (INSTRUCTIONS
      (EVALUATE-SPLICE (procedure-header procedure false))
      (MOVE W
	    (& (EVALUATE (+ (procedure-required procedure)
		   (procedure-optional procedure)
		   (if (procedure/closure? procedure) 1 0))))
	    (D 1))
      (MOVEQ (& (EVALUATE (if (procedure-rest procedure) 1 0))) (D 2))
      (JSR (EVALUATE entry:compiler-setup-lexpr))))))

(define-rule statement
  (CONTINUATION-HEAP-CHECK (? internal-label))
  (enable-frame-pointer-offset!
   (continuation-frame-pointer-offset (label->continuation internal-label)))
  (let ((gc-label (generate-label)))
    (INSTRUCTIONS
     (LABEL (EVALUATE gc-label))
     (JSR (EVALUATE entry:compiler-interrupt-continuation))
     (EVALUATE-SPLICE (make-external-label internal-label))
     (CMP L (EVALUATE reg:compiled-memtop) (A 5))
     (B GE S (@PCR (EVALUATE gc-label))))))

(define (procedure-header procedure gc-label)
  (let ((internal-label (procedure-label procedure)))
    (append! (if (procedure/closure? procedure)
		 (let ((required (1+ (procedure-required procedure)))
		       (optional (procedure-optional procedure))
		       (label (procedure-external-label procedure)))
		   (if (and (procedure-rest procedure)
			    (zero? required))
		       (begin (set-procedure-external-label! procedure
							     internal-label)
			      (INSTRUCTIONS (ENTRY-POINT (EVALUATE internal-label))))
		       (INSTRUCTIONS
			(ENTRY-POINT (EVALUATE label))
			(EVALUATE-SPLICE (make-external-label label))
			(EVALUATE (test-dnw required 0))
			(EVALUATE-SPLICE
			 (cond ((procedure-rest procedure)
				(INSTRUCTIONS
				 (B GE S (@PCR (EVALUATE internal-label)))))
			       ((zero? optional)
				(INSTRUCTIONS
				 (B EQ S (@PCR (EVALUATE internal-label)))))
			       (else
				(let ((wna-label (generate-label)))
				  (INSTRUCTIONS
				   (B LT S (@PCR (EVALUATE wna-label)))
				   (EVALUATE (test-dnw (+ required optional) 0))
				   (B LE S (@PCR (EVALUATE internal-label)))
				   (LABEL (EVALUATE wna-label)))))))
			(JMP (EVALUATE entry:compiler-wrong-number-of-arguments)))))
		 (INSTRUCTIONS))
	     (if gc-label
		 (INSTRUCTIONS
		  (LABEL (EVALUATE gc-label))
		  (JSR (EVALUATE entry:compiler-interrupt-procedure)))
		 (INSTRUCTIONS))
	     (INSTRUCTIONS
	      (EVALUATE-SPLICE (make-external-label internal-label))))))

(define (make-external-label label)
  (INSTRUCTIONS (DC W (- (EVALUATE label) (EVALUATE *block-start-label*)))
		(LABEL (EVALUATE label))))

;;;; Poppers

(define-rule statement
  (MESSAGE-RECEIVER:CLOSURE (? frame-size))
  (record-push!
   (INSTRUCTIONS (MOVE L (& (EVALUATE (* frame-size 4))) (@-A 7)))))

(define-rule statement
  (MESSAGE-RECEIVER:STACK (? frame-size))
  (record-push!
   (INSTRUCTIONS
    (MOVE L
	  (& (EVALUATE (+ #x00100000 (* frame-size 4))))
	  (@-A 7)))))

(define-rule statement
  (MESSAGE-RECEIVER:SUBPROBLEM (? label))
  (record-continuation-frame-pointer-offset! label)
  (increment-frame-pointer-offset! 2
    (INSTRUCTIONS
     (PEA (@PCR (EVALUATE label)))
     (MOVE B (& (EVALUATE type-code:return-address)) (@A 7))
     (MOVE L (& #x00200000) (@-A 7)))))

(define (apply-closure-sequence frame-size receiver-offset label)
  (INSTRUCTIONS
   (EVALUATE (load-dnw frame-size 1))
   (LEA (@AO 7 (EVALUATE (* (+ receiver-offset (frame-pointer-offset)) 4)))
	(A 0))
   (LEA (@PCR (EVALUATE label)) (A 1))
   (JMP (EVALUATE popper:apply-closure))))

(define (apply-stack-sequence frame-size receiver-offset n-levels label)
  (INSTRUCTIONS
   (MOVEQ (& (EVALUATE n-levels)) (D 0))
   (EVALUATE (load-dnw frame-size 1))
   (LEA (@AO 7 (EVALUATE (* (+ receiver-offset (frame-pointer-offset)) 4)))
	(A 0))
   (LEA (@PCR (EVALUATE label)) (A 1))
   (JMP (EVALUATE popper:apply-stack))))

(define-rule statement
  (MESSAGE-SENDER:VALUE (? receiver-offset))
  (disable-frame-pointer-offset!
   (INSTRUCTIONS
    (EVALUATE-SPLICE (clear-map!))
    (EVALUATE-SPLICE
     (increment-anl 7 (+ receiver-offset (frame-pointer-offset))))
    (JMP (EVALUATE popper:value)))))
(define popper:value '(@AO 6 #x01E8))
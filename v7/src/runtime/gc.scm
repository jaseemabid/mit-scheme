#| -*-Scheme-*-

$Id: gc.scm,v 14.14 1993/09/20 19:30:50 gjr Exp $

Copyright (c) 1988-93 Massachusetts Institute of Technology

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

;;;; Garbage Collector
;;; package: (runtime garbage-collector)

(declare (usual-integrations))

(define (initialize-package!)
  (set! gc-boot-loading? true)
  (set! hook/gc-flip default/gc-flip)
  (set! hook/purify default/purify)
  (set! hook/stack-overflow default/stack-overflow)
  (set! hook/hardware-trap default/hardware-trap)
  (set! default-safety-margin 4500)
  (set! pure-space-queue '())
  (set! constant-space-queue '())
  (set! hook/gc-start default/gc-start)
  (set! hook/gc-finish default/gc-finish)
  (let ((fixed-objects (get-fixed-objects-vector)))
    (let ((interrupt-vector (vector-ref fixed-objects 1)))
      (vector-set! interrupt-vector 0 condition-handler/stack-overflow)
      (vector-set! interrupt-vector 2 condition-handler/gc))
    (vector-set! fixed-objects #x0C condition-handler/hardware-trap)
    ((ucode-primitive set-fixed-objects-vector!) fixed-objects)))

(define (condition-handler/gc interrupt-code interrupt-enables)
  interrupt-code interrupt-enables
  (hook/gc-flip default-safety-margin))

(define (condition-handler/stack-overflow interrupt-code interrupt-enables)
  interrupt-code
  (hook/stack-overflow)
  (set-interrupt-enables! interrupt-enables))

(define (condition-handler/hardware-trap escape-code)
  ((ucode-primitive set-trap-state!)
   ((ucode-primitive set-trap-state!) 2)) ; Ask.
  (hook/hardware-trap escape-code))

(define hook/gc-flip)
(define hook/purify)
(define hook/stack-overflow)
(define hook/hardware-trap)
(define default-safety-margin)

(define (default/gc-flip safety-margin)
  (define (real-default)
    (gc-flip-internal safety-margin))

  (cond ((not (null? pure-space-queue))
	 (let ((result (purify-internal pure-space-queue true safety-margin)))
	   (cond ((not (pair? result))
		  ;; Wrong phase -- wait until next time.
		  (real-default))
		 ((not (car result))
		  (set! pure-space-queue (cdr pure-space-queue))
		  (queued-purification-failure)
		  (cdr result))
		 (else
		  (set! pure-space-queue '())
		  (cdr result)))))
	((not (null? constant-space-queue))
	 (let ((result
		(purify-internal constant-space-queue false safety-margin)))
	   (cond ((not (pair? result))
		  ;; Wrong phase -- wait until next time.
		  (real-default))
		 ((not (car result))
		  (set! constant-space-queue (cdr constant-space-queue))
		  (queued-purification-failure)
		  (cdr result))
		 (else
		  (set! constant-space-queue '())
		  (cdr result)))))
	(else
	 (real-default))))

(define (queued-purification-failure)
  (warn "Unable to purify all queued items; dequeuing one"))

(define (default/purify item pure-space? queue?)
  (if (not (if pure-space? (object-pure? item) (object-constant? item)))
      (cond ((not queue?)
	     (let loop ()
	       (let ((result
		      (purify-internal item
				       pure-space?
				       default-safety-margin)))
		 (cond ((not (pair? result))
			;; Wrong phase -- try again.
			(gc-flip)
			(loop))
		       ((not (car result))
			(error "PURIFY: not enough room in constant space"
			       item))
		       (else
			unspecific)))))
	    (pure-space?
	     (with-absolutely-no-interrupts
	      (lambda ()
		(set! pure-space-queue (cons item pure-space-queue))
		unspecific)))
	    (else
	     (with-absolutely-no-interrupts
	      (lambda ()
		(set! constant-space-queue (cons item constant-space-queue))
		unspecific))))))

(define (default/stack-overflow)
  (abort->nearest "Aborting!: maximum recursion depth exceeded"))

(define (default/hardware-trap escape-code)
  escape-code
  (abort->nearest "Aborting!: the hardware trapped"))

(define pure-space-queue)
(define constant-space-queue)
(define hook/gc-start)
(define hook/gc-finish)

(define (gc-flip-internal safety-margin)
  (let ((start-value (hook/gc-start)))
    (let ((space-remaining ((ucode-primitive garbage-collect) safety-margin)))
      (gc-finish start-value space-remaining)
      space-remaining)))

(define (purify-internal item pure-space? safety-margin)
  (let ((start-value (hook/gc-start)))
    (let ((result
	   ((ucode-primitive primitive-purify) item
					       pure-space?
					       safety-margin)))
      (gc-finish start-value (cdr result))
      result)))

(define (default/gc-start)
  false)

(define (default/gc-finish start-value space-remaining)
  start-value space-remaining
  false)

(define (gc-finish start-value space-remaining)
  (if (< space-remaining 4096)
      (if gc-boot-loading?
	  (let ((console ((ucode-primitive tty-output-channel 0))))
	    ((ucode-primitive channel-write 4)
	     console
	     gc-boot-death-message
	     0
	     ((ucode-primitive string-length 1) gc-boot-death-message))
	    ((ucode-primitive exit-with-value 1) #x14))
	  (abort->nearest
	   (cmdl-message/append
	    (cmdl-message/strings "Aborting!: out of memory")
	    ;; Clean up whatever possible to avoid a reoccurrence.
	    (cmdl-message/active
	     (lambda (port)
	       port
	       (with-gc-notification! true gc-clean)))))))
  ((ucode-primitive request-interrupts! 1) interrupt-bit/after-gc)
  (hook/gc-finish start-value space-remaining))

(define gc-boot-loading?)
  
(define gc-boot-death-message
  "\n;; Aborting boot-load: Not enough memory to load -- Use -large option.\n")

;;;; User Primitives

(define (set-gc-safety-margin! #!optional safety-margin)
  (if (not (or (default-object? safety-margin) (not safety-margin)))
      (begin
	(set! default-safety-margin safety-margin)
	(gc-flip safety-margin)))
  default-safety-margin)

(define (gc-flip #!optional safety-margin)
  ;; Optionally overrides the GC safety margin for this flip only.
  (with-absolutely-no-interrupts
   (lambda ()
     (hook/gc-flip (if (default-object? safety-margin)
		       default-safety-margin
		       safety-margin)))))

(define (flush-purification-queue!)
  (if (or (not (null? pure-space-queue))
	  (not (null? constant-space-queue)))
      (begin
	(gc-flip)
	(flush-purification-queue!))))

(define (purify item #!optional pure-space? queue?)
  ;; Purify an item -- move it into pure space and clean everything by
  ;; doing a gc-flip.
  (hook/purify item
	       (if (default-object? pure-space?) true pure-space?)
	       (if (default-object? queue?) true queue?))
  item)

(define (constant-space/in-use)
  (- (get-next-constant) constant-space/base))

;; This is set to the correct value during the cold load.
(define constant-space/base)
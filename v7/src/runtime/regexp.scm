;;; -*-Scheme-*-
;;;
;;; $Id: regexp.scm,v 1.7 2000/03/21 21:23:38 cph Exp $
;;;
;;; Copyright (c) 1986, 1989-1999 Massachusetts Institute of Technology
;;;
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License as
;;; published by the Free Software Foundation; either version 2 of the
;;; License, or (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;;; Regular Expressions
;;; package: (runtime regular-expression)

(declare (usual-integrations))

(define registers)

(define (initialize-package!)
  (set! registers (make-vector 20 #f))
  unspecific)

(define-structure (re-registers (type-descriptor re-registers-rtd))
  (vector #f read-only #t))

(define (guarantee-re-registers object procedure)
  (if (not (re-registers? object))
      (error:wrong-type-argument object "regular-expression registers"
				 procedure))
  (re-registers-vector object))

(define (re-match-start-index i #!optional regs)
  (guarantee-re-register i 'RE-MATCH-START-INDEX)
  (vector-ref (if (or (default-object? regs) (not regs))
		  registers
		  (guarantee-re-registers regs 'RE-MATCH-START-INDEX))
	      i))

(define (re-match-end-index i #!optional regs)
  (guarantee-re-register i 'RE-MATCH-END-INDEX)
  (vector-ref (if (or (default-object? regs) (not regs))
		  registers
		  (guarantee-re-registers regs 'RE-MATCH-START-INDEX))
	      (fix:+ i 10)))

(define (guarantee-re-register i operator)
  (if (not (and (exact-nonnegative-integer? i) (< i 10)))
      (error:wrong-type-argument i "regular-expression register" operator)))

(define (re-registers)
  (make-re-registers (vector-copy registers)))

(define (set-re-registers! regs)
  (let ((regs (guarantee-re-registers regs 'SET-RE-REGISTERS!)))
    (do ((i 0 (fix:+ i 1)))
	((fix:= 20 i))
      (vector-set! registers i (vector-ref regs i)))))

(define (preserving-re-registers thunk)
  (let ((registers* unspecific))
    (dynamic-wind (lambda () (set! registers* (re-registers)) unspecific)
		  thunk
		  (lambda () (set-re-registers! registers*)))))

(define (regexp-group . alternatives)
  (let ((alternatives
	 (list-transform-positive alternatives identity-procedure)))
    (if (null? alternatives)
	"\\(\\)"
	(apply string-append
	       (cons "\\("
		     (let loop ((alternatives alternatives))
		       (cons (car alternatives)
			     (if (null? (cdr alternatives))
				 (list "\\)")
				 (cons "\\|" (loop (cdr alternatives)))))))))))

(define (re-match-extract string regs i)
  (substring string
	     (re-match-start-index i regs)
	     (re-match-end-index i regs)))

(define (make-substring-operation name primitive)
  (lambda (regexp string start end #!optional case-fold? syntax-table)
    (let ((regexp
	   (if (compiled-regexp? regexp)
	       regexp
	       (re-compile-pattern regexp
				   (if (default-object? case-fold?)
				       #f
				       case-fold?))))
	  (regs (make-vector 20 #f)))
      (and (primitive (compiled-regexp/byte-stream regexp)
		      (compiled-regexp/translation-table regexp)
		      (char-syntax-table/entries
		       (if (or (default-object? syntax-table)
			       (not syntax-table))
			   standard-char-syntax-table
			   syntax-table))
		      regs string start end)
	   (make-re-registers regs)))))

(define re-substring-match
  (make-substring-operation 'RE-SUBSTRING-MATCH
			    (ucode-primitive re-match-substring)))

(define re-substring-search-forward
  (make-substring-operation 'RE-SUBSTRING-SEARCH-FORWARD
			    (ucode-primitive re-search-substring-forward)))

(define re-substring-search-backward
  (make-substring-operation 'RE-SUBSTRING-SEARCH-BACKWARD
			    (ucode-primitive re-search-substring-backward)))

(define (make-string-operation substring-operation)
  (lambda (regexp string #!optional case-fold? syntax-table)
    (substring-operation regexp string 0 (string-length string)
			 (if (default-object? case-fold?) #f case-fold?)
			 (if (default-object? syntax-table) #f syntax-table))))

(define re-string-match
  (make-string-operation re-substring-match))

(define re-string-search-forward
  (make-string-operation re-substring-search-forward))

(define re-string-search-backward
  (make-string-operation re-substring-search-backward))
;;; -*-Scheme-*-
;;;
;;; $Id: syntax-transforms.scm,v 1.1.2.1 2002/01/17 21:30:16 cph Exp $
;;;
;;; Copyright (c) 1989-1991, 2001, 2002 Massachusetts Institute of Technology
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
;;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;;; 02111-1307, USA.

;;;; MIT Scheme syntax

;;; Procedures to convert transformers to internal form.  Required
;;; during cold load, so must be loaded very early in the sequence.

(declare (usual-integrations))

(define (sc-macro-transformer->expander transformer)
  (lambda (form environment closing-environment)
    (make-syntactic-closure closing-environment '()
      (transformer form environment))))

(define (rsc-macro-transformer->expander transformer)
  (lambda (form environment closing-environment)
    (make-syntactic-closure environment '()
      (transformer form closing-environment))))

(define (er-macro-transformer->expander transformer)
  (lambda (form environment closing-environment)
    (make-syntactic-closure environment '()
      (transformer form
		   (let ((renames '()))
		     (lambda (identifier)
		       (let ((association (assq identifier renames)))
			 (if association
			     (cdr association)
			     (let ((rename
				    (make-syntactic-closure closing-environment
					'()
				      identifier)))
			       (set! renames
				     (cons (cons identifier rename)
					   renames))
			       rename)))))
		   (lambda (x y)
		     (identifier=? environment x
				   environment y))))))

(define (non-hygienic-macro-transformer->expander transformer)
  (lambda (form environment closing-environment)
    closing-environment
    (make-syntactic-closure environment '()
      (apply transformer (cdr form)))))
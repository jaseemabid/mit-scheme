;;; -*-Scheme-*-
;;;
;;; $Id: parser.scm,v 1.26 2001/11/14 18:15:31 cph Exp $
;;;
;;; Copyright (c) 2001 Massachusetts Institute of Technology
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

;;;; Parser language

;;; A parser is a procedure of one argument, a parser buffer.  It
;;; attempts to parse the contents of the buffer, starting at the
;;; location of the buffer pointer.  If the parse is successful, the
;;; buffer pointer is advanced to the end of the parsed segment, and a
;;; vector of results is returned.  If the parse fails, the buffer
;;; pointer is unchanged, and #F is returned.

(declare (usual-integrations))

;;;; Preprocessor

(define (preprocess-parser-expression expression
				      external-bindings
				      internal-bindings)
  (cond ((and (pair? expression)
	      (symbol? (car expression))
	      (list? (cdr expression)))
	 (let ((preprocessor (parser-preprocessor (car expression))))
	   (if preprocessor
	       (preprocessor expression external-bindings internal-bindings)
	       (error "Unknown parser expression:" expression))))
	((symbol? expression)
	 (let ((preprocessor (parser-preprocessor expression)))
	   (if preprocessor
	       (preprocessor expression external-bindings internal-bindings)
	       expression)))
	((or (string? expression)
	     (char? expression))
	 (preprocess-parser-expression `(NOISE ,expression)
				       external-bindings
				       internal-bindings))
	(else
	 (error "Unknown parser expression:" expression))))

(define (preprocess-parser-expressions expressions
				       external-bindings
				       internal-bindings)
  (map (lambda (expression)
	 (preprocess-parser-expression expression
				       external-bindings
				       internal-bindings))
       expressions))

(define (define-parser-preprocessor name procedure)
  (if (pair? name)
      (for-each (lambda (name) (define-parser-preprocessor name procedure))
		name)
      (hash-table/put! parser-preprocessors name procedure))
  name)

(syntax-table/define system-global-syntax-table 'DEFINE-*PARSER-MACRO
  (lambda (bvl expression)
    (cond ((symbol? bvl)
	   `(DEFINE-*PARSER-EXPANDER ',bvl
	      (LAMBDA ()
		,expression)))
	  ((named-lambda-bvl? bvl)
	   `(DEFINE-*PARSER-EXPANDER ',(car bvl)
	      (LAMBDA ,(cdr bvl)
		,expression)))
	  (else
	   (error "Malformed bound-variable list:" bvl)))))

(define (define-*parser-expander name procedure)
  (define-parser-macro name
    (lambda (expression external-bindings internal-bindings)
      (preprocess-parser-expression (if (pair? expression)
					(apply procedure (cdr expression))
					(procedure))
				    external-bindings
				    internal-bindings))))

(define (parser-preprocessor name)
  (or (lookup-parser-macro name)
      (hash-table/get parser-preprocessors name #f)))

(define parser-preprocessors
  (make-eq-hash-table))

(define-*parser-expander '+
  (lambda (expression)
    `(SEQ ,expression (* ,expression))))

(define-*parser-expander '?
  (lambda (expression)
    `(ALT ,expression (SEQ))))

(define-*parser-expander 'COMPLETE
  (lambda (expression)
    `(SEQ ,expression (MATCH (END-OF-INPUT)))))

(define-*parser-expander 'TOP-LEVEL
  (lambda (expression)
    `(SEQ ,expression (DISCARD-MATCHED))))

(define-parser-preprocessor '(ALT SEQ)
  (lambda (expression external-bindings internal-bindings)
    `(,(car expression)
      ,@(flatten-expressions (preprocess-parser-expressions (cdr expression)
							    external-bindings
							    internal-bindings)
			     (car expression)))))

(define-parser-preprocessor '*
  (lambda (expression external-bindings internal-bindings)
    `(,(car expression)
      ,(preprocess-parser-expression (check-1-arg expression)
				     external-bindings
				     internal-bindings))))

(define-parser-preprocessor '(MATCH NOISE)
  (lambda (expression external-bindings internal-bindings)
    `(,(car expression)
      ,(preprocess-matcher-expression (check-1-arg expression)
				      external-bindings
				      internal-bindings))))

(define-parser-preprocessor '(TRANSFORM MAP ENCAPSULATE)
  (lambda (expression external-bindings internal-bindings)
    (check-2-args expression)
    `(,(car expression) ,(cadr expression)
			,(preprocess-parser-expression (caddr expression)
						       external-bindings
						       internal-bindings))))

(define-parser-preprocessor 'WITH-POINTER
  (lambda (expression external-bindings internal-bindings)
    (check-2-args expression (lambda (expression) (symbol? (cadr expression))))
    `(,(car expression) ,(cadr expression)
			,(preprocess-parser-expression (caddr expression)
						       external-bindings
						       internal-bindings))))

(define-parser-preprocessor 'SEXP
  (lambda (expression external-bindings internal-bindings)
    external-bindings internal-bindings
    (check-1-arg expression)
    expression))

(define-parser-preprocessor 'DISCARD-MATCHED
  (lambda (expression external-bindings internal-bindings)
    external-bindings internal-bindings
    (check-0-args expression)
    expression))

(define-parser-preprocessor 'VALUES
  (lambda (expression external-bindings internal-bindings)
    external-bindings internal-bindings
    expression))

;;;; Compiler

(syntax-table/define system-global-syntax-table '*PARSER
  (lambda (expression)
    (generate-parser-code expression)))

(define (generate-parser-code expression)
  (generate-external-procedure expression preprocess-parser-expression
    (lambda (expression)
      (bind-delayed-lambdas
       (lambda (ks kf) (compile-parser-expression expression #f ks kf))
       (make-parser-ks-lambda (lambda (v kf) kf v))
       (make-kf-lambda (lambda () #f))))))

(define (compile-parser-expression expression pointer ks kf)
  (cond ((and (pair? expression)
	      (symbol? (car expression))
	      (list? (cdr expression))
	      (hash-table/get parser-compilers (car expression) #f))
	 => (lambda (entry)
	      (let ((arity (car entry))
		    (compiler (cdr entry)))
		(if (and arity (not (= (length (cdr expression)) arity)))
		    (error "Incorrect arity for parser:" expression))
		(apply compiler pointer ks kf (cdr expression)))))
	((or (symbol? expression)
	     (and (pair? expression) (eq? (car expression) 'SEXP)))
	 (wrap-external-parser `((PROTECT ,(if (pair? expression)
					       (cadr expression)
					       expression))
				 ,*buffer-name*)
			       ks
			       kf))
	(else
	 (error "Malformed parser:" expression))))

(define (wrap-external-parser expression ks kf)
  (with-value-binding expression
    (lambda (v)
      `(IF ,v
	   ,(delay-call ks v kf)
	   ,(delay-call kf)))))

(define-macro (define-parser form . compiler-body)
  (let ((name (car form))
	(parameters (cdr form)))
    `(DEFINE-PARSER-COMPILER ',name
       ,(if (symbol? parameters) `#F (length parameters))
       (LAMBDA (POINTER KS KF . ,parameters)
	 ,@compiler-body))))

(define (define-parser-compiler keyword arity compiler)
  (hash-table/put! parser-compilers keyword (cons arity compiler))
  keyword)

(define parser-compilers
  (make-eq-hash-table))

(define-parser (match expression)
  (call-with-pointer pointer
    (lambda (pointer)
      (bind-delayed-lambdas
       (lambda (ks)
	 (compile-matcher-expression expression pointer ks kf))
       (make-matcher-ks-lambda
	(lambda (kf)
	  (delay-call ks
		      `(VECTOR
			(GET-PARSER-BUFFER-TAIL ,*buffer-name* ,pointer))
		      kf)))))))

(define-parser (noise expression)
  (bind-delayed-lambdas
   (lambda (ks)
     (compile-matcher-expression expression pointer ks kf))
   (make-matcher-ks-lambda
     (lambda (kf)
       (delay-call ks `(VECTOR) kf)))))

(define-parser (values . expressions)
  pointer
  (delay-call ks
	      `(VECTOR ,@(map (lambda (expression)
				`(PROTECT ,expression))
			      expressions))
	      kf))

(define-parser (transform transform expression)
  (post-processed-parser expression pointer ks kf
    (lambda (ks v kf)
      (wrap-external-parser `((PROTECT ,transform) ,v) ks kf))))

(define-parser (map transform expression)
  (post-processed-parser expression pointer ks kf
    (lambda (ks v kf)
      (delay-call ks `(VECTOR-MAP (PROTECT ,transform) ,v) kf))))

(define-parser (encapsulate transform expression)
  (post-processed-parser expression pointer ks kf
    (lambda (ks v kf)
      (delay-call ks `(VECTOR ((PROTECT ,transform) ,v)) kf))))

(define (post-processed-parser expression pointer ks kf procedure)
  (bind-delayed-lambdas
   (lambda (ks)
     (compile-parser-expression expression pointer ks kf))
   (make-parser-ks-lambda
    (lambda (v kf)
      (procedure ks v kf)))))

(define-parser (with-pointer identifier expression)
  pointer
  ;; Ignore the POINTER context.  This is a kludge that prevents the
  ;; binding of IDENTIFIER from being discarded by the optimizer.
  `((LAMBDA (,identifier)
      ,(compile-parser-expression expression identifier ks kf))
    ,(fetch-pointer)))

(define-parser (discard-matched)
  pointer
  `(BEGIN
     (DISCARD-PARSER-BUFFER-HEAD! ,*buffer-name*)
     ,(delay-call ks `(VECTOR) kf)))

(define-parser (seq . expressions)
  (if (pair? expressions)
      (if (pair? (cdr expressions))
	  (let loop
	      ((expressions expressions)
	       (pointer pointer)
	       (vs '())
	       (kf kf))
	    (bind-delayed-lambdas
	     (lambda (ks)
	       (compile-parser-expression (car expressions) pointer ks kf))
	     (make-parser-ks-lambda
	      (lambda (v kf)
		(let ((vs (cons v vs)))
		  (if (pair? (cdr expressions))
		      (loop (cdr expressions) #f vs kf)
		      (delay-call ks `(VECTOR-APPEND ,@(reverse vs)) kf)))))))
	  (compile-parser-expression (car expressions) pointer ks kf))
      (delay-call ks `(VECTOR) kf)))

(define-parser (alt . expressions)
  (if (pair? expressions)
      (if (pair? (cdr expressions))
	  (let loop ((expressions expressions) (pointer pointer))
	    (if (pair? (cdr expressions))
		(call-with-pointer pointer
		  (lambda (pointer)
		    (bind-delayed-lambdas
		     (lambda (kf)
		       (compile-parser-expression (car expressions)
						  pointer
						  ks
						  kf))
		     (backtracking-kf pointer
		       (lambda ()
			 (loop (cdr expressions) pointer))))))
		(compile-parser-expression (car expressions)
					   pointer
					   ks
					   kf)))
	  (compile-parser-expression (car expressions) ks kf))
      (delay-call kf)))

(define-parser (* expression)
  pointer
  (let ((ks2 (make-ks-identifier))
	(v (make-value-identifier))
	(kf2 (make-kf-identifier)))
    `(LET ,ks2 ((,v (VECTOR)) (,kf2 ,kf))
       ,(call-with-pointer #f
	  (lambda (pointer)
	    (bind-delayed-lambdas
	     (lambda (ks kf)
	       (compile-parser-expression expression pointer ks kf))
	     (make-parser-ks-lambda
	      (lambda (v2 kf)
		(delay-call ks2 `(VECTOR-APPEND ,v ,(delay-reference v2)) kf)))
	     (backtracking-kf pointer
	       (lambda ()
		 (delay-call ks v kf2)))))))))
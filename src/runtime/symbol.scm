#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017 Massachusetts Institute of Technology

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

|#

;;;; Symbols
;;; package: (runtime symbol)

(declare (usual-integrations))

(declare (integrate-operator symbol?))
(define (symbol? object)
  (or (interned-symbol? object)
      (uninterned-symbol? object)))

(define-integrable (interned-symbol? object)
  (object-type? (ucode-type interned-symbol) object))

(define-integrable (uninterned-symbol? object)
  (object-type? (ucode-type uninterned-symbol) object))

(define-guarantee symbol "symbol")
(define-guarantee interned-symbol "interned symbol")
(define-guarantee uninterned-symbol "uninterned symbol")

(define (string->uninterned-symbol string #!optional start end)
  ((ucode-primitive system-pair-cons) (ucode-type uninterned-symbol)
				      (string->utf8 string start end)
				      (make-unmapped-unbound-reference-trap)))

(define (string->symbol string #!optional start end)
  ((ucode-primitive string->symbol) (string->utf8 string start end)))

(define (symbol->string symbol)
  (if (not (symbol? symbol))
      (error:not-a symbol? symbol 'symbol->string))
  (let ((s (system-pair-car symbol)))
    (cond ((maybe-ascii s))
	  ((bytevector? s) (utf8->string s))
	  ((legacy-string? s) (utf8->string (%legacy-string->bytevector s)))
	  (else (error "Illegal symbol name:" s)))))

(define (symbol . objects)
  (string->symbol (%string* objects 'symbol)))

(define (intern string)
  ((ucode-primitive string->symbol) (foldcase->utf8 string)))

(define (intern-soft string)
  ((ucode-primitive find-symbol) (foldcase->utf8 string)))

(define (symbol-name symbol)
  (let ((bytes (system-pair-car symbol)))
    (or (maybe-ascii bytes)
	(utf8->string bytes))))

(define (symbol-hash symbol #!optional modulus)
  (string-hash (symbol-name symbol) modulus))

(define (symbol<? x y)
  (string<? (symbol-name x) (symbol-name y)))

(define (symbol>? x y)
  (string<? (symbol-name y) (symbol-name x)))

(define-primitives
  (legacy-string? string? 1)
  (legacy-string-allocate string-allocate 1)
  (legacy-string-length string-length 1)
  (vector-8b-ref 2)
  (vector-8b-set! 3))

(define (maybe-ascii bytes)
  ;; Needed during cold load.
  (let ((string (object-new-type (ucode-type string) bytes)))
    (and (ascii-string? string)
	 string)))

(define (foldcase->utf8 string)
  (if (ascii-string? string)
      ;; Needed during cold load.
      (%legacy-string->bytevector (ascii-string-foldcase string))
      (string->utf8 (string-canonical-foldcase string))))

(define (ascii-string? string)
  (and (legacy-string? string)
       (let ((end (legacy-string-length string)))
	 (let loop ((i 0))
	   (if (fix:< i end)
	       (and (fix:< (vector-8b-ref string i) #x80)
		    (loop (fix:+ i 1)))
	       #t)))))

(define (ascii-string-foldcase string)
  (let ((end (legacy-string-length string)))
    (if (let loop ((i 0))
	  (if (fix:< i end)
	      (and (not (ascii-changes-when-case-folded?
			 (vector-8b-ref string i)))
		   (loop (fix:+ i 1)))
	      #t))
	string
	(let ((string* (legacy-string-allocate end)))
	  (do ((i 0 (fix:+ i 1)))
	      ((fix:= i end))
	    (vector-8b-set! string*
			    i
			    (ascii-foldcase (vector-8b-ref string i))))
	  string*))))

(define (ascii-changes-when-case-folded? code)
  (and (fix:>= code (char->integer #\A))
       (fix:<= code (char->integer #\Z))))

(define (ascii-foldcase code)
  (if (ascii-changes-when-case-folded? code)
      (fix:+ (char->integer #\a)
	     (fix:- code (char->integer #\A)))
      code))
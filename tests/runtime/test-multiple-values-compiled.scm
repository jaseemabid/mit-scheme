#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016
    Massachusetts Institute of Technology

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

(declare (usual-integrations))

(define (compiled-producer)
  (let ((a 1)
	(b 2)
	(c 3))
    (values a b c)))

(define (compiled-producer-mismatch extra)
  (cons 'compiled extra))

(define (compiled-consumer a b c)
  (note "a: "a" b: "b" c: "c)
  'compiled-done)

(define (compiled-consumer-mismatch a b)
  (note "mismatch: a: "a" b: "b)
  (cons a b))

(define (compiled-caller)

  (define (test-note producer consumer)
    (note "caller: compiled, producer: "producer", consumer: "consumer))

  (test-note 'interpreted 'interpreted)
  (call-with-values interpreted-producer interpreted-consumer)

  (test-note 'compiled 'interpreted)
  (call-with-values compiled-producer interpreted-consumer)

  (test-note 'interpreted 'compiled)
  (call-with-values interpreted-producer compiled-consumer)

  (test-note 'compiled 'compiled)
  (call-with-values compiled-producer compiled-consumer))

(define (compiled-arity-errors)
  (assert-error
   (lambda () (call-with-values compiled-producer-mismatch compiled-consumer))
   (list condition-type:wrong-number-of-arguments))
  (assert-error
   (lambda ()
     (call-with-values interpreted-producer-mismatch compiled-consumer))
   (list condition-type:wrong-number-of-arguments))
  (assert-error
   (lambda () (call-with-values compiled-producer compiled-consumer-mismatch))
   (list condition-type:wrong-number-of-arguments))
  (assert-error
   (lambda ()
     (call-with-values interpreted-producer compiled-consumer-mismatch))
   (list condition-type:wrong-number-of-arguments))

  (assert-error
   (lambda ()
     (call-with-values interpreted-producer-mismatch interpreted-consumer))
   (list condition-type:wrong-number-of-arguments))
  (assert-error
   (lambda ()
     (call-with-values compiled-producer-mismatch interpreted-consumer))
   (list condition-type:wrong-number-of-arguments))
  (assert-error
   (lambda ()
     (call-with-values interpreted-producer interpreted-consumer-mismatch))
   (list condition-type:wrong-number-of-arguments))
  (assert-error
   (lambda ()
     (call-with-values compiled-producer interpreted-consumer-mismatch))
   (list condition-type:wrong-number-of-arguments)))
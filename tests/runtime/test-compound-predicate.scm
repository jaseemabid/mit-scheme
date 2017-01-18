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

;;;; Tests for compound predicates

(declare (usual-integrations))

(define-test 'compound
  (lambda ()
    (test-compound-predicate-operations (disjoin) 'disjoin '())
    (test-compound-predicate-operations (conjoin) 'conjoin '())

    (assert-eqv string? (disjoin string?))
    (assert-eqv string? (disjoin string? string?))

    (assert-eqv string? (conjoin string?))
    (assert-eqv string? (conjoin string? string?))

    (test-compound-predicate-operations (disjoin string? symbol?)
                                        'disjoin
                                        (list string? symbol?))
    (test-compound-predicate-operations (conjoin string? symbol?)
                                        'conjoin
                                        (list string? symbol?))))

(define-test 'ordering
  (lambda ()
    (assert-true (predicate<= string? (disjoin string? symbol?)))
    (assert-false (predicate<= (disjoin string? symbol?) string?))

    (assert-false (predicate<= string? (conjoin string? symbol?)))
    (assert-true (predicate<= (conjoin string? symbol?) string?))))

(define (test-compound-predicate-operations predicate operator operands)
  (assert-true (compound-predicate? predicate))
  (assert-eqv (compound-predicate-operator predicate) operator)
  (assert-lset= eqv? (compound-predicate-operands predicate) operands))

(define-test 'construction
  (lambda ()
    (test-element-construction (disjoin)
			       '() '(41 #t "41" 'foo))
    (test-element-construction (disjoin number? boolean?)
			       '(41 #t) '("41" 'foo))
    (test-element-construction (disjoin number? string?)
			       '(41 "41") '(#t 'foo))
    (test-element-construction (conjoin)
			       '(41 #t "41" 'foo) '())
    (test-element-construction (conjoin number? boolean?)
			       '() '(41 #t "41" 'foo))
    (test-element-construction (conjoin number? string?)
			       '() '(41 #t "41" 'foo))))

(define (test-element-construction predicate data non-data)
  (let ((constructor (predicate-element-constructor predicate))
	(accessor (predicate-element-accessor predicate))
	(tagging-strategy (predicate-tagging-strategy predicate)))
    (for-each
     (lambda (datum)
       (let ((object (constructor datum)))
	 (assert-true (predicate object))
	 (assert-eq datum (accessor object))
	 (cond ((eqv? tagging-strategy predicate-tagging-strategy:never)
		(assert-eq datum object))
	       ((eqv? tagging-strategy predicate-tagging-strategy:always)
		(assert-not-eq datum object))
	       (else
		(if (predicate<= (object->predicate datum) predicate)
		    (assert-eq datum object)
		    (assert-not-eq datum object))))))
     data)
    (for-each (lambda (non-datum)
		(assert-type-error (lambda () (constructor non-datum))))
	      non-data)))
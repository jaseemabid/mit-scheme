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

;;;; Character String Operations
;;; package: (runtime string)

;;; This file is designed to be compiled with type and range checking
;;; turned off. The advertised user-visible procedures all explicitly
;;; check their arguments.
;;;
;;; Many of the procedures are split into several user versions that
;;; just validate their arguments and pass them on to an internal
;;; version (prefixed with `%') that assumes all arguments have been
;;; checked.  This avoids repeated argument checks.

(declare (usual-integrations)
	 (integrate-external "char")
	 (integrate-external "chrset"))

;;;; Primitives

(define-primitives
  (string-allocate 1)
  (string-length 1)
  (string-ref 2)
  (string-set! 3)
  (string? 1)
  vector-8b-fill!
  vector-8b-find-next-char
  vector-8b-find-next-char-ci
  vector-8b-find-previous-char
  vector-8b-find-previous-char-ci
  (vector-8b-ref 2)
  (vector-8b-set! 3))

;;;; Basic Operations

(define (make-legacy-string k #!optional char)
  (let ((string (string-allocate k)))
    (if (not (default-object? char))
	(begin
	  (guarantee 8-bit-char? char 'make-legacy-string)
	  (string-fill! string char)))
    string))

(define (make-vector-8b length #!optional ascii)
  (make-legacy-string length
		      (if (default-object? ascii)
			  ascii
			  (integer->char ascii))))

;;;; Trim

(define (string-trim-left string #!optional char-set)
  (let ((index
	 (string-find-next-char-in-set string
				       (if (default-object? char-set)
					   char-set:not-whitespace
					   char-set))))
    (if index
	(substring string index (string-length string))
	"")))

(define (string-trim-right string #!optional char-set)
  (let ((index
	 (string-find-previous-char-in-set string
					   (if (default-object? char-set)
					       char-set:not-whitespace
					       char-set))))
    (if index
	(substring string 0 (fix:+ index 1))
	"")))

(define (string-trim string #!optional char-set)
  (let* ((char-set
	 (if (default-object? char-set)
	     char-set:not-whitespace
	     char-set))
	 (index (string-find-next-char-in-set string char-set)))
    (if index
	(substring string
		   index
		   (fix:+ (string-find-previous-char-in-set string char-set)
			  1))
	"")))

;;;; Pad

(define (string-pad-right string n #!optional char)
  (guarantee-string string 'STRING-PAD-RIGHT)
  (guarantee-string-index n 'STRING-PAD-RIGHT)
  (let ((length (string-length string)))
    (if (fix:= length n)
	string
	(let ((result (string-allocate n)))
	  (if (fix:> length n)
	      (string-copy! result 0 string 0 n)
	      (begin
		(string-copy! result 0 string 0 length)
		(string-fill! result
			      (if (default-object? char)
				  #\space
				  (begin
				    (guarantee-char char 'STRING-PAD-RIGHT)
				    char))
			      length
			      n)))
	  result))))

(define (string-pad-left string n #!optional char)
  (guarantee-string string 'STRING-PAD-LEFT)
  (guarantee-string-index n 'STRING-PAD-LEFT)
  (let ((length (string-length string)))
    (if (fix:= length n)
	string
	(let ((result (string-allocate n))
	      (i (fix:- n length)))
	  (if (fix:< i 0)
	      (string-copy! result 0 string (fix:- 0 i) length)
	      (begin
		(string-fill! result
			      (if (default-object? char)
				  #\space
				  (begin
				    (guarantee-char char 'STRING-PAD-RIGHT)
				    char))
			      0
			      i)
		(string-copy! result i string 0 length)))
	  result))))

;;;; String search

(define (substring? pattern text)
  (and (string-search-forward pattern text) #t))

(define (string-search-forward pattern text)
  (guarantee-string pattern 'STRING-SEARCH-FORWARD)
  (guarantee-string text 'STRING-SEARCH-FORWARD)
  (%substring-search-forward text 0 (string-length text)
			     pattern 0 (string-length pattern)))

(define (substring-search-forward pattern text tstart tend)
  (guarantee-string pattern 'SUBSTRING-SEARCH-FORWARD)
  (guarantee-substring text tstart tend 'SUBSTRING-SEARCH-FORWARD)
  (%substring-search-forward text tstart tend
			     pattern 0 (string-length pattern)))

(define (string-search-backward pattern text)
  (guarantee-string pattern 'STRING-SEARCH-BACKWARD)
  (guarantee-string text 'STRING-SEARCH-BACKWARD)
  (%substring-search-backward text 0 (string-length text)
			      pattern 0 (string-length pattern)))

(define (substring-search-backward pattern text tstart tend)
  (guarantee-string pattern 'SUBSTRING-SEARCH-BACKWARD)
  (guarantee-substring text tstart tend 'SUBSTRING-SEARCH-BACKWARD)
  (%substring-search-backward text tstart tend
			      pattern 0 (string-length pattern)))

(define (string-search-all pattern text)
  (guarantee-string pattern 'STRING-SEARCH-ALL)
  (guarantee-string text 'STRING-SEARCH-ALL)
  (%substring-search-all text 0 (string-length text)
			 pattern 0 (string-length pattern)))

(define (substring-search-all pattern text tstart tend)
  (guarantee-string pattern 'SUBSTRING-SEARCH-ALL)
  (guarantee-substring text tstart tend 'SUBSTRING-SEARCH-ALL)
  (%substring-search-all text tstart tend
			 pattern 0 (string-length pattern)))

(define (%substring-search-forward text tstart tend pattern pstart pend)
  ;; Returns index of first matched char, or #F.
  (if (fix:< (fix:- pend pstart) 4)
      (%dumb-substring-search-forward text tstart tend pattern pstart pend)
      (%bm-substring-search-forward text tstart tend pattern pstart pend)))

(define (%dumb-substring-search-forward text tstart tend pattern pstart pend)
  (if (fix:= pstart pend)
      0
      (let* ((leader (string-ref pattern pstart))
	     (plen (fix:- pend pstart))
	     (tend (fix:- tend plen)))
	(let loop ((tstart tstart))
	  (let ((tstart
		 (let find-leader ((tstart tstart))
		   (and (fix:<= tstart tend)
			(if (char=? leader (string-ref text tstart))
			    tstart
			    (find-leader (fix:+ tstart 1)))))))
	    (and tstart
		 (if (substring=? text (fix:+ tstart 1) (fix:+ tstart plen)
				  pattern (fix:+ pstart 1) pend)
		     tstart
		     (loop (fix:+ tstart 1)))))))))

(define (%substring-search-backward text tstart tend pattern pstart pend)
  ;; Returns index following last matched char, or #F.
  (if (fix:< (fix:- pend pstart) 4)
      (%dumb-substring-search-backward text tstart tend pattern pstart pend)
      (%bm-substring-search-backward text tstart tend pattern pstart pend)))

(define (%dumb-substring-search-backward text tstart tend pattern pstart pend)
  (if (fix:= pstart pend)
      0
      (let* ((pend-1 (fix:- pend 1))
	     (trailer (string-ref pattern pend-1))
	     (plen (fix:- pend pstart))
	     (tstart+plen (fix:+ tstart plen)))
	(let loop ((tend tend))
	  (let ((tend
		 (let find-trailer ((tend tend))
		   (and (fix:<= tstart+plen tend)
			(if (char=? trailer (string-ref text (fix:- tend 1)))
			    tend
			    (find-trailer (fix:- tend 1)))))))
	    (and tend
		 (if (substring=? text (fix:- tend plen) (fix:- tend 1)
				  pattern pstart pend-1)
		     tend
		     (loop (fix:- tend 1)))))))))

(define (%substring-search-all text tstart tend pattern pstart pend)
  (let ((plen (fix:- pend pstart)))
    (cond ((fix:= plen 1)
	   (let ((c (string-ref pattern pstart)))
	     (let loop ((ti tend) (occurrences '()))
	       (let ((index (substring-find-previous-char text tstart ti c)))
		 (if index
		     (loop index (cons index occurrences))
		     occurrences)))))
	  #;    ;This may not be worthwhile -- I have no measurements.
	  ((fix:< plen 4)
	   (let loop ((ti tend) (occurrences '()))
	     (let ((index
		    (%dumb-substring-search-backward text tstart ti
						     pattern pstart pend)))
	       (if index
		   (loop (fix:+ index (fix:- plen 1)) (cons index occurrences))
		   occurrences))))
	  (else
	   (%bm-substring-search-all text tstart tend pattern pstart pend)))))

;;;; Boyer-Moore String Search

;;; Cormen, Leiserson, and Rivest, "Introduction to Algorithms",
;;; Chapter 34, "String Matching".

(define (%bm-substring-search-forward text tstart tend pattern pstart pend)
  (let ((m (fix:- pend pstart))
	(pstart-1 (fix:- pstart 1))
	(pend-1 (fix:- pend 1))
	(lambda* (compute-last-occurrence-function pattern pstart pend))
	(gamma
	 (compute-good-suffix-function pattern pstart pend
				       (compute-gamma0 pattern pstart pend))))
    (let ((tend-m (fix:- tend m))
	  (m-1 (fix:- m 1)))
      (let outer ((s tstart))
	(and (fix:<= s tend-m)
	     (let inner ((pj pend-1) (tj (fix:+ s m-1)))
	       (if (fix:= (vector-8b-ref pattern pj) (vector-8b-ref text tj))
		   (if (fix:= pstart pj)
		       s
		       (inner (fix:- pj 1) (fix:- tj 1)))
		   (outer
		    (fix:+ s
			   (fix:max (fix:- (fix:- pj pstart-1)
					   (lambda* (vector-8b-ref text tj)))
				    (gamma (fix:- pj pstart))))))))))))

(define (%bm-substring-search-backward text tstart tend pattern pstart pend)
  (let ((m (fix:- pend pstart))
	(pend-1 (fix:- pend 1))
	(rpattern (reverse-pattern pattern pstart pend)))
    (let ((tstart+m (fix:+ tstart m))
	  (lambda* (compute-last-occurrence-function rpattern 0 m))
	  (gamma
	   (compute-good-suffix-function rpattern 0 m
					 (compute-gamma0 rpattern 0 m))))
      (let outer ((s tend))
	(and (fix:>= s tstart+m)
	     (let inner ((pj pstart) (tj (fix:- s m)))
	       (if (fix:= (vector-8b-ref pattern pj) (vector-8b-ref text tj))
		   (if (fix:= pend-1 pj)
		       s
		       (inner (fix:+ pj 1) (fix:+ tj 1)))
		   (outer
		    (fix:- s
			   (fix:max (fix:- (fix:- pend pj)
					   (lambda* (vector-8b-ref text tj)))
				    (gamma (fix:- pend-1 pj))))))))))))

(define (%bm-substring-search-all text tstart tend pattern pstart pend)
  (let ((m (fix:- pend pstart))
	(pstart-1 (fix:- pstart 1))
	(pend-1 (fix:- pend 1))
	(lambda* (compute-last-occurrence-function pattern pstart pend))
	(gamma0 (compute-gamma0 pattern pstart pend)))
    (let ((gamma (compute-good-suffix-function pattern pstart pend gamma0))
	  (tend-m (fix:- tend m))
	  (m-1 (fix:- m 1)))
      (let outer ((s tstart) (occurrences '()))
	(if (fix:<= s tend-m)
	    (let inner ((pj pend-1) (tj (fix:+ s m-1)))
	      (if (fix:= (vector-8b-ref pattern pj) (vector-8b-ref text tj))
		  (if (fix:= pstart pj)
		      (outer (fix:+ s gamma0) (cons s occurrences))
		      (inner (fix:- pj 1) (fix:- tj 1)))
		  (outer (fix:+ s
				(fix:max (fix:- (fix:- pj pstart-1)
						(lambda*
						 (vector-8b-ref text tj)))
					 (gamma (fix:- pj pstart))))
			 occurrences)))
	    (reverse! occurrences))))))

(define (compute-last-occurrence-function pattern pstart pend)
  (let ((lam (make-vector 256 0)))
    (do ((j pstart (fix:+ j 1)))
	((fix:= j pend))
      (vector-set! lam
		   (vector-8b-ref pattern j)
		   (fix:+ (fix:- j pstart) 1)))
    (lambda (symbol)
      (vector-ref lam symbol))))

(define (compute-good-suffix-function pattern pstart pend gamma0)
  (let ((m (fix:- pend pstart)))
    (let ((pi
	   (compute-prefix-function (reverse-pattern pattern pstart pend)
				    0
				    m))
	  (gamma (make-vector m gamma0))
	  (m-1 (fix:- m 1)))
      (do ((l 0 (fix:+ l 1)))
	  ((fix:= l m))
	(let ((j (fix:- m-1 (vector-ref pi l)))
	      (k (fix:- (fix:+ 1 l) (vector-ref pi l))))
	  (if (fix:< k (vector-ref gamma j))
	      (vector-set! gamma j k))))
      (lambda (index)
	(vector-ref gamma index)))))

(define (compute-gamma0 pattern pstart pend)
  (let ((m (fix:- pend pstart)))
    (fix:- m
	   (vector-ref (compute-prefix-function pattern pstart pend)
		       (fix:- m 1)))))

(define (compute-prefix-function pattern pstart pend)
  (let* ((m (fix:- pend pstart))
	 (pi (make-vector m)))
    (vector-set! pi 0 0)
    (let outer ((k 0) (q 1))
      (if (fix:< q m)
	  (let ((k
		 (let ((pq (vector-8b-ref pattern (fix:+ pstart q))))
		   (let inner ((k k))
		     (cond ((fix:= pq (vector-8b-ref pattern (fix:+ pstart k)))
			    (fix:+ k 1))
			   ((fix:= k 0)
			    k)
			   (else
			    (inner (vector-ref pi (fix:- k 1)))))))))
	    (vector-set! pi q k)
	    (outer k (fix:+ q 1)))))
    pi))

(define (reverse-pattern pattern pstart pend)
  (let ((builder (string-builder)))
    (do ((i (fix:- pend 1) (fix:- i 1)))
	((not (fix:>= i pstart)))
      (builder (string-ref pattern i)))
    (builder)))

;;;; Guarantors
;;
;; The guarantors are integrated.  Most are structured as combination of
;; simple tests which the compiler can open-code, followed by a call to a
;; GUARANTEE-.../FAIL version which does the tests again to signal a
;; meaningful message.  Structuring the code this way significantly
;; reduces code bloat from large integrated procedures.

(declare (integrate-operator guarantee-string))
(define-guarantee string "string")

(define-integrable (guarantee-2-strings object1 object2 procedure)
  (if (not (and (string? object1) (string? object2)))
      (guarantee-2-strings/fail object1 object2 procedure)))

(define (guarantee-2-strings/fail object1 object2 procedure)
  (cond ((not (string? object1))
	 (error:wrong-type-argument object1 "string" procedure))
	((not (string? object2))
	 (error:wrong-type-argument object2 "string" procedure))))

(define-integrable (guarantee-string-index object caller)
  (if (not (index-fixnum? object))
      (error:wrong-type-argument object "string index" caller)))

(define-integrable (guarantee-substring string start end caller)
  (if (not (and (string? string)
		(index-fixnum? start)
		(index-fixnum? end)
		(fix:<= start end)
		(fix:<= end (string-length string))))
      (guarantee-substring/fail string start end caller)))

(define (guarantee-substring/fail string start end caller)
  (guarantee-string string caller)
  (guarantee-substring-end-index end (string-length string) caller)
  (guarantee-substring-start-index start end caller))

(define-integrable (guarantee-substring-end-index end length caller)
  (guarantee-string-index end caller)
  (if (not (fix:<= end length))
      (error:bad-range-argument end caller))
  end)

(define-integrable (guarantee-substring-start-index start end caller)
  (guarantee-string-index start caller)
  (if (not (fix:<= start end))
      (error:bad-range-argument start caller))
  start)

(define-integrable (guarantee-2-substrings string1 start1 end1
					   string2 start2 end2
					   procedure)
  (guarantee-substring string1 start1 end1 procedure)
  (guarantee-substring string2 start2 end2 procedure))
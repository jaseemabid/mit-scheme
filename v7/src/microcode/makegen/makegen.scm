#| -*-Scheme-*-

$Id: makegen.scm,v 1.7.2.4 2007/01/06 00:10:00 cph Exp $

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007 Massachusetts Institute of Technology

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

;;;; Generate "Makefile.in" from template.

(declare (usual-integrations))

(load-option 'REGULAR-EXPRESSION)
(load-option 'SYNCHRONOUS-SUBPROCESS)

(define (generate-makefile template deps-filename makefile)
  (let ((file-lists
	 (let ((make-list
		(lambda (pattern proc)
		  (map (lambda (pathname)
			 (cons (pathname-name pathname)
			       (proc (read-file pathname))))
		       (keep-matching-items (directory-read "makegen/")
			 (lambda (pathname)
			   (re-string-match (string-append "^"
							   pattern
							   "\\.scm$")
					    (file-namestring pathname))))))))
	   (append
	    (make-list "files-.+" (lambda (files) files))
	    (make-list "dirs-.+" enumerate-directories)))))
    (call-with-input-file template
      (lambda (input)
	(call-with-output-file makefile
	  (lambda (output)
	    (write-string "# This file automatically generated from " output)
	    (write-string (file-namestring template) output)
	    (newline output)
	    (write-string "# on " output)
	    (write-string (universal-time->string (get-universal-time)) output)
	    (write-string "." output)
	    (newline output)
	    (newline output)
	    (let loop ((column 0))
	      (let ((char (read-char input)))
		(if (not (eof-object? char))
		    (if (and (char=? #\@ char)
			     (eqv? #\( (peek-char input)))
			(let ((command (read input)))
			  (if (eqv? #\@ (peek-char input))
			      (read-char input)
			      (error "Missing @ at end of command:" command))
			  (loop (interpret-command command column
						   file-lists deps-filename
						   output)))
			(begin
			  (write-char char output)
			  (loop
			   (if (char=? #\newline char)
			       0
			       (+ column 1))))))))))))))

(define (enumerate-directories specs)
  (map (lambda (path)
	 (enough-namestring (pathname-new-type path #f)))
       (append-map (lambda (spec)
		     (delete-matching-items
			 (directory-read
			  (merge-pathnames
			   "*.scm"
			   (pathname-as-directory (car spec))))
		       (lambda (path)
			 (member (pathname-name path) (cdr spec)))))
		   specs)))

(define (interpret-command command column file-lists deps-filename output)
  (let ((malformed (lambda () (error "Malformed command:" command))))
    (if (not (and (pair? command)
		  (symbol? (car command))
		  (list? (cdr command))))
	(malformed))
    (let ((guarantee-nargs
	   (lambda (n)
	     (if (not (= n (length (cdr command))))
		 (malformed)))))
      (let ((write-suffixed
	     (lambda (suffix)
	       (guarantee-nargs 1)
	       (let ((entry (assoc (cadr command) file-lists)))
		 (if (not entry)
		     (malformed))
		 (write-items (map (lambda (file) (string-append file suffix))
				   (cdr entry))
			      column
			      output)))))
      (case (car command)
	((WRITE-SOURCES)
	 (write-suffixed ".c"))
	((WRITE-OBJECTS)
	 (write-suffixed ".o"))
	((WRITE-DEPENDENCIES)
	 (guarantee-nargs 0)
	 (write-dependencies file-lists deps-filename output))
	(else
	 (error "Unknown command:" command)))))))

(define (write-dependencies file-lists deps-filename output)
  (maybe-update-dependencies
   deps-filename
   (sort (append-map (lambda (file-list)
		       (if (string-prefix? "files-" (car file-list))
			   (map (lambda (base) (string-append base ".c"))
				(cdr file-list))
			   '()))
		     file-lists)
	 string<?))
  (call-with-input-file deps-filename
    (lambda (input)
      (let ((buffer (make-string 4096)))
	(let loop ()
	  (let ((n (read-substring! buffer 0 4096 input)))
	    (if (> n 0)
		(begin
		  (write-substring buffer 0 n output)
		  (loop)))))))))

(define (maybe-update-dependencies deps-filename source-files)
  (if (let ((mtime (file-modification-time deps-filename)))
	(or (not mtime)
	    (there-exists? source-files
	      (lambda (source-file)
		(> (file-modification-time source-file) mtime)))))
      (let ((rules (map generate-rule source-files)))
	(call-with-output-file deps-filename
	  (lambda (output)
	    (let loop ((rules rules))
	      (if (pair? rules)
		  (begin
		    (write-rule (car rules) output)
		    (if (pair? (cdr rules))
			(begin
			  (newline output)
			  (loop (cdr rules)))))))
	    (newline output)
	    (newline output)
	    (let ((rule (generate-rule "liarc-gendeps.c"))
		  (prefix "LIARC_HEAD_FILES = "))
	      (write-string prefix output)
	      (write-items (cddr rule) (string-length prefix) output)))))))

(define (generate-rule filename)
  (parse-rule
   (unbreak-lines
    (with-string-output-port
     (lambda (port)
       (run-shell-command (string-append "./makegen-cc " filename)
			  'OUTPUT port))))))

(define (unbreak-lines string)
  (let ((indexes (string-search-all "\\\n" string)))
    (let ((n (length indexes))
	  (end (string-length string)))
      (let ((result (make-string (- end (* 2 n)))))
	(let loop ((start 0) (indexes indexes) (rstart 0))
	  (if (pair? indexes)
	      (begin
		(substring-move! string start (car indexes) result rstart)
		(loop (+ (car indexes) 2)
		      (cdr indexes)
		      (+ rstart (- (car indexes) start))))
	      (substring-move! string start end result rstart)))
	result))))

(define (parse-rule rule)
  (let ((items (burst-string rule char-set:whitespace #t)))
    (if (not (string-suffix? ":" (car items)))
	(error "Missing rule target:" rule))
    (cons* (string-head (car items) (- (string-length (car items)) 1))
	   (cadr items)
	   (sort (list-transform-negative (cddr items) pathname-absolute?)
		 string<?))))

(define (write-rule rule port)
  (write-string (car rule) port)
  (write-string ": " port)
  (write-items (cdr rule) (+ (string-length (car rule)) 2) port))

(define (write-items items start-column port)
  (let loop ((items* items) (column start-column))
    (if (pair? items*)
	(let ((column
	       (if (eq? items* items)
		   column
		   (begin
		     (write-string " " port)
		     (+ column 1))))
	      (delta (string-length (car items*))))
	  (let ((new-column (+ column delta)))
	    (if (>= new-column 78)
		(begin
		  (write-string "\\\n\t" port)
		  (write-string (car items*) port)
		  (loop (cdr items*) (+ 8 delta)))
		(begin
		  (write-string (car items*) port)
		  (loop (cdr items*) new-column)))))
	column)))
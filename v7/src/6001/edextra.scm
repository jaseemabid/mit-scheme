#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/6001/edextra.scm,v 1.4 1992/09/02 02:56:36 cph Exp $

Copyright (c) 1992 Massachusetts Institute of Technology

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

;;;; 6.001: Edwin Extensions

(declare (usual-integrations))

(load-edwin-library 'PRINT)

(define-command print-graphics
  "Print out the last displayed picture."
  '()
  (let ((call-with-last-picture-file
	 (environment-lookup (->environment '(student pictures))
			     'call-with-last-picture-file)))
    (lambda ()
      (call-with-last-picture-file
       (lambda (filename)
	 (if filename
	     (begin
	       (message "Spooling...")
	       (shell-command
		false false false false
		(string-append "/users/u6001/bin/print-pgm.sh "
			       filename
			       " "
			       (print/assemble-switches "Scheme Picture" '())))
	       (append-message "done"))
	     (editor-error "No picture to print!")))))))
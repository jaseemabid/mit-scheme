;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/edwin/schmod.scm,v 1.14 1990/10/03 04:55:57 cph Rel $
;;;
;;;	Copyright (c) 1986, 1989, 1990 Massachusetts Institute of Technology
;;;
;;;	This material was developed by the Scheme project at the
;;;	Massachusetts Institute of Technology, Department of
;;;	Electrical Engineering and Computer Science.  Permission to
;;;	copy this software, to redistribute it, and to use it for any
;;;	purpose is granted, subject to the following restrictions and
;;;	understandings.
;;;
;;;	1. Any copy made of this software must include this copyright
;;;	notice in full.
;;;
;;;	2. Users of this software agree to make their best efforts (a)
;;;	to return to the MIT Scheme project any improvements or
;;;	extensions that they make, so that these may be included in
;;;	future releases; and (b) to inform MIT of noteworthy uses of
;;;	this software.
;;;
;;;	3. All materials developed as a consequence of the use of this
;;;	software shall duly acknowledge such use, in accordance with
;;;	the usual standards of acknowledging credit in academic
;;;	research.
;;;
;;;	4. MIT has made no warrantee or representation that the
;;;	operation of this software will be error-free, and MIT is
;;;	under no obligation to provide any services, by way of
;;;	maintenance, update, or otherwise.
;;;
;;;	5. In conjunction with products arising from the use of this
;;;	material, there shall be no use of the name of the
;;;	Massachusetts Institute of Technology nor of any adaptation
;;;	thereof in any advertising, promotional, or sales literature
;;;	without prior written consent from MIT in each case.
;;;
;;; NOTE: Parts of this program (Edwin) were created by translation
;;; from corresponding parts of GNU Emacs.  Users should be aware that
;;; the GNU GENERAL PUBLIC LICENSE may apply to these parts.  A copy
;;; of that license should have been included along with this file.
;;;

;;;; Scheme Mode

(declare (usual-integrations))

(define-command scheme-mode
  "Enter Scheme mode."
  ()
  (lambda ()
    (set-current-major-mode! (ref-mode-object scheme))))

(define-major-mode scheme fundamental "Scheme"
  "Major mode specialized for editing Scheme code.
\\[lisp-indent-line] indents the current line for Scheme.
\\[indent-sexp] indents the next s-expression.

The following commands evaluate Scheme expressions;
normally they record the associated output in a transcript buffer:

\\[eval-expression] reads and evaluates an expression in minibuffer.
\\[eval-previous-sexp] evaluates the expression preceding point.
\\[eval-definition] evaluates the current definition.
\\[eval-buffer] evaluates the buffer.
\\[eval-region] evaluates the current region."

  (local-set-variable! syntax-table scheme-mode:syntax-table)
  (local-set-variable! syntax-ignore-comments-backwards false)
  (local-set-variable! lisp-indent-hook standard-lisp-indent-hook)
  (local-set-variable! lisp-indent-methods scheme-mode:indent-methods)
  (local-set-variable! comment-column 40)
  (local-set-variable! comment-locator-hook lisp-comment-locate)
  (local-set-variable! comment-indent-hook lisp-comment-indentation)
  (local-set-variable! comment-start ";")
  (local-set-variable! comment-end "")
  (local-set-variable! paragraph-start "^$")
  (local-set-variable! paragraph-separate (ref-variable paragraph-start))
  (local-set-variable! indent-line-procedure (ref-command lisp-indent-line))
  (event-distributor/invoke! (ref-variable scheme-mode-hook)))

(define-variable scheme-mode-hook
  "An event distributor that is invoked when entering Scheme mode."
  (make-event-distributor))

(define-key 'scheme #\rubout 'backward-delete-char-untabify)
(define-key 'scheme #\tab 'lisp-indent-line)
(define-key 'scheme #\) 'lisp-insert-paren)
(define-key 'scheme #\m-o 'eval-buffer)
(define-key 'scheme #\m-z 'eval-definition)
(define-key 'scheme #\c-m-q 'indent-sexp)
(define-key 'scheme #\c-m-z 'eval-region)

;;;; Read Syntax

(define scheme-mode:syntax-table (make-syntax-table))

(modify-syntax-entries! scheme-mode:syntax-table #\nul #\/ "_")
(modify-syntax-entries! scheme-mode:syntax-table #\: #\@ "_")
(modify-syntax-entries! scheme-mode:syntax-table #\[ #\` "_")
(modify-syntax-entries! scheme-mode:syntax-table #\{ #\rubout "_")

(modify-syntax-entry! scheme-mode:syntax-table #\space " ")
(modify-syntax-entry! scheme-mode:syntax-table #\tab " ")
(modify-syntax-entry! scheme-mode:syntax-table #\page " ")
(modify-syntax-entry! scheme-mode:syntax-table #\[ " ")
(modify-syntax-entry! scheme-mode:syntax-table #\] " ")
(modify-syntax-entry! scheme-mode:syntax-table #\{ " ")
(modify-syntax-entry! scheme-mode:syntax-table #\} " ")
(modify-syntax-entry! scheme-mode:syntax-table #\| "  23")

(modify-syntax-entry! scheme-mode:syntax-table #\; "< ")
(modify-syntax-entry! scheme-mode:syntax-table #\newline "> ")

(modify-syntax-entry! scheme-mode:syntax-table #\' "  p")
(modify-syntax-entry! scheme-mode:syntax-table #\` "  p")
(modify-syntax-entry! scheme-mode:syntax-table #\, "_ p")
(modify-syntax-entry! scheme-mode:syntax-table #\@ "_ p")
(modify-syntax-entry! scheme-mode:syntax-table #\# "_ p14")

(modify-syntax-entry! scheme-mode:syntax-table #\" "\" ")
(modify-syntax-entry! scheme-mode:syntax-table #\\ "\\ ")
(modify-syntax-entry! scheme-mode:syntax-table #\( "()")
(modify-syntax-entry! scheme-mode:syntax-table #\) ")(")

;;;; Indentation

(define (scheme-mode:indent-let-method state indent-point normal-indent)
  (lisp-indent-special-form
   (let ((m (parse-state-containing-sexp state)))
     (let ((start (forward-to-sexp-start (forward-one-sexp (mark1+ m)
							   indent-point)
					 indent-point)))
       (if (and start
		(not (re-match-forward "\\s(" start)))
	   2
	   1)))
   state indent-point normal-indent))

(define scheme-mode:indent-methods (make-string-table))

(for-each (lambda (entry)
	    (string-table-put! scheme-mode:indent-methods
			       (symbol->string (car entry))
			       (cdr entry)))
	  `(
	    (BEGIN . 0)
	    (CASE . 1)
	    (DELAY . 0)
	    (DO . 2)
	    (LAMBDA . 1)
	    (LET . ,scheme-mode:indent-let-method)
	    (LET* . 1)
	    (LETREC . 1)

	    (CALL-WITH-INPUT-FILE . 1)
	    (WITH-INPUT-FROM-FILE . 1)
	    (CALL-WITH-OUTPUT-FILE . 1)
	    (WITH-OUTPUT-TO-FILE . 1)

	    ;; Remainder are MIT Scheme specific.

	    (FLUID-LET . 1)
	    (IN-PACKAGE . 1)
	    (LET-SYNTAX . 1)
	    (LOCAL-DECLARE . 1)
	    (MACRO . 1)
	    (MAKE-ENVIRONMENT . 0)
	    (NAMED-LAMBDA . 1)
	    (USING-SYNTAX . 1)

	    (WITH-INPUT-FROM-PORT . 1)
	    (WITH-INPUT-FROM-STRING . 1)
	    (WITH-OUTPUT-TO-PORT . 1)
	    (WITH-OUTPUT-TO-STRING . 0)
	    (WITH-VALUES . 1)

	    (BIND-CONDITION-HANDLER . 2)
	    (LIST-TRANSFORM-POSITIVE . 1)
	    (LIST-TRANSFORM-NEGATIVE . 1)
	    (LIST-SEARCH-POSITIVE . 1)
	    (LIST-SEARCH-NEGATIVE . 1)
	    (SYNTAX-TABLE-DEFINE . 2)
	    (FOR-ALL? . 1)
	    (THERE-EXISTS? . 1)
	    ))
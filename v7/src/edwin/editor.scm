;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/edwin/editor.scm,v 1.209 1991/11/26 08:02:55 cph Exp $
;;;
;;;	Copyright (c) 1986, 1989-91 Massachusetts Institute of Technology
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

;;;; Editor Top Level

(declare (usual-integrations))

(define (edit . args)
  (cond ((not edwin-editor)
	 (apply create-editor args))
	((not (null? args))
	 (error "edwin: Arguments ignored when re-entering editor" args)))
  (call-with-current-continuation
   (lambda (continuation)
     (fluid-let ((editor-abort continuation)
		 (current-editor edwin-editor)
		 (recursive-edit-continuation false)
		 (recursive-edit-level 0))
       (editor-grab-display edwin-editor
	 (lambda (with-editor-ungrabbed operations)
	   (let ((message (cmdl-message/null)))
	     (cmdl/start
	      (push-cmdl
	       (lambda (cmdl)
		 cmdl		;ignore
		 (bind-condition-handler (list condition-type:error)
		     internal-error-handler
		   (lambda ()
		     (top-level-command-reader edwin-initialization)))
		 message)
	       false
	       `((START-CHILD ,(editor-start-child-cmdl with-editor-ungrabbed))
		 ,@operations))
	      message)))))))
  (if edwin-finalization (edwin-finalization))
  unspecific)

(define (edwin . args) (apply edit args))
(define (within-editor?) (not (unassigned? current-editor)))

(define editor-abort)
(define edwin-editor false)
(define current-editor)

;; Set this before entering the editor to get something done after the
;; editor's dynamic environment is initialized, but before the command
;; loop is started.
(define edwin-initialization false)

;; Set this while in the editor to get something done after leaving
;; the editor's dynamic environment; for example, this can be used to
;; reset and then reenter the editor.
(define edwin-finalization false)

(define create-editor-args
  (list false))

(define (create-editor . args)
  (let ((args
	 (if (null? args)
	     create-editor-args
	     (begin
	       (set! create-editor-args args)
	       args))))
    (reset-editor)
    (initialize-typein!)
    (initialize-typeout!)
    (initialize-command-reader!)
    (initialize-processes!)
    (set! edwin-editor
	  (make-editor "Edwin"
		       (let ((name (car args)))
			 (cond (name
				(name->display-type name))
			       ((display-type/available? console-display-type)
				console-display-type)
			       ((display-type/available? x-display-type)
				x-display-type)
			       (else
				(error "can't find usable display type"))))
		       (cdr args)))
    (set! edwin-initialization
	  (lambda ()
	    (set! edwin-initialization false)
	    (standard-editor-initialization)))
    unspecific))

(define (standard-editor-initialization)
  (start-inferior-repl!
   (current-buffer)
   user-initial-environment
   user-initial-syntax-table
   (and (not (ref-variable inhibit-startup-message))
	(cmdl-message/append
	 (cmdl-message/active
	  (lambda (port)
	    (identify-world port)
	    (newline port)
	    (newline port)))
	 (cmdl-message/strings
	  "You are in an interaction window of the Edwin editor."
	  "Type C-h for help.  C-h m will describe some commands."))))
  (with-editor-interrupts-disabled
   (lambda ()
     (if (not init-file-loaded?)
	 (begin
	   (let ((filename (os/init-file-name)))
	     (if (file-exists? filename)
		 (let ((buffer (temporary-buffer " *dummy*")))
		   (with-selected-buffer buffer
		     (lambda ()
		       (load-edwin-file filename '(EDWIN) true)))
		   (kill-buffer buffer))))
	   (set! init-file-loaded? true)
	   unspecific)))))

(define inhibit-editor-init-file? false)
(define init-file-loaded? false)

(define-variable inhibit-startup-message
  "True inhibits the initial startup messages.
This is for use in your personal init file, once you are familiar
with the contents of the startup message."
  false)

(define (reset-editor)
  (without-interrupts
   (lambda ()
     (if edwin-editor
	 (begin
	   (for-each (lambda (screen)
		       (screen-discard! screen))
		     (editor-screens edwin-editor))
	   (set! edwin-editor false)
	   (set! init-file-loaded? false)
	   (set! *previous-popped-up-buffer* (object-hash false))
	   (set! *previous-popped-up-window* (object-hash false))
	   unspecific)))))

(define (reset-editor-windows)
  (for-each (lambda (screen)
	      (send (screen-root-window screen) ':salvage!))
	    (editor-screens edwin-editor)))

;;; There is a problem with recursive edits and multiple screens.
;;; When you switch screens the recursive edit aborts. The problem
;;; is that a top level ^G in a recursive edit aborts the recursive
;;; edit and a ^G is signalled when you switch screens. I think that
;;; ^G should not abort a recursive edit.

(define (enter-recursive-edit)
  (let ((value
	 (call-with-current-continuation
	   (lambda (continuation)
	     (fluid-let ((recursive-edit-continuation continuation)
			 (recursive-edit-level (1+ recursive-edit-level)))
	       (let ((recursive-edit-event!
		      (lambda ()
			(for-each (lambda (window)
				    (window-modeline-event! window
							    'RECURSIVE-EDIT))
				  (window-list)))))
		 (dynamic-wind recursive-edit-event!
			       command-reader
			       recursive-edit-event!)))))))
    (if (eq? value 'ABORT)
	(abort-current-command)
	(begin
	  (reset-command-prompt!)
	  value))))

(define (exit-recursive-edit value)
  (if recursive-edit-continuation
      (recursive-edit-continuation value)
      (editor-error "No recursive edit is in progress")))

(define recursive-edit-continuation)
(define recursive-edit-level)

(define (internal-error-handler condition)
  (cond (debug-internal-errors?
	 (exit-editor-and-signal-error condition))
	((ref-variable debug-on-internal-error)
	 (debug-scheme-error condition "internal"))
	(else
	 (editor-beep)
	 (message (condition/report-string condition))
	 (abort-current-command))))

(define-variable debug-on-internal-error
  "True means enter debugger if error is signalled while the editor is running.
This does not affect editor errors or evaluation errors."
  false)

(define debug-internal-errors? false)

(define (exit-editor-and-signal-error condition)
  (within-continuation editor-abort
    (lambda ()
      (error condition))))

(define condition-type:editor-error
  (make-condition-type 'EDITOR-ERROR condition-type:error '(STRINGS)
    (lambda (condition port)
      (write-string "Editor error: " port)
      (write-string (message-args->string (editor-error-strings condition))
		    port))))

(define editor-error
  (let ((signaller
	 (condition-signaller condition-type:editor-error
			      '(STRINGS)
			      standard-error-handler)))
    (lambda strings
      (signaller strings))))

(define editor-error-strings
  (condition-accessor condition-type:editor-error 'STRINGS))

(define (editor-error-handler condition)
  (if (ref-variable debug-on-editor-error)
      (debug-scheme-error condition "editor")
      (begin
	(editor-beep)
	(let ((strings (editor-error-strings condition)))
	  (if (not (null? strings))
	      (apply message strings)))
	(abort-current-command))))

(define-variable debug-on-editor-error
  "True means signal Scheme error when an editor error occurs."
  false)

(define (%editor-error)
  (editor-beep)
  (abort-current-command))

(define *^G-interrupt-handler*)

(define (^G-signal)
  (*^G-interrupt-handler*))

(define (intercept-^G-interrupts interceptor thunk)
  (let ((signal-tag "signal-tag"))
    (let ((value
	   (call-with-current-continuation
	     (lambda (continuation)
	       (fluid-let ((*^G-interrupt-handler*
			    (lambda () (continuation signal-tag))))
		 (thunk))))))
      (if (eq? value signal-tag)
	  (interceptor)
	  value))))

(define (editor-grab-display editor receiver)
  (display-type/with-display-grabbed (editor-display-type editor)
    (lambda (with-display-ungrabbed operations)
      (with-current-local-bindings!
	(lambda ()
	  (let ((enter
		 (lambda ()
		   (let ((screen (selected-screen)))
		     (screen-enter! screen)
		     (update-screen! screen true))))
		(exit
		 (lambda ()
		   (screen-exit! (selected-screen)))))
	    (dynamic-wind enter
			  (lambda ()
			    (receiver
			     (lambda (thunk)
			       (dynamic-wind exit
					     (lambda ()
					       (with-display-ungrabbed thunk))
					     enter))
			      operations))
			  exit)))))))

(define (editor-start-child-cmdl with-editor-ungrabbed)
  (lambda (cmdl thunk)
    cmdl
    (with-editor-ungrabbed thunk)))
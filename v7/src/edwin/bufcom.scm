;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/edwin/bufcom.scm,v 1.85 1990/10/03 04:54:03 cph Exp $
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

;;;; Buffer Commands

(declare (usual-integrations))

(define-command not-modified
  "Pretend that this buffer hasn't been altered."
  ()
  (lambda ()
    (buffer-not-modified! (current-buffer))))

(define-variable select-buffer-create
  "If true, buffer selection commands may create new buffers."
  true)

(define (prompt-for-select-buffer prompt)
  (lambda ()
    (list
     (buffer-name
      ((if (ref-variable select-buffer-create)
	   prompt-for-buffer
	   prompt-for-existing-buffer)
       prompt
       (previous-buffer))))))

(define-command switch-to-buffer
  "Select buffer with specified name.
If the variable select-buffer-create is true,
specifying a non-existent buffer will cause it to be created."
  (prompt-for-select-buffer "Switch to buffer")
  (lambda (buffer)
    (select-buffer (find-buffer buffer))))

(define-command switch-to-buffer-in-new-screen
  "Select buffer in a new screen."
  (prompt-for-select-buffer "Switch to buffer in new screen")
  (lambda (buffer)
    (select-buffer-in-new-screen (find-buffer buffer))))

(define-command switch-to-buffer-other-window
  "Select buffer in another window."
  (prompt-for-select-buffer "Switch to buffer in other window")
  (lambda (buffer)
    (select-buffer-other-window (find-buffer buffer))))

(define-command create-buffer
  "Create a new buffer with a given name, and select it."
  "sCreate buffer"
  (lambda (name)
    (select-buffer (new-buffer name))))

(define-command create-buffer-in-new-screen
  "Create a new buffer with a given name, and select it in a new screen."
  "sCreate buffer in new screen"
  (lambda (name)
    (select-buffer-in-new-screen (new-buffer name))))

(define-command insert-buffer
  "Insert the contents of a specified buffer at point."
  "bInsert buffer"
  (lambda (buffer)
    (let ((point (mark-right-inserting (current-point))))
      (region-insert-string!
       point
       (region->string (buffer-region (find-buffer buffer))))
      (push-current-mark! (current-point))
      (set-current-point! point))))

(define-command twiddle-buffers
  "Select previous buffer."
  ()
  (lambda ()
    (let ((buffer (previous-buffer)))
      (if buffer
	  (select-buffer buffer)
	  (editor-error "No previous buffer to select")))))

(define-command bury-buffer
  "Put current buffer at the end of the list of all buffers.
There it is the least likely candidate for other-buffer to return;
thus, the least likely buffer for \\[switch-to-buffer] to select by default."
  ()
  (lambda ()
    (let ((buffer (current-buffer))
	  (previous (previous-buffer)))
      (if previous
	  (begin
	    (select-buffer previous)
	    (bury-buffer buffer))))))

(define-command kill-buffer
  "One arg, a string or a buffer.  Get rid of the specified buffer."
  "bKill buffer"
  (lambda (buffer)
    (kill-buffer-interactive (find-buffer buffer))))

(define (kill-buffer-interactive buffer)
  (if (not (other-buffer buffer)) (editor-error "Only one buffer"))
  (save-buffer-changes buffer)
  (kill-buffer buffer))

(define-command kill-some-buffers
  "For each buffer, ask whether to kill it."
  ()
  (lambda ()
    (kill-some-buffers true)))

(define (kill-some-buffers prompt?)
  (for-each (lambda (buffer)
	      (if (and (not (minibuffer? buffer))
		       (or (not prompt?)
			   (prompt-for-confirmation?
			    (string-append "Kill buffer '"
					   (buffer-name buffer)
					   "'"))))
		  (if (other-buffer buffer)
		      (kill-buffer-interactive buffer)
		      (let ((dummy (new-buffer "*Dummy*")))
			(kill-buffer-interactive buffer)
			(create-buffer initial-buffer-name)
			(kill-buffer dummy)))))
	    (buffer-list)))

(define-command rename-buffer
  "Change the name of the current buffer.
Reads the new name in the echo area."
  "sRename buffer (to new name)"
  (lambda (name)
    (if (find-buffer name)
	(editor-error "Buffer named " name " already exists"))
    (rename-buffer (current-buffer) name)))

(define-command normal-mode
  "Reset mode and local variable bindings to their default values.
Just like what happens when the file is first visited."
  ()
  (lambda ()
    (initialize-buffer! (current-buffer))))

(define (save-buffer-changes buffer)
  (if (and (buffer-pathname buffer)
	   (buffer-modified? buffer)
	   (buffer-writeable? buffer)
	   (prompt-for-yes-or-no?
	    (string-append "Buffer "
			   (buffer-name buffer)
			   " contains changes.  Write them out")))
      (write-buffer-interactive buffer)))

(define (new-buffer name)
  (create-buffer
   (if (find-buffer name)
       (let search-loop ((n 2))
	 (let ((new-name (string-append name "<" (write-to-string n) ">")))
	   (if (find-buffer new-name)
	       (search-loop (1+ n))
	       new-name)))
       name)))

(define (string->temporary-buffer string name)
  (let ((buffer (temporary-buffer name)))
    (insert-string string (buffer-point buffer))
    (set-buffer-point! buffer (buffer-start buffer))
    (buffer-not-modified! buffer)
    (pop-up-buffer buffer false)))

(define (with-output-to-temporary-buffer name thunk)
  (let ((buffer (temporary-buffer name)))
    (with-output-to-mark (buffer-point buffer) thunk)
    (set-buffer-point! buffer (buffer-start buffer))
    (buffer-not-modified! buffer)
    (pop-up-buffer buffer false)))

(define (temporary-buffer name)
  (let ((buffer (find-or-create-buffer name)))
    (buffer-reset! buffer)
    buffer))

(define (prompt-for-buffer prompt default-buffer)
  (let ((name (prompt-for-buffer-name prompt default-buffer false)))
    (or (find-buffer name)
	(let ((buffer (create-buffer name)))
	  (temporary-message "(New Buffer)")
	  buffer))))

(define (prompt-for-existing-buffer prompt default-buffer)
  (find-buffer (prompt-for-buffer-name prompt default-buffer true)))

(define (prompt-for-buffer-name prompt default-buffer require-match?)
  (prompt-for-string-table-name prompt
				(and default-buffer
				     (buffer-name default-buffer))
				(if default-buffer
				    'VISIBLE-DEFAULT
				    'NO-DEFAULT)
				(buffer-names)
				require-match?))
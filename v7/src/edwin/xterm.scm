;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/edwin/xterm.scm,v 1.16 1991/03/16 08:13:31 cph Exp $
;;;
;;;	Copyright (c) 1989-91 Massachusetts Institute of Technology
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

;;;; X Terminal

(declare (usual-integrations))

(define-primitives
  (x-open-display 1)
  (x-close-all-displays 0)
  (x-close-display 1)
  (x-close-window 1)
  (x-display-flush 1)
  (x-display-process-events 2)
  (x-display-sync 2)
  (x-window-beep 1)
  (x-window-display 1)
  (x-window-set-event-mask 2)
  (x-window-set-icon-name 2)
  (x-window-set-name 2)
  (xterm-clear-rectangle! 6)
  (xterm-draw-cursor 1)
  (xterm-enable-cursor 2)
  (xterm-erase-cursor 1)
  (xterm-open-window 3)
  (xterm-restore-contents 6)
  (xterm-save-contents 5)
  (xterm-scroll-lines-down 6)
  (xterm-scroll-lines-up 6)
  (xterm-set-size 3)
  (xterm-write-char! 5)
  (xterm-write-cursor! 3)
  (xterm-write-substring! 7)
  (xterm-x-size 1)
  (xterm-y-size 1))

(define-structure (xterm-screen-state
		   (constructor make-xterm-screen-state (xterm display))
		   (conc-name xterm-screen-state/))
  (xterm false read-only true)
  (display false read-only true)
  (redisplay-flag true)
  (selected? true))

(define screen-list)

(define (make-xterm-screen #!optional geometry)
  (let ((screen
	 (let ((xterm
		(xterm-open-window (or (get-x-display)
				       (error "unable to open display"))
				   (and (not (default-object? geometry))
					geometry)
				   false)))
	   (x-window-set-event-mask xterm event-mask)
	   (make-screen (make-xterm-screen-state xterm
						 (x-window-display xterm))
			xterm-screen/beep
			xterm-screen/clear-line!
			xterm-screen/clear-rectangle!
			xterm-screen/clear-screen!
			xterm-screen/discard!
			xterm-screen/enter!
			xterm-screen/exit!
			xterm-screen/flush!
			xterm-screen/modeline-event!
			false
			xterm-screen/scroll-lines-down!
			xterm-screen/scroll-lines-up!
			xterm-screen/wrap-update!
			xterm-screen/write-char!
			xterm-screen/write-cursor!
			xterm-screen/write-substring!
			8
			(xterm-x-size xterm)
			(xterm-y-size xterm)))))
    (set! screen-list (cons screen screen-list))
    screen))

(define-integrable (screen-xterm screen)
  (xterm-screen-state/xterm (screen-state screen)))

(define-integrable (screen-display screen)
  (xterm-screen-state/display (screen-state screen)))

(define-integrable (screen-redisplay-flag screen)
  (xterm-screen-state/redisplay-flag (screen-state screen)))

(define-integrable (set-screen-redisplay-flag! screen flag)
  (set-xterm-screen-state/redisplay-flag! (screen-state screen) flag))

(define-integrable (screen-selected? screen)
  (xterm-screen-state/selected? (screen-state screen)))

(define-integrable (set-screen-selected?! screen selected?)
  (set-xterm-screen-state/selected?! (screen-state screen) selected?))

(define (xterm->screen xterm)
  (let loop ((screens screen-list))
    (and (not (null? screens))
	 (if (eqv? xterm (screen-xterm (car screens)))
	     (car screens)
	     (loop (cdr screens))))))

(define (xterm-screen/wrap-update! screen thunk)
  (dynamic-wind
   (lambda ()
     (xterm-enable-cursor (screen-xterm screen) false))
   thunk
   (lambda ()
     (if (screen-selected? screen)
	 (let ((xterm (screen-xterm screen)))
	   (xterm-enable-cursor xterm true)
	   (xterm-draw-cursor xterm)))
     (if (screen-redisplay-flag screen)
	 (begin
	   (update-xterm-screen-names! screen)
	   (set-screen-redisplay-flag! screen false)))
     (xterm-screen/flush! screen))))

(define (xterm-screen/discard! screen)
  (set! screen-list (delq! screen screen-list))
  (x-close-window (screen-xterm screen)))

(define (xterm-screen/modeline-event! screen window type)
  window type				; ignored
  (set-screen-redisplay-flag! screen true))

(define (xterm-screen/enter! screen)
  (set-screen-selected?! screen true)
  (let ((xterm (screen-xterm screen)))
    (xterm-enable-cursor xterm true)
    (xterm-draw-cursor xterm))
  (xterm-screen/flush! screen))

(define (xterm-screen/exit! screen)
  (set-screen-selected?! screen false)
  (let ((xterm (screen-xterm screen)))
    (xterm-enable-cursor xterm false)
    (xterm-erase-cursor xterm))
  (xterm-screen/flush! screen))

(define (xterm-screen/scroll-lines-down! screen xl xu yl yu amount)
  (xterm-scroll-lines-down (screen-xterm screen) xl xu yl yu amount)
  'UNCHANGED)

(define (xterm-screen/scroll-lines-up! screen xl xu yl yu amount)
  (xterm-scroll-lines-up (screen-xterm screen) xl xu yl yu amount)
  'UNCHANGED)

(define (xterm-screen/beep screen)
  (x-window-beep (screen-xterm screen))
  (xterm-screen/flush! screen))

(define-integrable (xterm-screen/flush! screen)
  (x-display-flush (screen-display screen)))

(define (xterm-screen/write-char! screen x y char highlight)
  (xterm-write-char! (screen-xterm screen) x y char (if highlight 1 0)))

(define (xterm-screen/write-cursor! screen x y)
  (xterm-write-cursor! (screen-xterm screen) x y))

(define (xterm-screen/write-substring! screen x y string start end highlight)
  (xterm-write-substring! (screen-xterm screen) x y string start end
			  (if highlight 1 0)))

(define (xterm-screen/clear-line! screen x y first-unused-x)
  (xterm-clear-rectangle! (screen-xterm screen)
			  x first-unused-x y (fix:1+ y) 0))

(define (xterm-screen/clear-rectangle! screen xl xu yl yu highlight)
  (xterm-clear-rectangle! (screen-xterm screen)
			  xl xu yl yu (if highlight 1 0)))

(define (xterm-screen/clear-screen! screen)
  (xterm-clear-rectangle! (screen-xterm screen)
			  0 (screen-x-size screen) 0 (screen-y-size screen) 0))

;;;; Event Handling

(define (get-xterm-input-operations screen)
  (let ((display (screen-display screen))
	(string false)
	(start 0)
	(end 0)
	(pending-event false))
    (let ((get-next-event
	   (lambda (time-limit)
	     (if pending-event
		 (let ((event pending-event))
		   (set! pending-event false)
		   event)
		 (x-display-process-events display time-limit))))
	  (process-key-press-event
	   (lambda (event)
	     (set! string (vector-ref event 2))
	     (set! start 0)
	     (set! end (string-length string))
	     (if signal-interrupts?
		 (let ((i (string-find-previous-char string #\BEL)))
		   (if i
		       (begin
			 (set! start (fix:+ i 1))
			 (signal-interrupt!)))))))
	  (process-special-event
	   (lambda (event)
	     (let ((handler (vector-ref event-handlers (vector-ref event 0)))
		   (screen (xterm->screen (vector-ref event 1))))
	       (if (and handler screen)
		   (handler screen event))))))
      (let ((guarantee-input
	     (lambda ()
	       (let loop ()
		 (let ((event (get-next-event false)))
		   (cond ((not event)
			  (error "#F returned from blocking read"))
			 ((eq? true event)
			  false)
			 ((eq? event-type:key-press (vector-ref event 0))
			  (process-key-press-event event)
			  (if (fix:< start end) true (loop)))
			 (else
			  (process-special-event event)
			  (loop))))))))
	(values
	 (lambda ()			;halt-update?
	   (if (or (fix:< start end) pending-event)
	       true
	       (let ((event (get-next-event 0)))
		 (and event
		      (begin
			(set! pending-event event)
			true)))))
	 (lambda ()			;char-ready?
	   (if (fix:< start end)
	       true
	       (let loop ()
		 (let ((event (get-next-event 0)))
		   (cond ((or (not event) (eq? true event))
			  false)
			 ((eq? event-type:key-press (vector-ref event 0))
			  (process-key-press-event event)
			  (if (fix:< start end) true (loop)))
			 (else
			  (process-special-event event)
			  (loop)))))))
	 (lambda ()			;peek-char
	   (and (or (fix:< start end) (guarantee-input))
		(string-ref string start)))
	 (lambda ()			;read-char
	   (and (or (fix:< start end) (guarantee-input))
		(let ((char (string-ref string start)))
		  (set! start (fix:+ start 1))
		  char))))))))

;;; The values of these flags must be equal to the corresponding event
;;; types in "microcode/x11base.c"

(define-integrable event-type:button-down 0)
(define-integrable event-type:button-up 1)
(define-integrable event-type:configure 2)
(define-integrable event-type:enter 3)		
(define-integrable event-type:focus-in 4)	
(define-integrable event-type:focus-out 5)		
(define-integrable event-type:key-press 6)		
(define-integrable event-type:leave 7)		
(define-integrable event-type:motion 8) 
(define-integrable number-of-event-types 9)

;; This mask contains button-down, button-up, configure, focus-in, and
;; key-press.
(define-integrable event-mask #x057)

(define event-handlers
  (make-vector number-of-event-types false))

(define-integrable (define-event-handler event-type handler)
  (vector-set! event-handlers event-type handler))

(define-event-handler event-type:configure
  (lambda (screen event)
    (let ((x-size (vector-ref event 2))
	  (y-size (vector-ref event 3)))
      (if (not (and (= x-size (screen-x-size screen))
		    (= y-size (screen-y-size screen))))
	  (begin
	    (set-screen-size! screen x-size y-size)
	    (update-screen! screen true))))))

(define-event-handler event-type:button-down
  (lambda (screen event)
    (send (screen-root-window screen) ':button-event!
	  (make-down-button (vector-ref event 4))
	  (vector-ref event 2)
	  (vector-ref event 3))
    (update-screen! screen false)))

(define-event-handler event-type:button-up
  (lambda (screen event)
    (send (screen-root-window screen) ':button-event!
	  (make-up-button (vector-ref event 4))
	  (vector-ref event 2)
	  (vector-ref event 3))
    (update-screen! screen false)))

(define-event-handler event-type:focus-in
  (lambda (screen event)
    event
    (if (not (selected-screen? screen))
	(command-reader/reset-and-execute
	 (lambda ()
	   (select-screen screen))))))

(define signal-interrupts?)
(define pending-interrupt?)

(define (signal-interrupt!)
  (editor-beep)
  (temporary-message "Quit")
  (set! pending-interrupt? false)
  (^G-signal))

(define (with-editor-interrupts-from-x receiver)
  (fluid-let ((signal-interrupts? true)
	      (pending-interrupt? false))
    (receiver (lambda (thunk) (thunk)))))

(define (with-x-interrupts-enabled thunk)
  (bind-signal-interrupts? true thunk))

(define (with-x-interrupts-disabled thunk)
  (bind-signal-interrupts? false thunk))

(define (bind-signal-interrupts? new-mask thunk)
  (let ((old-mask))
    (dynamic-wind (lambda ()
		    (set! old-mask signal-interrupts?)
		    (set! signal-interrupts? new-mask)
		    (if (and new-mask pending-interrupt?)
			(signal-interrupt!)))
		  thunk
		  (lambda ()
		    (set! new-mask signal-interrupts?)
		    (set! signal-interrupts? old-mask)
		    (if (and old-mask pending-interrupt?)
			(signal-interrupt!))))))

(define x-display-type)
(define x-display-data)

(define (get-x-display)
  ;; X-OPEN-DISPLAY hangs, uninterruptibly, when the X server is
  ;; running the login loop of xdm.  Can this be fixed?
  (or x-display-data
      (let ((display (x-open-display false)))
	(set! x-display-data display)
	display)))

(define (initialize-package!)
  (set! screen-list '())
  (set! x-display-type
	(make-display-type 'X
			   true
			   get-x-display
			   make-xterm-screen
			   get-xterm-input-operations
			   with-editor-interrupts-from-x
			   with-x-interrupts-enabled
			   with-x-interrupts-disabled))
  (set! x-display-data false)
  unspecific)
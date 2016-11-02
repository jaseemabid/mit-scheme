;;;; -*-Scheme-*-

;;;; Delete VALUES expanders.

(let ((env (->environment '(scode-optimizer expansion))))
  (for-each
    (lambda (name)
      (let ((entry (assq name (access usual-integrations/expansion-alist env))))
	(if (pair? entry)
	    (begin
	      (set! (access usual-integrations/expansion-names env)
		    (delq! name
			   (access usual-integrations/expansion-names env)))
	      (set! (access usual-integrations/expansion-values env)
		    (delq! (cdr entry)
			   (access usual-integrations/expansion-values env)))
	      (set! (access usual-integrations/expansion-alist env)
		    (delq! entry
			   (access usual-integrations/expansion-alist env)))))))
    '(CALL-WITH-VALUES VALUES WITH-VALUES)))

;;;; Keep debugging info.

;;; This allows the cross-compiler to be debugged even after its
;;; binaries have been deleted.

(define (patch-scode-optimizer)
  (eval
   '(define original-compile-scode/internal compile-scode/internal)
   (->environment '(compiler top-level)))
  (eval
   '(define (compile-scode/internal scode #!optional info-output-pathname
				    rtl-output-port lap-output-port wrapper)
      (original-compile-scode/internal scode 'keep
				       rtl-output-port lap-output-port wrapper))
   (->environment '(compiler top-level))))

(define (32bit-words?)
  (= 4 (vector-ref (gc-space-status) 0)))

(if (and (find-package '(COMPILER TOP-LEVEL) #f)
	 (not (32bit-words?)))
    (patch-scode-optimizer))
#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/rtlgen/rtlgen.scm,v 1.10 1987/04/18 00:26:35 cph Exp $

Copyright (c) 1987 Massachusetts Institute of Technology

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

;;;; RTL Generation

(declare (usual-integrations))

(define *nodes*)

(define (generate-rtl quotations procedures)
  (with-new-node-marks
   (lambda ()
     (fluid-let ((*nodes* '()))
       (for-each (lambda (quotation)
		   (set-quotation-rtl-entry!
		    quotation
		    (generate:top-level (quotation-fg-entry quotation))))
		 quotations)
       (for-each generate:procedure procedures)
       (for-each (lambda (rnode)
		   (node-property-remove! rnode generate:node))
		 *nodes*)))))

(define-integrable (generate:top-level expression)
  (generate:node expression 0 false))

(define (generate:subproblem subproblem offset rest-generator)
  (let ((cfg (subproblem-cfg subproblem)))
    (if (cfg-null? cfg)
	(and rest-generator (rest-generator offset))
	(generate:node (cfg-entry-node cfg) offset rest-generator))))

(define (generate:next node offset rest-generator)
  (cond ((not node) (and rest-generator (rest-generator offset)))
	((node-marked? node)
	 (let ((memo (node-property-get node generate:node)))
	   (if (not (= (car memo) offset))
	       (error "Node entered at different offsets" node))
	   (cdr memo)))
	(else (generate:node node offset rest-generator))))

(define (generate:node node offset rest-generator)
  (node-mark! node)
  (let ((cfg ((vector-method node generate:node) node offset rest-generator)))
    (node-property-put! node generate:node (cons offset cfg))
    (set! *nodes* (cons node *nodes*))
    cfg))

(define-integrable (generate:next-is-null? next rest-generator)
  (and (not next) (not rest-generator)))

(define (generate:procedure procedure)
  (set-procedure-rtl-entry!
   procedure
   (let ((body (generate:top-level (procedure-fg-entry procedure))))
     (if (procedure/ic? procedure)
	 body
	 (scfg*node->node!
	  (scfg*scfg->scfg!
	   ((if (or (procedure-rest procedure)
		    (and (procedure/closure? procedure)
			 (not (null? (procedure-optional procedure)))))
		rtl:make-setup-lexpr
		rtl:make-procedure-heap-check)
	    procedure)
	   (setup-stack-frame procedure))
	  body)))))

(define (setup-stack-frame procedure)
  (let ((block (procedure-block procedure)))
    (define (cellify-variables variables)
      (scfg*->scfg! (map cellify-variable variables)))

    (define (cellify-variable variable)
      (if (variable-in-cell? variable)
	  (let ((locative
		 (stack-locative-offset (rtl:make-fetch register:stack-pointer)
					(variable-offset block variable))))
	    (rtl:make-assignment
	     locative
	     (rtl:make-cell-cons (rtl:make-fetch locative))))
	  (make-null-cfg)))

    (let ((names (procedure-names procedure))
	  (values (procedure-values procedure)))
      (scfg-append! (setup-bindings names values '())
		    (setup-auxiliary (procedure-auxiliary procedure) '())
		    (cellify-variables (procedure-required procedure))
		    (cellify-variables (procedure-optional procedure))
		    (let ((rest (procedure-rest procedure)))
		      (if rest
			  (cellify-variable rest)
			  (make-null-cfg)))
		    (scfg*->scfg!
		     (map (lambda (name value)
			    (if (and (procedure? value)
				     (procedure/closure? value))
				(letrec-close block name value)
				(make-null-cfg)))
			  names values))))))

(define (setup-bindings names values pushes)
  (if (null? names)
      (scfg*->scfg! pushes)
      (setup-bindings (cdr names)
		      (cdr values)
		      (cons (make-auxiliary-push (car names)
						 (letrec-value (car values)))
			    pushes))))

(define (letrec-value value)
  (cond ((constant? value)
	 (rtl:make-constant (constant-value value)))
	((procedure? value)
	 (case (procedure/type value)
	   ((CLOSURE)
	    (make-closure-cons value (rtl:make-constant '())))
	   ((IC)
	    (make-ic-cons value))
	   ((OPEN-EXTERNAL OPEN-INTERNAL)
	    (error "Letrec value is open procedure" value))
	   (else
	    (error "Unknown procedure type" value))))
	(else
	 (error "Unknown letrec binding value" value))))

(define (letrec-close block variable value)
  (make-closure-environment value 0 scfg*scfg->scfg!
    (lambda (environment)
      (rtl:make-assignment
       (closure-procedure-environment-locative
	(find-variable block variable 0
	  (lambda (locative) locative)
	  (lambda (nearest-ic-locative name)
	    (error "Missing closure variable" variable))))
       environment))))

(define (setup-auxiliary variables pushes)
  (if (null? variables)
      (scfg*->scfg! pushes)
      (setup-auxiliary (cdr variables)
		       (cons (make-auxiliary-push (car variables)
						  (rtl:make-unassigned))
			     pushes))))

(define (make-auxiliary-push variable value)
  (rtl:make-push (if (variable-in-cell? variable)
		     (rtl:make-cell-cons value)
		     value)))

;;;; Statements

(define (define-generator tag generator)
  (define-vector-method tag generate:node generator))

(define-generator definition-tag
  (lambda (definition offset rest-generator)
    (scfg*node->node!
     (rvalue->sexpression (definition-rvalue definition) offset
       (lambda (expression)
	 (find-variable (definition-block definition)
			(definition-lvalue definition)
			offset
	   (lambda (locative)
	     (error "Definition of compiled variable"))
	   (lambda (environment name)
	     (rtl:make-interpreter-call:define environment name expression)))))
     (generate:next (snode-next definition) offset rest-generator))))

(define-generator assignment-tag
  (lambda (assignment offset rest-generator)
    (generate-assignment (assignment-block assignment)
			 (assignment-lvalue assignment)
			 (assignment-rvalue assignment)
			 (snode-next assignment)
			 offset
			 rest-generator
			 rvalue->sexpression)))

(define (generate-assignment block lvalue rvalue next offset rest-generator
			     rvalue->sexpression)
  ((vector-method lvalue generate-assignment)
   block lvalue rvalue next offset rest-generator rvalue->sexpression))

(define (define-assignment tag generator)
  (define-vector-method tag generate-assignment generator))

(define-assignment variable-tag
  (lambda (block variable rvalue next offset rest-generator
		 rvalue->sexpression)
    (scfg*node->node! (if (integrated-vnode? variable)
			  (make-null-cfg)
			  (rvalue->sexpression rvalue offset
			    (lambda (expression)
			      (find-variable block variable offset
				(lambda (locative)
				  (rtl:make-assignment locative expression))
				(lambda (environment name)
				  (rtl:make-interpreter-call:set!
				   environment
				   (intern-scode-variable! block name)
				   expression))))))
		      (generate:next next offset rest-generator))))

(define-assignment temporary-tag
  (lambda (block temporary rvalue next offset rest-generator
		 rvalue->sexpression)
    (case (temporary-type temporary)
      ((#F)
       (scfg*node->node!
	(if (integrated-vnode? temporary)
	    (make-null-cfg)
	    (rvalue->sexpression rvalue offset
	      (lambda (expression)
		(rtl:make-assignment temporary expression))))
	(generate:next next offset rest-generator)))
      ((VALUE)
       (assignment:value-register block rvalue next offset
				  rest-generator rvalue->sexpression))
      (else
       (error "Unknown temporary type" temporary)))))

(define (assignment:value-register block rvalue next offset
				   rest-generator rvalue->sexpression)
  (if (not (generate:next-is-null? next rest-generator))
      (error "Return node has next"))
  (scfg*node->node!
   (scfg*scfg->scfg! (if (value-temporary? rvalue)
			 (make-null-cfg)
			 (rvalue->sexpression rvalue offset
			   (lambda (expression)
			     (rtl:make-assignment register:value expression))))
		     (if (stack-block? block)
			 (if (stack-parent? block)
			     (rtl:make-message-sender:value
			      (+ offset (block-frame-size block)))
			     (scfg*scfg->scfg!
			      (rtl:make-pop-frame (block-frame-size block))
			      (rtl:make-return)))
			 (rtl:make-return)))
   (generate:next next offset rest-generator)))

(define-assignment value-ignore-tag
  (lambda (block value-ignore rvalue next offset rest-generator
		 rvalue->sexpression)
    (if (not (generate:next-is-null? next rest-generator))
	(error "Return node has next"))
    (generate:next next offset rest-generator)))

;;;; Predicates

(define (define-predicate-generator tag node-generator)
  (define-generator tag
    (lambda (pnode offset rest-generator)
      (generate:predicate pnode offset rest-generator
			  (node-generator pnode offset)))))

(define (generate:predicate pnode offset rest-generator pcfg)
  (pcfg*node->node!
   pcfg
   (generate:next (pnode-consequent pnode) offset rest-generator)
   (generate:next (pnode-alternative pnode) offset rest-generator)))

(define-predicate-generator true-test-tag
  (lambda (test offset)
    (let ((rvalue (true-test-rvalue test)))
      (if (rvalue-known-constant? rvalue)
	  (constant->pcfg (rvalue-constant-value rvalue))
	  (rvalue->pexpression rvalue offset rtl:make-true-test)))))

(define-predicate-generator unassigned-test-tag
  (lambda (test offset)
    (find-variable (unassigned-test-block test)
		   (unassigned-test-variable test)
		   offset
      (lambda (locative)
	(rtl:make-unassigned-test (rtl:make-fetch locative)))
      (lambda (environment name)
	(scfg*pcfg->pcfg!
	 (rtl:make-interpreter-call:unassigned? environment name)
	 (rtl:make-true-test (rtl:interpreter-call-result:unassigned?)))))))

(define-predicate-generator unbound-test-tag
  (lambda (test offset)
    (let ((variable (unbound-test-variable test)))
      (if (ic-block? (variable-block variable))
	  (scfg*pcfg->pcfg!
	   (rtl:make-interpreter-call:unbound?
	    (nearest-ic-block-expression (unbound-test-block test) offset)
	    (variable-name variable))
	   (rtl:make-true-test (rtl:interpreter-call-result:unbound?)))
	  (make-false-pcfg)))))

;;;; Expressions

(define (rvalue->sexpression rvalue offset receiver)
  (rvalue->expression rvalue offset scfg*scfg->scfg! receiver))

(define (rvalue->pexpression rvalue offset receiver)
  (rvalue->expression rvalue offset scfg*pcfg->pcfg! receiver))

(define (rvalue->expression rvalue offset scfg-append! receiver)
  ((vector-method rvalue rvalue->expression)
   rvalue offset scfg-append! receiver))

(define (define-rvalue->expression tag generator)
  (define-vector-method tag rvalue->expression generator))

(define (constant->expression constant offset scfg-append! receiver)
  (receiver (rtl:make-constant (constant-value constant))))

(define-rvalue->expression constant-tag constant->expression)

(define-rvalue->expression block-tag
  (lambda (block offset scfg-append! receiver)
    (receiver (rtl:make-fetch register:environment))))

(define-rvalue->expression reference-tag
  (lambda (reference offset scfg-append! receiver)
    (reference->expression (reference-block reference)
			   (reference-variable reference)
			   offset
			   scfg-append!
			   receiver)))

(define (reference->expression block variable offset scfg-append! receiver)
  (if (vnode-known-constant? variable)
      (constant->expression (vnode-known-value variable) offset scfg-append!
			    receiver)
      (find-variable block variable offset
	(lambda (locative)
	  (receiver (rtl:make-fetch locative)))
	(lambda (environment name)
	  (scfg-append! (rtl:make-interpreter-call:lookup
			 environment
			 (intern-scode-variable! block name))
			(receiver (rtl:interpreter-call-result:lookup)))))))

(define-rvalue->expression temporary-tag
  (lambda (temporary offset scfg-append! receiver)
    (if (vnode-known-constant? temporary)
	(constant->expression (vnode-known-value temporary) offset scfg-append!
			      receiver)
	(let ((type (temporary-type temporary)))
	  (cond ((not type) (receiver (rtl:make-fetch temporary)))
		((eq? type 'VALUE) (receiver (rtl:make-fetch register:value)))
		(else (error "Illegal temporary reference" type)))))))

(define-rvalue->expression access-tag
  (lambda (*access offset scfg-append! receiver)
    (rvalue->expression (access-environment *access) offset scfg-append!
      (lambda (expression)
	(scfg-append! (rtl:make-interpreter-call:access expression
							(access-name *access))
		      (receiver (rtl:interpreter-call-result:access)))))))

(define-rvalue->expression procedure-tag
  (lambda (procedure offset scfg-append! receiver)
    (case (procedure/type procedure)
      ((CLOSURE)
       (make-closure-environment procedure offset scfg-append!
	 (lambda (environment)
	   (receiver (make-closure-cons procedure environment)))))
      ((IC)
       (receiver (make-ic-cons procedure)))
      ((OPEN-EXTERNAL OPEN-INTERNAL)
       (error "Reference to open procedure" procedure))
      (else
       (error "Unknown procedure type" procedure)))))

(define (make-ic-cons procedure)
  ;; IC procedures have their entry points linked into their headers
  ;; at load time by the linker.
  (let ((header
	 (scode/make-lambda (variable-name (procedure-name procedure))
			    (map variable-name (procedure-required procedure))
			    (map variable-name (procedure-optional procedure))
			    (let ((rest (procedure-rest procedure)))
			      (and rest (variable-name rest)))
			    (map variable-name
				 (append (procedure-auxiliary procedure)
					 (procedure-names procedure)))
			    '()
			    false)))
    (set! *ic-procedure-headers*
	  (cons (cons procedure header)
		*ic-procedure-headers*))
    (rtl:make-typed-cons:pair
     (rtl:make-constant (scode/procedure-type-code header))
     (rtl:make-constant header)
     ;; Is this right if the procedure is being closed
     ;; inside another IC procedure?
     (rtl:make-fetch register:environment))))

(define (make-closure-environment procedure offset scfg-append! receiver)
  (let ((block (block-parent (procedure-block procedure))))
    (define (ic-locative closure-block block offset)
      (let ((loser
	     (lambda (locative)
	       (error "Closure parent not IC block"))))
	(find-block closure-block block offset loser loser
	  (lambda (locative nearest-ic-locative) locative))))
    (cond ((not block)
	   (receiver (rtl:make-constant false)))
	  ((ic-block? block)
	   (receiver
	    (let ((closure-block (procedure-closure-block procedure)))
	      (if (ic-block? closure-block)
		  (rtl:make-fetch register:environment)
		  (ic-locative closure-block block offset)))))
	  ((closure-block? block)
	   (let ((closure-block (procedure-closure-block procedure)))
	     (define (loop variables n)
	       (cond ((null? variables)
		      (return-3 offset n '()))
		     ((integrated-vnode? (car variables))
		      (loop (cdr variables) n))
		     (else 
		      (transmit-values (loop (cdr variables) (1+ n))
			(lambda (offset n pushes)
			  (return-3
			   (1+ offset)
			   n
			   (cons (rtl:make-push
				  (rtl:make-fetch
				   (find-closure-variable closure-block
							  (car variables)
							  offset)))
				 pushes)))))))

	     (define (make-frame n pushes)
	       (scfg-append! (scfg*->scfg!
			      (reverse!
			       (cons (rtl:make-interpreter-call:enclose n)
				     pushes)))
			     (receiver (rtl:interpreter-call-result:enclose))))

	     (transmit-values (loop (block-bound-variables block) 0)
	       (lambda (offset n pushes)
		 (let ((parent (block-parent block)))
		   (if parent
		       (make-frame (1+ n)
				   (cons (rtl:make-push
					  (ic-locative closure-block parent
						       offset))
					 pushes))
		       (make-frame n pushes)))))))
	  (else (error "Unknown block type" block)))))

(define (make-closure-cons procedure environment)
  (rtl:make-typed-cons:pair (rtl:make-constant type-code:compiled-procedure)
			    (rtl:make-entry:procedure procedure)
  "node rtl arguments")
;;;; -*-Scheme-*-
;;; $Id: scc-macros.scm,v 1.3.2.1 2002/02/02 03:58:36 cph Exp $

(define-syntax define-constant
  define-integrable)

(define-syntax define-in-line
  define-integrable)

(define-integrable *running-in-mit-scheme* #t)
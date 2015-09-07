;#lang scheme/load
;(require (planet neil/sicp))

(load "core.scm")

(load "environment.scm")

(define (install-variable-eval)
  
  (define lookup-variable-value ((make-environment)
                                 'lookup))
  
  (define (eval exp env)
    (lookup-variable-value exp env))
  
  (put eval 'eval variable-keyword)
  '(variable eval installed))
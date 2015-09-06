;#lang scheme/load
;(require (planet neil/sicp))

(load "core.scm")

(load "environment.scm")

(define (install-variable-eval)
  (define (eval exp env)
    (let ([value (lookup-variable-value exp env)])
      value))
  (put eval 'eval '**variable**)
  '(variable eval installed))
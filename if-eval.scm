;#lang scheme/load
;(require (planet neil/sicp))

(load "core.scm")
(load "if.scm")

;对判断语句的处理
(define (install-if-eval)
  
  (define dispatch (make-if))
  
  (define (true? exp)
    ((dispatch 'true?) exp))
  
  (define (predicate exp)
    ((dispatch 'predicate) exp))
  
  (define (consequent exp)
    ((dispatch 'consequent) exp))
  
  (define (alternative exp)
    ((dispatch 'alternative) exp))
  
  (define (eval exp env)
    (if (true? (interp (predicate exp) env))
        (interp (consequent exp) env)
        (interp (alternative exp) env)))
  
  (put eval 'eval 'if)
  '(if eval installed))
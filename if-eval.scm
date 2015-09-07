;#lang scheme/load
;(require (planet neil/sicp))

(load "core.scm")
(load "if.scm")

;对判断语句的处理
(define (install-if-eval)
  
  (let ([if-dispatch (make-if)])
    
    (define true? (if-dispatch 'true?))
    
    (define predicate (if-dispatch 'predicate))
    
    (define consequent (if-dispatch 'consequent))
    
    (define alternative (if-dispatch 'alternative))
    
    (define (eval exp env)
      (if (true? (interp (predicate exp) env))
          (interp (consequent exp) env)
          (interp (alternative exp) env)))
    
    (put eval eval-proc-key 'if)
    '(if eval installed)))
;#lang scheme/load
;(require (planet neil/sicp))

(load "core.scm")

(load "begin.scm")

;对begind的处理
(define (install-begin-eval)
  
  (let ([begin-dispatch (make-begin)])
    
    (define actions (begin-dispatch 'actions))
    
    (define (eval exp env)
      (interp-sequence (actions exp)
                       env))
    
    (put eval 'eval 'begin)
    '(begin eval installed)))



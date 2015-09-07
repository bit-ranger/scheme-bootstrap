;#lang scheme/load
;(require (planet neil/sicp))

(load "core.scm")
(load "environment.scm")
(load "definition.scm")


;对定义的处理
(define (install-definition-eval)
  
  (let ([define-dispatch (make-define)]
        [env-dispatch (make-environment)])
    
    (define variable
      (define-dispatch 'variable))
    
    (define value
      (define-dispatch 'value))
    
    (define define-variable! (env-dispatch 'def))
    
    (define (eval exp env)
      (define-variable! (variable exp)
                        (interp (value exp) env)
                        env)
      'ok)
    
    (put eval 'eval 'define)
    '(define eval installed)))



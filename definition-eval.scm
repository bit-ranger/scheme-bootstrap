;#lang scheme/load
;(require (planet neil/sicp))

(load "core.scm")
(load "environment.scm")
(load "definition.scm")


;对定义的处理
(define (install-definition-eval)
  
  (define dispatch (make-define))

  (define (variable exp)
    ((dispatch 'variable) exp))

  (define (value exp)
    ((dispatch 'value) exp))
  
  (define (eval exp env)
    (define-variable! (variable exp)
                      (interp (value exp) env)
                      env)
    'ok)
  
  (put eval 'eval 'define)
  '(define eval installed))



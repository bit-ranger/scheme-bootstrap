;#lang scheme/load
;(require (planet neil/sicp))

(load "core.scm")

(load "begin.scm")

;对begind的处理
(define (install-begin-eval)

  (define dispatch (make-begin))

  (define (eval exp env)
    (interp-sequence ((dispatch 'actions) exp)
                   env))
  
  (put eval 'eval 'begin)
  '(begin eval installed))



;#lang scheme/load
;(require (planet neil/sicp))

(load "core.scm")
(load "environment.scm")


;对赋值的处理办法
(define (install-assignment-eval)
  ;取出变量名
  (define (variable exp)
    (cadr exp))

  ;值的表达式
  (define (value exp)
    (caddr exp))

  (define set-variable-value! ((make-environment) 'set))
  
  (define (eval exp env)
    (set-variable-value! (variable exp)
                         (interp (value exp) env)
                         env)
    'ok)
  
  (put eval eval-proc-key 'set!)
  '(assignment eval installed))
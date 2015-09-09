;#lang scheme/load
;(require (planet neil/sicp))

(load "core.scm")
(load "analyze.scm")
(load "environment.scm")
(load "assignment.scm")


;对赋值的处理办法
(define (install-assignment-eval)
  
  (let ([assign-dispatch (make-assignment)])
    
    (define set-variable-value! ((make-environment) 'set))
    
    (define variable (assign-dispatch 'variable))
    
    (define value (assign-dispatch 'value))
    
    (define (eval exp env)
      (set-variable-value! (variable exp)
                           (interp (value exp) env)
                           env)
      'ok)
    
    (define (observe exp)
      (let ([var (variable exp)]
            [proc (analyze (value exp))])
        (lambda (env)
          (set-variable-value! var
                               (proc env)
                               env)
          'ok)))
    
    (put eval eval-proc-key 'set!)
    (put observe observe-proc-key 'set!)
    '(assignment eval installed)))
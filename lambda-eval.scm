;#lang scheme/load
;(require (planet neil/sicp))

(load "core.scm")

(load "procedure.scm")

(load "lambda.scm")


;对lambda的处理
(define (install-lambda-eval)
  
  (let ([lambda-dispatch (make-lambda)]
        [procedure-dispatch (make-procedure)])
    
    (define parameters (lambda-dispatch 'parameters))
    
    (define body (lambda-dispatch 'body))
    
    (define new-procedure (procedure-dispatch 'construct))
    
    (define (eval exp env)
      (new-procedure (parameters exp)
                     (body exp)
                     env))
    
    (put eval eval-proc-key 'lambda)
    (put eval eval-proc-key 'λ)
    '(install lambda done)))
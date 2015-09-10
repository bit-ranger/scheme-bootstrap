;#lang scheme/load
;(require (planet neil/sicp))

(load "core.scm")
(load "analyze.scm")
(load "procedure.scm")
(load "proc-transform.scm")
(load "lambda.scm")


;对lambda的处理
(define (install-lambda-eval)
  
  (let ([lambda-dispatch (make-lambda)]
        [procedure-dispatch (make-procedure)]
        [trans-dispatch (make-proc-transform)])
    
    
    (define parameters (lambda-dispatch 'parameters))
    
    (define (body exp)
      ((trans-dispatch 'trans-body) ((lambda-dispatch 'body) exp)))
    
    (define new-procedure (procedure-dispatch 'construct))
    
    (define (eval exp env)
      (new-procedure (parameters exp)
                     (body exp)
                     env))
    
    (define (observe exp)
      (let ([vars (parameters exp)]
            [proc (analyze-sequence (body exp))])
        (lambda (env)
          (new-procedure vars proc env))))
    
    (put eval eval-proc-key 'lambda)
    (put eval eval-proc-key 'λ)
    (put observe observe-proc-key 'lambda)
    (put observe observe-proc-key 'λ)
    '(lambda eval installed)))
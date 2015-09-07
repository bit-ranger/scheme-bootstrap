;#lang scheme/load
;(require (planet neil/sicp))

(load "core.scm")
(load "lambda.scm")
(load "application.scm")
(load "let.scm")

;对let处理
(define (install-let-eval)
  
  (define new-lambda ((make-lambda) 'construct))
  
  (define new-application ((make-application) 'construct))
  
  (let ([let-dispatch (make-let)])
    
    (define body (let-dispatch 'body))
    
    (define binds (let-dispatch 'binds))
    
    (define parameters (let-dispatch 'parameters))
    
    (define values (let-dispatch 'values))
    
    
    (define (eval exp env)
      (let ([binding (binds exp)])
        (let ([new-exp (new-application (new-lambda (parameters binding)
                                                    (body exp))
                                        (values binding))
                       ])
          (interp new-exp env))))
    
    
    (put eval eval-proc-key 'let)
    '(let eval installed)))


(load "eval/core.scm")
(load "eval/analyze.scm")
(load "syntax/lambda.scm")
(load "syntax/application.scm")
(load "syntax/let.scm")

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
    
    (define (observe exp)
      (let ([binding (binds exp)])
        (let ([new-exp (new-application (new-lambda (parameters binding)
                                                    (body exp))
                                        (values binding))
                       ])
          (analyze new-exp))))
    
    (put eval eval-proc-key 'let)
    (put observe observe-proc-key 'let)
    '(let eval installed)))


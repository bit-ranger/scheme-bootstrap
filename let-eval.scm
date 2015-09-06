;#lang scheme/load
;(require (planet neil/sicp))

(load "core.scm")
(load "lambda.scm")
(load "definition.scm")
(load "application.scm")
(load "let.scm")

;对let处理
(define (install-let-eval)
  
  (define new-lambda ((make-lambda) 'construct))
  
  (define let-dispatch (make-let))
   
  (defien body (let-dispatch 'body))
  
  (define binds (let-dispatch 'binds))
  
  (define (parameters binds)
    (if (null? binds)
        binds
        (cons (caar binds)
              (parameters (cdr binds)))))
  
  (define (values binds)
    (if (null? binds)
        binds
        (cons (cadar binds)
              (values (cdr binds)))))
  
  
  (define (eval exp env)
    (let ([binding (binds exp)])
      (let ([new-exp (make-application
                      (new-lambda (parameters binding)
                                  (body exp))
                      (values binding))
                     ])
        (interp new-exp env))))
  
  
  (put eval 'eval 'let)
  '(let eval installed))


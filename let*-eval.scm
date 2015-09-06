;#lang scheme/load
;(require (planet neil/sicp))

(load "core.scm")
(load "let.scm")

;对let*处理
(define (install-let*-eval)
  
  
  (define (binds exp)
    (cdr exp))
  
  (define (body exp)
    (cddr exp))
  
  (define new-let ((make-let) 'construct))
  
  ;转成嵌套的let
  (define (let*->lets binds body)
    (if (null? binds)
        body
        (new-let (list (car binds))
                 (let*->lets (cdr binds)))))
  
  (define (eval exp env)
    (let ([new-exp (let*->lets (binds exp)
                               (body exp))])
      (interp new-exp env)))
  
  
  (put eval 'eval 'let*)
  '(let* eval installed))

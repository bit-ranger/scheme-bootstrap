;#lang scheme/load
;(require (planet neil/sicp))

(load "core.scm")
(load "let.scm")

;对let*处理
(define (install-let*-eval)
  
  
  (define (binds exp)
    (cadr exp))
  
  (define (body exp)
    (cddr exp))
  
  (define (new-binds k-v)
    (list k-v))
  
  (define (new-body seq)
    (list seq))
  
  (define new-let ((make-let) 'construct))
  
  ;转成嵌套的let
  (define (let*->lets binds body)
    (define (iter binds)
      (if (null? binds)
          body
          (new-body (new-let (new-binds (car binds))
                             (iter (cdr binds))))))
    (car (iter binds)))
  
  (define (eval exp env)
    (let ([new-exp (let*->lets (binds exp)
                               (body exp))])
      (interp new-exp env)))
  
  
  (put eval eval-proc-key 'let*)
  '(let* eval installed))



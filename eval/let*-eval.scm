(load "eval/core.scm")
(load "eval/analyze.scm")
(load "syntax/let.scm")

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
    (let ((new-exp (let*->lets (binds exp)
                               (body exp))))
      (interp new-exp env)))
  
  (define (observe exp)
    (let ((new-exp (let*->lets (binds exp)
                               (body exp))))
      (analyze new-exp)))
  
  (put eval eval-proc-key 'let*)
  (put observe observe-proc-key 'let*)
  '(let* eval installed))

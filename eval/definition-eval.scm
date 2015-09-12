(load "eval/core.scm")
(load "eval/analyze.scm")
(load "eval/environment.scm")
(load "syntax/definition.scm")


;对定义的处理
(define (install-definition-eval)
  
  (let ((define-dispatch (make-define))
        (env-dispatch (make-environment)))
    
    (define variable
      (define-dispatch 'variable))
    
    (define value
      (define-dispatch 'value))
    
    (define define-variable! (env-dispatch 'def))
    
    (define (eval exp env)
      (define-variable! (variable exp)
                        (interp (value exp) env)
                        env)
      'ok)

    (define (observe exp)
      (let ((var (variable exp))
            (proc (analyze (value exp))))
        (lambda (env)
          (define-variable! var (proc env) env)
          'ok)))
    
    (put eval eval-proc-key 'define)
    (put observe observe-proc-key 'define)
    '(define eval installed)))



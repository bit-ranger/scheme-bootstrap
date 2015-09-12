(load "eval/core.scm")
(load "eval/analyze.scm")
(load "syntax/thunk.scm")

(define (install-delay&force-eval)
  
  (let ((thunk-dispatch (make-thunk)))
    
    (define new-thunk (thunk-dispatch 'construct))
    (define thunk? (thunk-dispatch 'thunk?))
    (define thunk-body (thunk-dispatch 'body))
    (define thunk-env (thunk-dispatch 'env))
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    (define (delay-body exp)
      (cadr exp))
    
    (define (delay-eval exp env)
      (new-thunk (delay-body exp) env))
    
    (define (delay-observe exp)
      (let ((proc (analyze (delay-body exp))))
        (lambda (env)
          (new-thunk proc env))))   ;闭包一个经过静态分析的过程，而非表达式
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    (define (force-body exp)
      (cadr exp))
    
    (define (force-eval exp env)
      (let ((thunk (interp (force-body exp) env)))
        (if (thunk? thunk)
            (interp (thunk-body thunk) (thunk-env thunk))
            (error "Only can force delayed thunk -- FORCE-EVAL"
                   thunk))))
    
    (define (force-observe exp)
      (let ((proc (analyze (force-body exp))))
        (lambda (env)
          (let ((thunk (proc env)))
            (if (thunk? thunk)
                ((thunk-body thunk) env)
                (error "Only can force delayed thunk -- FORCE-OBSERVE"
                       thunk))))))
    
    
    (put delay-eval eval-proc-key 'delay)
    (put delay-observe observe-proc-key 'delay)
    (put force-eval eval-proc-key 'force)
    (put force-observe observe-proc-key 'force)
    '(delay&force eval installed)))


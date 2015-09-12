(load "eval/core.scm")
(load "eval/analyze.scm")
(load "syntax/if.scm")

;对判断语句的处理
(define (install-if-eval)
  
  (let ([if-dispatch (make-if)])
    
    (define true? (if-dispatch 'true?))
    
    (define predicate (if-dispatch 'predicate))
    
    (define consequent (if-dispatch 'consequent))
    
    (define alternative (if-dispatch 'alternative))
    
    (define (eval exp env)
      (if (true? (interp (predicate exp) env))
          (interp (consequent exp) env)
          (interp (alternative exp) env)))

    (define (observe exp)
      (let ([pproc (analyze (predicate exp))]
            [cproc (analyze (consequent exp))]
            [aproc (analyze (alternative exp))])
        (lambda (env)
          (if (true? (pproc env))
              (cproc env)
              (aproc env)))))
    
    (put eval eval-proc-key 'if)
    (put observe observe-proc-key 'if)
    '(if eval installed)))

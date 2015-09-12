(load "eval/core.scm")
(load "eval/analyze.scm")
(load "eval/procedure.scm")
(load "eval/environment.scm")
(load "syntax/application.scm")


(define (install-application-eval)
  
  (let ([application-dispatch (make-application)]
        [procedure-dispatch (make-procedure)]
        [environment-dispatch (make-environment)])
    
    ;表达式操作部分
    (define operator (application-dispatch 'operator))
    
    ;操作数
    (define operands (application-dispatch 'operands))
    
    ;没有操作数?
    (define (no-operands? ops)
      (null? ops))
    
    ;第一个操作数
    (define (first-operand ops)
      (car ops))
    
    ;剩余的操作数
    (define (rest-operands ops)
      (cdr ops))
    
    (define primitive-procedure?
      (procedure-dispatch 'primitive?))
    
    (define apply-primitive-procedure
      (procedure-dispatch 'apply-primitive))
    
    (define compound-procedure?
      (procedure-dispatch 'compound?))
    
    (define procedure-parameters
      (procedure-dispatch 'parameters))
    
    (define procedure-body
      (procedure-dispatch 'body))
    
    (define procedure-environment
      (procedure-dispatch 'environment))
    
    (define extend-environment
      (environment-dispatch 'extend))
    
    ;求参数列表的值
    ;若参数列表总某个参数的值为null,则解释器会判断参数列表结束，
    ;因此，此处需要特殊处理，每个参数都用一个cons存放。
    (define (list-of-values exps env)
      (if (no-operands? exps)
          exps
          (cons (let ([value (interp (first-operand exps) env)])
                  value)
                (list-of-values (rest-operands exps) env))))
    
    
    (define (adhibition procedure arguments)
      (cond [(primitive-procedure? procedure)
             (apply-primitive-procedure procedure arguments)]
            [(compound-procedure? procedure)
             (interp-sequence (procedure-body procedure)
                              (extend-environment (procedure-parameters procedure)
                                                  arguments
                                                  (procedure-environment procedure)))]
            [else (error "Unknown procedure -- ADHIBITION"
                         procedure
                         arguments)]))
    
    
    (define (eval exp env)
      (adhibition (interp (operator exp) env)
                  (list-of-values (operands exp) env)))
    
    (define (execute procedure arguments)
      (cond [(primitive-procedure? procedure)
             (apply-primitive-procedure procedure arguments)]
            [(compound-procedure? procedure)
             ((procedure-body procedure)
              (extend-environment (procedure-parameters procedure)
                                  arguments
                                  (procedure-environment procedure)))]
            [else (error "Unknown procedure -- EXECUTE"
                         procedure
                         arguments)]))
    
    
    (define (observe exp)
      (let ([fproc (analyze (operator exp))]
            [aprocs (map analyze (operands exp))])
        (lambda (env)
          (execute (fproc env)
                   (map (lambda (aproc)
                          (aproc env))
                        aprocs)))))
    
    
    (put eval eval-proc-key application-keyword)
    (put observe observe-proc-key application-keyword)
    '(application eval installed)))

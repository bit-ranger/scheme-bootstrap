;#lang scheme/load
;(require (planet neil/sicp))

(load "core.scm")
(load "procedure.scm")
(load "environment.scm")
(load "application.scm")


(define (install-application-eval)

  (define dispatch (make-application))
  
  ;表达式操作部分
  (define (operator exp)
    ((dispatch 'operator) exp))

  ;操作数
  (define (operands exp)
    ((dispatch 'operands) exp))

  ;没有操作数?
  (define (no-operands? ops)
    (null? ops))

  ;第一个操作数
  (define (first-operand ops)
    (car ops))

  ;剩余的操作数
  (define (rest-operands ops)
    (cdr ops))

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


  (put  eval 'eval '**application**)
  '(application eval installed))
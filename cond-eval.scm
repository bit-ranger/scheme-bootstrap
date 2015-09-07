;#lang scheme/load
;(require (planet neil/sicp))

(load "core.scm")
(load "if.scm")
(load "begin.scm")
(load "application.scm")

;对cond的处理
(define (install-cond-eval)
  
  ;cond语句序列
  (define (cond-clauses exp)
    (cdr exp))
  
  ;是否为else子句？
  (define (cond-else-clause? clause)
    (eq? (cond-predicate clause) 'else))
  
  ;是否为指向型cond?
  (define (cond-point-clause? clause)
    (eq? (cadr clause) '=>))
  
  ;子句的谓词
  (define (cond-predicate clause)
    (car clause))
  
  ;子句的推论
  (define (cond-actions clause)
    (if (cond-point-clause? clause)
        (cddr clause)
        (cdr clause)))
  
  ;cond转成if
  (define (cond->if exp)
    (expand-clauses (cond-clauses exp)))
  
  (define new-if ((make-if) 'construct))
  (define new-begin ((make-begin) 'construct))
  (define new-application ((make-application) 'construct))
  
  ;展开cond语句序列
  (define (expand-clauses clauses)
    (if (null? clauses)
        'false
        (let ([first (car clauses)]
              [rest  (cdr clauses)])
          (if (cond-else-clause? first)
              (if (null? rest)
                  (sequence->exp (cond-actions first))
                  (error "ELSE clause is not last -- COND->IF"
                         clauses))
              (new-if (cond-predicate first)
                      (resolve-clause first)
                      (expand-clauses rest))))))
  
  (define (resolve-clause clause)
    (if (cond-point-clause? clause)
        ;如果是一个指向型的cond子句，则将action的结果应用到predicate。
        (new-application (sequence->exp (cond-actions clause)) (cond-predicate clause))
        (sequence->exp (cond-actions clause))))
  
  ;序列转表达式
  (define (sequence->exp seq)
    (cond [(null? seq) seq]
          [(last-exp? seq) (first-exp seq)]
          [else (new-begin seq)]))                            ;if的每个分支都是一个语句，而不是语句序列
  
  (define (eval exp env)
    (interp (cond->if exp) env))
  
  (put eval eval-proc-key 'cond)
  '(cond eval installed))

;#lang scheme/load
;(require (planet neil/sicp))

(load "core.scm")
(load "if.scm")


;对or的处理
(define (install-or-eval)
  
  (define new-if ((make-if) 'construct))
  
  ;and语句序列
  (define (or-clauses exp)
    (cdr exp))
  
  ;or转成if
  (define (or->if exp)
    (let ([value (expand-clauses (or-clauses exp))])
      (display value)
      value))
  
  ;展开or语句序列
  (define (expand-clauses clauses)
    (if (null? clauses)
        'false
        (let ([first (car clauses)]
              [rest  (cdr clauses)])
          (new-if first
                  'true
                  (expand-clauses rest)))))
  
  (define (eval exp env)
    (interp (or->if exp) env))
  
  (put eval eval-proc-key 'or)
  (put eval eval-proc-key '||)
  '(or eval installed))

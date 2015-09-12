(load "eval/core.scm")
(load "eval/analyze.scm")
(load "syntax/if.scm")

;对and的处理
(define (install-and-eval)
  
  (define new-if ((make-if) 'construct))
  
  ;and语句序列
  (define (and-clauses exp)
    (cdr exp))
  
  ;and转成if
  (define (and->if exp)
    (expand-clauses (and-clauses exp)))
  
  ;展开and语句序列
  (define (expand-clauses clauses)
    (if (null? clauses)
        'true
        (let ([first (car clauses)]
              [rest  (cdr clauses)])
          (new-if first
                  (expand-clauses rest)
                  'false))))
  
  (define (eval exp env)
    (interp (and->if exp) env))
  
  (define (observe exp)
    (analyze (and->if exp)))
  
  (put eval eval-proc-key 'and)
  (put eval eval-proc-key '&&)
  (put observe observe-proc-key 'and)
  (put observe observe-proc-key '&&)
  '(and eval installed))





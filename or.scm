;#lang scheme

(require (planet neil/sicp))

(require "interp-core.ss")

(require "interp-if.ss")

(provide (all-defined-out))


;对or的处理
(define (install-or-package)
 
  ;and语句序列
  (define (or-clauses exp)
    (cdr exp))

  ;and转成if
  (define (or->if exp)
    (expand-clauses (or-clauses exp)))

  ;展开and语句序列
  (define (expand-clauses clauses)
    (if (null? clauses)
        'false
        (let ([first (car clauses)]
              [rest  (cdr clauses)])
          (make-if first
                   'true
                   (expand-clauses rest)))))
  
  (define (eval exp env)
    (interp (or->if exp) env))

  (put eval 'eval 'or)
  '(install or done))

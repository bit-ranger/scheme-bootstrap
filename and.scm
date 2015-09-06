;#lang scheme

(require (planet neil/sicp))

(require "interp-core.ss")

(require "interp-if.ss")

(provide (all-defined-out))


;对and的处理
(define (install-and-package)
 
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
          (make-if first
                   (expand-clauses rest)
                   'false))))
  
  (define (eval exp env)
    (interp (and->if exp) env))

  (put eval 'eval 'and)
  '(install and done))





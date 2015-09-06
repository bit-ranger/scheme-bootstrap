;#lang scheme
;(require (planet neil/sicp))

;判断exp是否以tag开头
(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

;创建过程
(define (make-procedure parameters body env)
  (list 'procedure parameters body env))

;是否为普通过程
(define (compound-procedure? p)
  (tagged-list? p 'procedure))

;过程参数列表
(define (procedure-parameters p)
  (cadr p))

;过程体
(define (procedure-body p)
  (caddr p))

;过程环境
(define (procedure-environment p)
  (cadddr p))

;是否为基本过程
(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

;基本过程的实现
(define (primitive-implementation proc)
  (cadr proc))

;基本过程里列表
(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)))

;基本过程名列表
(define (primitive-procedure-names)
  (map car
       primitive-procedures))

;基本过程实现列表
(define (primitive-procedure-objects)
  (map (lambda (proc)
         (list 'primitive (cadr proc)))
       primitive-procedures))

;这些基本过程需要由解释器执行
(define (apply-primitive-procedure proc args)
  (let ([p (primitive-implementation proc)])
    (apply p args)))
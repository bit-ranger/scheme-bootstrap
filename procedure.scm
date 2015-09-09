;#lang scheme
;(require (planet neil/sicp))

(define (make-procedure)
  ;判断exp是否以tag开头
  (define (tagged-list? exp tag)
    (if (pair? exp)
        (eq? (car exp) tag)
        false))
  
  ;创建过程
  ;parameters是一个列表
  ;body是一个列表
  ;env是一个环境
  (define (construct parameters body env)
    (list 'procedure parameters body env))
  
  ;是否为普通过程
  (define (compound? p)
    (tagged-list? p 'procedure))
  
  ;过程参数列表
  (define (parameters p)
    (cadr p))
  
  ;过程体
  (define (body p)
    (caddr p))
  
  
  ;过程环境
  (define (environment p)
    (cadddr p))
  
  ;是否为基本过程
  (define (primitive? proc)
    (tagged-list? proc 'primitive))
  
  ;基本过程的实现
  (define (primitive-implementation proc)
    (cadr proc))
  
  ;基本过程列表
  (define primitive-procedures
    (list (list 'car car)
          (list 'cdr cdr)
          (list 'cons cons)
          (list 'null? null?)
          (list 'eq? eq?)
          (list '+ +)
          (list '- -)
          (list '* *)
          (list '/ /)
          (list '= =)))
  
  ;基本过程名列表
  (define (primitive-names)
    (map car
         primitive-procedures))
  
  ;基本过程对象列表
  (define (primitive-objects)
    (map (lambda (proc)
           (list 'primitive (cadr proc)))
         primitive-procedures))
  
  ;这些基本过程需要由解释器执行
  (define (apply-primitive proc args)
    (let ([p (primitive-implementation proc)])
      (apply p args)))
  
  (define (dispatch m)
    (cond [(eq? m 'construct) construct]
          [(eq? m 'compound?) compound?]
          [(eq? m 'parameters) parameters]
          [(eq? m 'body) body]
          [(eq? m 'environment) environment]
          [(eq? m 'primitive?) primitive?]
          [(eq? m 'apply-primitive) apply-primitive]
          [(eq? m 'primitive-names) (primitive-names)]
          [(eq? m 'primitive-objects) (primitive-objects)]
          [else (error "Unknown operator" m)]))
  dispatch)


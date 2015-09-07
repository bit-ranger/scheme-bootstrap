;#lang scheme

(load "lambda.scm")


(define (make-define)
  
  (define new-lambda ((make-lambda) 'construct))
  
  ;变量名
  (define (variable exp)
    (if (symbol? (cadr exp))
        (cadr exp)
        (caadr exp)))
  ;值
  (define (value exp)
    (if (symbol? (cadr exp))
        (caddr exp)
        (new-lambda (cdadr exp)
                    (cddr exp))))
  ;构造器
  (define (construct var param)
    (list 'define var param))

  ;是否为定义语句
  (define (define? exp)
    (and (pair? exp)
         (eq? 'define (car exp))))
  
  (define (dispatch m)
    (cond [(eq? m 'variable) variable]
          [(eq? m 'value) value]
          [(eq? m 'construct) construct]
          [(eq? m 'define?) define?]
          [else (error "Unknown operator" m)]))
  
  dispatch)




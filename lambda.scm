;#lang scheme

(define (make-lambda)
  
  ;构造lambda
  ;parameters是一个列表
  ;body是一个列表
  (define (construct parameters body)
    (cons 'lambda (cons parameters body)))
  
  ;lambda 参数列表
  (define (parameters exp)
    (cadr exp))
  
  ;lambda过程体
  (define (body exp)
    (cddr exp))
  
  (define (dispatch m)
    (cond [(eq? m 'parameters) parameters]
          [(eq? m 'body) body]
          [(eq? m 'construct) construct]
          [else (error "Unknown operator" m)]))
  
  dispatch)

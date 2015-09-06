;#lang scheme

(load "lambda.scm")


(define (make-define)

  (define lambda-diapatch (make-lambda))
  
  ;变量名
  (define (variable exp)
    (if (symbol? (cadr exp))
        (cadr exp)
        (caadr exp)))
  ;值
  (define (value exp)
    (if (symbol? (cadr exp))
        (caddr exp)
        ((lambda-diapatch 'construct)
         (cdadr exp)
         (cddr exp))))
  ;构造器
  (define (construct var param)
    (list 'define var param))
  
  (define (dispatch m)
    (cond [(eq? m 'variable) variable]
          [(eq? m 'value) value]
          [(eq? m 'construct) construct]
          [else (error "Unknown operator" m)]))

  dispatch)




;#lang scheme

;处理调用
;需要注意，operator与operands需要用cons连接，而不是list
(define (make-application)
  
  ;表达式操作部分
  (define (operator exp)
    (car exp))
  
  ;操作数
  (define (operands exp)
    (cdr exp))
  
  (define (construct proc params)
    (cons proc params))
  
  (define (dispatch m)
    (cond [(eq? 'operator m) operator]
          [(eq? 'operands m) operands]
          [(eq? 'construct m) construct]
          [else (error "Unknown operator" m)]))
  dispatch)


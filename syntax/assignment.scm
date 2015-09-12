(define (make-assignment)
  ;取出变量名
  (define (variable exp)
    (cadr exp))
  
  ;值的表达式
  (define (value exp)
    (caddr exp))
  
  ;新的赋值语句
  (define (construct var value)
    (list 'set! var value))
  
  (define (dispatch m)
    (cond ((eq? m 'variable) variable)
          ((eq? m 'value) value)
          ((eq? m 'construct) construct)
          (else (error "Unknown operator" m))))
  
  dispatch)
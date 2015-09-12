(define (make-if)
  
  ;if 谓词
  (define (predicate exp)
    (cadr exp))
  
  ;if 推论
  (define (consequent exp)
    (caddr exp))
  
  ;if 替代(可或缺)
  (define (alternative exp)
    (if (not (null? (cdddr exp)))
        (cadddr exp)
        'false)) ;若缺少，则返回'false，注意'false带引号，此处只是在操作文本。
  
  (define (true? x)
    (not (false? x)))
  
  (define (false? x)
    ;除非是字面量类型，否则解释器得到的结果应该是解释器语言中的值。
    ;这需要解释器提供一些基本变量或过程。
    (eq? x false))
  
  ;构造if
  ;三个参数都必须是单独一条语句
  (define (construct predicate consequent alternative)
    (list 'if predicate consequent alternative))
  
  (define (dispatch m)
    (cond [(eq? m 'predicate) predicate]
          [(eq? m 'consequent) consequent]
          [(eq? m 'alternative) alternative]
          [(eq? m 'true?) true?]
          [(eq? m 'construct) construct]
          [else (error "Unknown operator" m)]))
  
  dispatch)

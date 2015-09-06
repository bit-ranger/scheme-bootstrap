;#lang scheme
;(require (planet neil/sicp))

(define (make-if)
  
  ;if 谓词
  (define (predicate exp)
    (cadr exp))
  
  ;if 推论
  (define (consequent exp)
    (let ([value (caddr exp)])
      value))
  
  ;if 替代(可或缺)
  (define (alternative exp)
    (if (not (null? (cdddr exp)))
        (cadddr exp)
        'false)) ;若缺少，则返回'false，注意'false带引号，此处只是在操作文本。
  
  (define (true? x)
    (let ([value (not (false? x))])
      value))
  
  (define (false? x)
    (let ([value (eq? x false)]) ;除非是字面量类型，否则解释器得到的结果应该是解释器语言中的值。这需要解释器提供一些基本变量或过程。
      value))
  
  ;构造if
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

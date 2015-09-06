;#lang scheme

(define (make-begin)
;构造begin
  (define (construct seq)
    (cons 'begin seq))

  ;操作序列
  (define (actions exp)
    (cdr exp))

  (define (dispatch m)
    (cond [(eq? 'construct m) construct]
          [(eq? 'actions m) actions]
          [else (error "Unknown operator" m)]))
  dispatch)


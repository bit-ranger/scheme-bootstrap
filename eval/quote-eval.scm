(load "eval/core.scm")
(load "eval/analyze.scm")

;对引号的处理
(define (install-quote-eval)
  ;符号内容
  (define (text-of-quotation exp)
    (cadr exp))
  
  (define (eval exp env)
    (text-of-quotation exp))

  (define (observe exp)
    (let ([val (text-of-quotation exp)])
      (lambda (env) val)))
  
  (put eval eval-proc-key 'quote)
  (put observe observe-proc-key 'quote)
  '(quote eval installed))

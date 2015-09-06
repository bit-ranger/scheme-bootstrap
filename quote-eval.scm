;#lang scheme/load
;(require (planet neil/sicp))

(load "core.scm")

;对引号的处理
(define (install-quote-eval)
  ;符号内容
  (define (text-of-quotation exp)
    (cadr exp))
  
  (define (eval exp env)
    (text-of-quotation exp))
  
  (put eval 'eval 'quote)
  '(quote eval installed))

;#lang scheme/load
;(require (planet neil/sicp))

(load "types.scm")

(define (interp exp env)
  (cond [(self-evaluating? exp) exp]                                                  ; 字面量
        [else (interp-generic (wrap exp) env)]))

;封装语法类型
(define (wrap exp)
  (if (pair? exp)
      (attach-tag (car exp) exp)
      (attach-tag exp exp)))

;动态转发解释过程
(define (interp-generic tagged-exp env)
  (let ([type (type-tag tagged-exp)]
        [exp (contents tagged-exp)])
    (let ([proc (get 'eval type)])
      (if proc
          (proc exp env)
          (if (or (eq? type '**variable**)
                  (eq? type '**application**))
              (error "Unknown expression type -- INTERP" exp)
              (if (variable? exp)
                  (interp-generic (attach-tag '**variable** exp) env)
                  (interp-generic (attach-tag '**application** exp) env)))))))

;处理表达式序列
(define (interp-sequence exps env)
  (cond [(last-exp? exps) (interp (first-exp exps) env)]
        [else (interp (first-exp exps) env)
              (interp-sequence (rest-exps exps) env)]))

;是否字面量？
(define (self-evaluating? exp)
  (cond [(number? exp) true]
        [(string? exp) true]
        [else false]))

;是否变量？
(define (variable? exp)
  (symbol? exp))

;是否最后一个？
(define (last-exp? seq)
  (null? (cdr seq)))

;取序列第一个
(define (first-exp seq)
  (car seq))

;剩余的序列
(define (rest-exps seq)
  (cdr seq))
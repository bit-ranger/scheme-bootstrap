;#lang scheme/load
;(require (planet neil/sicp))

(load "core.scm")

(define observe-proc-key "analyze")

(define (analyze exp)
  (cond [(self-evaluating? exp) (lambda (env) exp)]                                                  ; 字面量
        [else (analyze-generic (wrap exp))]))

;动态转发解释过程
(define (analyze-generic tagged-exp)
  (let ([type (type-tag tagged-exp)]
        [exp (contents tagged-exp)])
    (let ([proc (get observe-proc-key type)])
      (if proc
          (proc exp)
          (if (or (eq? type variable-keyword)
                  (eq? type application-keyword))
              (error "Unknown expression type -- INTERP" exp)
              (cond [(variable? exp)
                     (analyze-generic (attach-tag variable-keyword exp))] 
                    [(application? exp)
                     (analyze-generic (attach-tag application-keyword exp))]
                    [else (error "Unknown expression type -- INTERP" exp)]))))))

;处理表达式序列
(define (analyze-sequence exps)
  ;连通两个函数的调用
  (define (sequentially proc1 proc2)
    (lambda (env) (proc1 env) (proc2 env)))
  (define (loop first-proc rest-procs)
    (if (null? rest-procs)
        first-proc
        (loop (sequentially first-proc (car rest-procs))
              (cdr rest-procs))))
  (let ([procs (map analyze exps)])
    (if (null? procs)
        (error "Empty sequence -- ANALYZE")
        'false)
    (loop (car procs) (cdr procs))))




























(load "syntax/definition.scm")
(load "syntax/letrec.scm")

(define (make-proc-transform)
  
  (let ([define-dispatch (make-define)]
        [letrec-dispatch (make-letrec)])
    
    (define new-letrec (letrec-dispatch 'construct))
    
    (define define? (define-dispatch 'define?))
    
    (define variable (define-dispatch 'variable))
    
    (define value (define-dispatch 'value))
    
    (define bind (letrec-dispatch 'bind))
    
    ;扫描出所有的内部定义语句
    (define (scan-out-defines seqs)
      (if (null? seqs)
          seqs
          (let ([seq (car seqs)])
            (if (define? seq)
                (cons seq
                      (scan-out-defines (cdr seqs)))
                (scan-out-defines (cdr seqs))))))
    
    ;扫描出所有不是内部定义的语句
    (define (scan-except-defines seqs)
      (if (null? seqs)
          seqs
          (let ([seq (car seqs)])
            (if (define? seq)
                (scan-except-defines (cdr seqs))
                (cons seq
                      (scan-except-defines (cdr seqs)))))))
    
    ;创建新的键值绑定表
    (define (new-binds seqs)
      (if (null? seqs)
          seqs
          (cons (bind (variable (car seqs))
                      (value (car seqs)))
                (new-binds (cdr seqs)))))
    
    (define (trans-body body-origin)
      (let* ([defs (scan-out-defines body-origin)]                ;所有定义语句
             [undefs (scan-except-defines body-origin)])          ;所有非定义语句
        (if (null? defs)
            undefs
            (let ([letrec-exp (new-letrec (new-binds defs)        ;转换成letrec语句
                                          undefs)])
              (list letrec-exp)))))
    
    (define (dispatch m)
      (cond [(eq? m 'trans-body) trans-body]
            [else (error "Unknown operator" m)]))
    
    dispatch))

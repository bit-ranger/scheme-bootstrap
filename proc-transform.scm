;#lang scheme

(load "definition.scm")
(load "let.scm")
(load "assignment.scm")

(define (make-proc-transform)
  
  (let ([define-dispatch (make-define)]
        [let-dispatch (make-let)]
        [assign-dispatch (make-assignment)])
    
    
    ;新的let语句
    (define new-let (let-dispatch 'construct))
    
    ;新的赋值语句
    (define new-assignment (assign-dispatch 'construct))
    
    (define define? (define-dispatch 'define?))
    
    (define variable (define-dispatch 'variable))
    
    (define value (define-dispatch 'value))
    
    (define bind (let-dispatch 'bind))
    
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
                      ''**unassigned**)                          ;此处用符号，而不用字面量
                (new-binds (cdr seqs)))))                                        ;若使用字面量，解释器将认为这是一个变量
    ;实现错误提示时将无法获知其真正绑定的变量名
    
    ;创建新的键值设置表
    (define (new-sets seqs)
      (if (null? seqs)
          seqs
          (cons (new-assignment (variable (car seqs))
                                (value (car seqs)))
                (new-sets (cdr seqs)))))
    
    
    ;过程体
    (define (trans-body body-origin)
      (let* ([defs (scan-out-defines body-origin)]
             [undefs (scan-except-defines body-origin)])
        (if (null? defs)
            undefs
            (let ([let-exp (new-let (new-binds defs)
                                    (append (new-sets defs)
                                            undefs))])
              (list let-exp)))))
    
    (define (dispatch m)
      (cond [(eq? m 'trans-body) trans-body]
            [else (error "Unknown operator" m)]))
    
    dispatch))
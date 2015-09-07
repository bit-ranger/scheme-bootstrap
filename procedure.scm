;#lang scheme
;(require (planet neil/sicp))

(load "definition.scm")
(load "let.scm")
(load "assignment.scm")

(define (make-procedure)
  (let ([define-dispatch (make-define)]
        [let-dispatch (make-let)]
        [assign-dispatch (make-assignment)])
    
    ;判断exp是否以tag开头
    (define (tagged-list? exp tag)
      (if (pair? exp)
          (eq? (car exp) tag)
          false))
    
    ;创建过程
    ;parameters是一个列表
    ;body是一个列表
    ;env是一个环境
    (define (construct parameters body env)
      (list 'procedure parameters body env))
    
    ;是否为普通过程
    (define (compound? p)
      (tagged-list? p 'procedure))
    
    ;过程参数列表
    (define (parameters p)
      (cadr p))
    
    ;过程体
    (define (body p)
      (let* ([origin (caddr p)]
             [defs (scan-out-defines origin)]
             [undefs (scan-except-defines origin)])
        (if (null? defs)
            undefs
            (let ([let-exp (new-let (new-binds defs)
                                 (append (new-sets defs)
                                         undefs))])
              (list let-exp)))))
    
    ;新的let语句
    (define new-let (let-dispatch 'construct))
    
    ;新的赋值语句
    (define new-assignment (assign-dispatch 'construct))
    
    
    ;扫描出所有的内部定义语句
    (define (scan-out-defines seqs)
      (if (null? seqs)
          seqs
          (let ([seq (car seqs)])
            (if ((define-dispatch 'define?) seq)
                (cons seq
                      (scan-out-defines (cdr seqs)))
                (scan-out-defines (cdr seqs))))))
    
    ;扫描出所有不是内部定义的语句
    (define (scan-except-defines seqs)
      (if (null? seqs)
          seqs
          (let ([seq (car seqs)])
            (if ((define-dispatch 'define?) seq)
                (scan-except-defines (cdr seqs))
                (cons seq
                      (scan-except-defines (cdr seqs)))))))
    
    ;创建新的键值绑定表
    (define (new-binds seqs)
      (if (null? seqs)
          seqs
          (cons ((let-dispatch 'bind) ((define-dispatch 'variable) (car seqs))
                                      ''**unassigned**)                          ;此处用符号，而不用字面量
                (new-binds (cdr seqs)))))                                        ;若使用字面量，解释器将认为这是一个变量
                                                                                 ;实现错误提示时将无法获知其真正绑定的变量名
                                   
    ;创建新的键值设置表
    (define (new-sets seqs)
      (if (null? seqs)
          seqs
          (cons (new-assignment ((define-dispatch 'variable) (car seqs))
                                ((define-dispatch 'value) (car seqs)))
                (new-sets (cdr seqs)))))
    
    
    
    
    ;过程环境
    (define (environment p)
      (cadddr p))
    
    ;是否为基本过程
    (define (primitive? proc)
      (tagged-list? proc 'primitive))
    
    ;基本过程的实现
    (define (primitive-implementation proc)
      (cadr proc))
    
    ;基本过程列表
    (define primitive-procedures
      (list (list 'car car)
            (list 'cdr cdr)
            (list 'cons cons)
            (list 'null? null?)
            (list 'eq? eq?)
            (list '+ +)))
    
    ;基本过程名列表
    (define (primitive-names)
      (map car
           primitive-procedures))
    
    ;基本过程对象列表
    (define (primitive-objects)
      (map (lambda (proc)
             (list 'primitive (cadr proc)))
           primitive-procedures))
    
    ;这些基本过程需要由解释器执行
    (define (apply-primitive proc args)
      (let ([p (primitive-implementation proc)])
        (apply p args)))
    
    (define (dispatch m)
      (cond [(eq? m 'construct) construct]
            [(eq? m 'compound?) compound?]
            [(eq? m 'parameters) parameters]
            [(eq? m 'body) body]
            [(eq? m 'environment) environment]
            [(eq? m 'primitive?) primitive?]
            [(eq? m 'apply-primitive) apply-primitive]
            [(eq? m 'primitive-names) (primitive-names)]
            [(eq? m 'primitive-objects) (primitive-objects)]
            [else (error "Unknown operator" m)]))
    dispatch))


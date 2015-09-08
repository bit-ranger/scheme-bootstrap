;#lang scheme

(load "let.scm")
(load "assignment.scm")
(load "letrec.scm")
(load "keywords.scm")


;提供同时定义语义的关键字
(define (install-letrec-eval)
  
  (let ([let-dispatch (make-let)]
        [assign-dispatch (make-assignment)]
        [letrec-dispatch (make-letrec)])
    
    (define new-let (let-dispatch 'construct))
    
    (define new-assignment (assign-dispatch 'construct))
    
    (define binds (letrec-dispatch 'binds))
    
    (define body (letrec-dispatch 'body))
    
    (define bind (letrec-dispatch 'bind))
    
    (define variable (letrec-dispatch 'variable))
    
    (define value (letrec-dispatch 'value))
    
    ;创建新的键值绑定表
    ;seq 旧的键值绑定表
    (define (new-binds seqs)
      (if (null? seqs)
          seqs
          (cons (bind (variable (car seqs))
                      (list 'quote undefined-keyword))            ;此处用符号，而不用字面量
                (new-binds (cdr seqs)))))                   ;若使用字面量，解释器将认为这是一个变量
    ;实现错误提示时将无法获知其真正绑定的变量名
    
    ;创建新的键值设置表
    ;seqs 旧的键值绑定表
    (define (new-sets seqs)
      (if (null? seqs)
          seqs
          (cons (new-assignment (variable (car seqs))
                                (value (car seqs)))
                (new-sets (cdr seqs)))))
    
    ;过程体解释
    (define (eval exp env)
      (let ([bd (binds exp)])
        (let ([let-exp (new-let (new-binds bd)
                                (append (new-sets bd)
                                        (body exp)))])
          (interp let-exp env))))
    
    (put eval eval-proc-key 'letrec)
    '(letrec eval installed)))
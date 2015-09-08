;#lang scheme

(define (make-letrec)
  
  ;let构造器
  ;body是一个列表
  ;binds是一个列表
  (define (construct binds body)
    (cons 'letrec (cons binds body)))
  
  ;letrec 绑定表
  (define (binds exp)
    (cadr exp))
  
  ;letrec 体
  (define (body exp)
    (cddr exp))
  
  ;创建一条绑定
  (define (bind key value)
    (list key value))
  
  ;绑定的key
  (define (variable kv)
    (car kv))
  
  ;绑定的value
  (define (value kv)
    (cadr kv))
  
  (define (parameters binds)
    (if (null? binds)
        binds
        (cons (caar binds)
              (parameters (cdr binds)))))
  
  (define (values binds)
    (if (null? binds)
        binds
        (cons (cadar binds)
              (values (cdr binds)))))
  
  (define (dispatch m)
    (cond [(eq? m 'construct) construct]
          [(eq? m 'binds) binds]
          [(eq? m 'body) body]
          [(eq? m 'parameters) parameters]
          [(eq? m 'values) values]
          [(eq? m 'bind) bind]
          [(eq? m 'variable) variable]
          [(eq? m 'value) value]
          [else (error "Unknown operator" m)]))
  
  dispatch)

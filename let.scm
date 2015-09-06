
(load "definition.scm")
(load "application.scm")
(load "lambda.scm")

(define (make-let)
  
  (define (construct binds body)
    (list 'let binds body))
  
  (define (binds exp)
    (if (named-let? exp)
        (cddr exp)
        (cdr exp)))
  
  (define (body exp)
    (if (named-let? exp)
        (new-body exp)
        (cddr exp)))
  
  ;命名let
  (define (named-let? exp)
    (not (pair? (cadr exp))))
  
  
  (define new-define ((make-define) 'construct))
  
  (define new-application ((make-application) 'construct))
  
  (define new-lambda ((make-lambda) 'construct))
  
  ;对body进行改变，转成内部定义，以及一次初始化调用
  (define (new-body exp)
    (let ([name (cadr exp)]
          [params (parameters (binds exp))])
      (list (new-define name
                        (new-lambda params
                                    (cdddr exp)))
            (new-application name params))))
  
  (define (dispatch m)
    (cond [(eq? m 'construct) construct]
          [(eq? m 'binds) binds]
          [(eq? m 'body) body]
          [else (error "Unknown operator" m)]))
  
  dispatch)


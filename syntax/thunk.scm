(define (make-thunk)
  
  (define (construct body env)
    (list 'thunk body env))
  
  (define (body thunk)
    (cadr thunk))
  
  (define (env thunk)
    (caddr thunk))
  
  (define (thunk? arg)
    (and (list? arg)
         (eq? 'thunk (car arg))))
  
  (define (dispatch m)
    (cond [(eq? m 'env) env]
          [(eq? m 'body) body]
          [(eq? m 'construct) construct]
          [(eq? m 'thunk?) thunk?]
          [else (error "Unknown operator" m)]))
  
  dispatch)

#lang scheme

(define a 1)

(define (test)
  
  (define a 3)
  
  (define b a)
  
  
  b)

(define (new-builder)
  (let* ([ls '()])
    
    (define assembly (λ (x y)
                       (cond [(null? x) y]
                             [else (if true
                                       (cons (car x)
                                             (assembly (cdr x)
                                                       y))
                                       false)])))
    
    (define (append x)
      (set! ls (assembly ls x)))
    
    (define (to-list)
      ls)
    
    (define (dispatch m)
      (cond [(eq? m 'to-list) (to-list)]
            [(eq? m 'append) append]))
    dispatch))

(define builder (new-builder))

((builder 'append) '(a b c))

((builder 'append) '(d e f))

(builder 'to-list)

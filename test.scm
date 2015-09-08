#lang scheme

(define a 1)

(define (test)
  
  (define b a)
  
  (define a 3)
  
  a)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;letrec模拟
(define (simulate-letrec)
  (let ([a '*]
        [b '*])
    (set! a b)
    (set! b 1)
    a))

;隔离定义作用域模拟
(define (simulate-scope)
  (let ([a '*]
        [b '*])
    (let ([x 1]
          [y a])
      (set! a x)
      (set! b y)
      b)))


(define (test-letrec)
  (define b 2)
  (letrec ([a b]
           [b 1])
    a))


(letrec ((b 2))
  (letrec ((a b) (b 1))
    a))


(let ((b '**undefined**))
  (set! b 2)
  (letrec ((a b) (b 1)) a))


(let ((a '**undefined**) (b '**undefined**))
  (set! a b)
  (set! b 1)
  a)
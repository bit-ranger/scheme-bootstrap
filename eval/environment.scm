(load "eval/keywords.scm")

(define (make-environment)
  
  ;已装入的框架
  (define (enclosing-environment env)
    (cdr env))
  
  ;第一个框架
  (define (first-frame env)
    (car env))
  
  ;创建一个框架
  (define (make-env-frame variables values)
    (cons variables values))
  
  ;框架中的变量列表
  (define (frame-variables frame)
    (car frame))
  
  ;框架中的值列表
  (define (frame-values frame)
    (cdr frame))
  
  ;添加一个键值对到框架中
  (define (add-binding-to-frame! var val frame)
    (set-car! frame (cons var (car frame)))
    (set-cdr! frame (cons val (cdr frame))))
  
  ;扩展环境
  ;即在已有的环境顶部增加一个frame
  ;frame中包含最新的参数值对
  (define (extend-environment vars vals base-env)
    (if (= (length vars) (length vals))
        (cons (make-env-frame vars vals) base-env)
        (if (< (length vars) (length vals))
            (error "Too many arguments supplied"
                   vars vals)
            (error "Too few arguments suuuplied"
                   vars vals))))
  
  ;查找变量的值
  (define (lookup-variable-value var env)
    (define (env-loop env)
      (define (scan vars vals)
        (cond ((null? vars)
               (env-loop (enclosing-environment env)))
              ((eq? var (car vars))
               (car vals))
              (else (scan (cdr vars) (cdr vals)))))
      (if (eq? env the-empty-environment)
          (error "Unbound variable" var)
          (let ((frame (first-frame env)))
            (scan (frame-variables frame)
                  (frame-values frame)))))
    (let ((value (env-loop env)))
      (if (eq? value undefined-keyword)
          (error var "undefined;  cannot use before initialization!")
          value)))
  
  ;改变赋值，全环境搜索
  (define (set-variable-value! var val env)
    (define (env-loop env)
      (define (scan vars vals)
        (cond ((null? vars)
               (env-loop (enclosing-environment env)))
              ((eq? var (car vars))
               (set-car! vals val))
              (else (scan (cdr vars) (cdr vals)))))
      (if (eq? env the-empty-environment)
          (error "Unbound variable" var)
          (let ((frame (first-frame env)))
            (scan (frame-variables frame)
                  (frame-values frame)))))
    (env-loop env))
  
  ;定义变量，只在当前frame搜索
  (define (define-variable! var val env)
    (let ((frame (first-frame env)))
      (define (scan vars vals)
        (cond ((null? vars)
               (add-binding-to-frame! var val frame))
              ((eq? var (car vars))
               (set-car! vals val))
              (else (scan (cdr vars) (cdr vals)))))
      (scan (frame-variables frame)
            (frame-values frame))))
  
  (define (dispatch m)
    (cond ((eq? m 'def) define-variable!)
          ((eq? m 'set) set-variable-value!)
          ((eq? m 'lookup) lookup-variable-value)
          ((eq? m 'extend) extend-environment)
          (else (error "Unknown operator" m))))
  dispatch)

;空环境
(define the-empty-environment
  '())

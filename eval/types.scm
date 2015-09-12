(load "lib/table.scm")

(define operation-table
  (make-table))

(define get (operation-table 'lookup-proc))

(define put (operation-table 'insert-proc!))


;;;为数据实体增加类型标签
;type-tag 类型符号
;contents 数据实体
(define (attach-tag type-tag contents)
  (cons type-tag contents))

;;;取出类型标签
(define (type-tag z)
  (if (pair? z)
      (car z)
      (error "Bad tagged datum -- TYPE-TAG" z)))

;;;取出数据实体
(define (contents z)
  (if (pair? z)
      (cdr z)
      (error "Bad tagged datum -- CONTENTS" z)))


;;;对函数进行应用，应用规则是根据参数类型自动分派
; op 函数名
; args 参数列表
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args))) ;;;参数类型的列表
    (let ((proc (get op type-tags)))     ;;;通过函数名与类型列表取到函数体
      (if proc
          (apply proc (map contents args))
          (error "No method for these types -- APPLY-GENERIC"
                 (list op type-tags))))))



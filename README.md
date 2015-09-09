# scheme-bootstrap

这是一个自举的scheme解释器

启动文件为： `interp.scm`

建议使用 [racket](http://racket-lang.org/) 或 [MIT-scheme](http://www.gnu.org/software/mit-scheme/) 运行此程序


# 示例

首先，打开`interpl.scm`文件的**REPL**

然后，输入 `(repl)`，此时控制台将进入等待输入状态。

在控制台输入scheme代码，例如：

```scheme
(define (new-builder)
  (let ((ls '()))
    
    (define assembly (lambda (x y)
                       (cond ((null? x) y)
                             (else (if true
                                       (cons (car x)
                                             (assembly (cdr x)
                                                       y))
                                       false)))))
    
    (define (append x)
      (set! ls (assembly ls x)))
    
    (define (to-list)
      ls)
    
    (define (dispatch m)
      (cond ((eq? m 'to-list) (to-list))
            ((eq? m 'append) append)))
    dispatch))
```

此时该函数将注册到环境中，然后再试用此函数。

```scheme
(define builder (new-builder))

((builder 'append) '(a b c))

((builder 'append) '(d e f))

(builder 'to-list)
```

## 一些“不正常”的代码

计算10的阶乘

```scheme
((λ (n)
   ((λ (f)
      (f f n))
    (λ (f k)
      (if (= k 1)
          1
          (* k (f f (- k 1)))))))
 10)
```

递归的内部定义
```scheme
(define (even? x)
  (define (e? n)
    (if (= n 0)
        true
        (o? (- n 1))))
  (define (o? n)
    (if (= n 0)
        false
        (e? (- n 1))))
  (e? x))
```

###Enjoy It!

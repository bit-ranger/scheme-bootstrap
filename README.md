# scheme-bootstrap

这是一个自举的scheme解释器

启动文件为： `interp.scm`

建议使用 [scheme](http://www.gnu.org/software/mit-scheme/) 或 [racket](http://racket-lang.org/) 运行此程序


# 程序入口

首先，打开`interpl.scm`文件的**REPL**


有两个程序入口可供使用，两个入口功能完全相同，但是其实现机理却完全不同。


**第一个**


输入 `(repl)`，此时控制台将进入等待输入状态。

该入口提供“边解释边执行”的运算过程，对每个子表达式都会重复使用完全相同的流程。

**第二个**

输入 `(repl-analyze)`，此时控制台将进入等待输入状态。

该入口提供“先解释后执行”的运算过程，对每次输入的表达式，都会先进行完全的静态分析，将表达式“编译”为解释器可以直接运行的过程，然后再执行编译后的过程。

理论上，第二个入口消除了大量的重复解释以及过程检索，效率应该更高。

# 示例

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

# 惰性求值

`delay` 用于使表达式延迟求值

```scheme
(define (f x)
  (delay (set! x (cons x '(2))))
  x)
```

`(f 1)` 返回值为 `1`

`force` 用于对延迟的表达式强制求值

```scheme
(define (f x)
  (force (delay (set! x (cons x '(2)))))
  x)
```
`(f 1)` 返回值为 `'(1 2)`


#Enjoy It!

#scheme-bootstrap

这是一个自举的scheme解释器

启动文件为： `interp.scm`

建议使用 [racket](http://racket-lang.org/) 或 [MIT-scheme](http://www.gnu.org/software/mit-scheme/) 运行此程序


#示例

首先，打开`interpl.scm`文件的**repl**

然后，输入 `(driver-loop)`

在输入框中输入scheme代码，例如：

```scheme
(define (append x y)
  (cond [(null? x) y]
        [else (if true
                  (cons (car x)

                        (append (cdr x)
                                y))
                  false)]))
```

此时该函数将注册到环境中，然后再试用此函数。

```scheme
(append '(a b c) '(e f g))
```

预计效果如下图：

![pic](http://7u2qnl.com1.z0.glb.clouddn.com/scheme-bootstrp.png)



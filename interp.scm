#lang scheme/load

(require (planet neil/sicp))

(load "core.scm")
(load "analyze.scm")
(load "variable-eval.scm")
(load "quote-eval.scm")
(load "assignment-eval.scm")
(load "definition-eval.scm")
(load "lambda-eval.scm")
(load "if-eval.scm")
(load "begin-eval.scm")
(load "cond-eval.scm")
(load "let-eval.scm")
(load "let*-eval.scm")
(load "and-eval.scm")
(load "or-eval.scm")
(load "letrec-eval.scm")
(load "application-eval.scm")
(load "environment.scm")
(load "procedure.scm")

(install-letrec-eval)
(install-let*-eval)
(install-and-eval)
(install-or-eval)
(install-assignment-eval)
(install-quote-eval)
(install-begin-eval)
(install-let-eval)
(install-cond-eval)
(install-lambda-eval)
(install-definition-eval)
(install-if-eval)
(install-application-eval)
(install-variable-eval)



(define env-dispatch (make-environment))
(define proc-dispatch (make-procedure))

(define (setup-environment)

  (define def-var (env-dispatch 'def))
  
  (let ([initial-env ((env-dispatch 'extend) (proc-dispatch 'primitive-names)
                                             (proc-dispatch 'primitive-objects)
                                             the-empty-environment)])
    
    ;为一些基本值赋予含义
    (def-var 'true true initial-env)
    (def-var 'false false initial-env)
    initial-env))

;全局环境
(define the-global-environment (setup-environment))

(define (prompt-for-input)
  (newline)
  (newline)
  (display ";;; M-Eval input: ")
  (newline))

(define (announce-output)
  (newline)
  (display ";;; M-Eval value: ")
  (newline))

(define (result-print object)
  (if ((proc-dispatch 'compound?) object)
      (display (list 'compound-procedure
                     ((proc-dispatch 'parameters) object)
                     ((proc-dispatch 'body) object)
                     '<procedure-env>))
      (display object)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;主程序,边解释边执行
(define (repl)
  (prompt-for-input)
  (let ([input (read)])
    (let ([output (interp input the-global-environment)])
      (announce-output)
      (result-print output)))
  (repl))

;;;主程序，先解释后执行
(define (repl-analyze)
  (prompt-for-input)
  (let ([input (read)])
    (let ([output ((analyze input) the-global-environment)])
      (announce-output)
      (result-print output)))
  (repl-analyze))
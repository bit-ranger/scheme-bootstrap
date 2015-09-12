;#lang scheme/load
;(require (planet neil/sicp))

(load "eval/core.scm")
(load "eval/analyze.scm")
(load "eval/variable-eval.scm")
(load "eval/quote-eval.scm")
(load "eval/assignment-eval.scm")
(load "eval/definition-eval.scm")
(load "eval/lambda-eval.scm")
(load "eval/if-eval.scm")
(load "eval/begin-eval.scm")
(load "eval/cond-eval.scm")
(load "eval/let-eval.scm")
(load "eval/let*-eval.scm")
(load "eval/and-eval.scm")
(load "eval/or-eval.scm")
(load "eval/letrec-eval.scm")
(load "eval/application-eval.scm")
(load "eval/environment.scm")
(load "eval/procedure.scm")
(load "eval/delay-force-eval.scm")

(install-delay&force-eval)
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
  
  (let ((initial-env ((env-dispatch 'extend) (proc-dispatch 'primitive-names)
                                             (proc-dispatch 'primitive-objects)
                                             the-empty-environment)))
    
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
  (let ((input (read)))
    (let ((output (interp input the-global-environment)))
      (announce-output)
      (result-print output)))
  (repl))

;;;主程序，先解释后执行
(define (repl-analyze)
  (prompt-for-input)
  (let ((input (read)))
    (let ((output ((analyze input) the-global-environment)))
      (announce-output)
      (result-print output)))
  (repl-analyze))

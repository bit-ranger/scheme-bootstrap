#lang scheme/load

(require (planet neil/sicp))

(load "core.scm")
(load "variable-eval.scm")
(load "quote-eval.scm")
(load "assignment-eval.scm")
(load "definition-eval.scm")
(load "lambda-eval.scm")
(load "if-eval.scm")
(load "begin-eval.scm")
(load "cond-eval.scm")
;(require "let.ss")
;(require "let*.ss")
;(require "and.ss")
;(require "or.ss")
(load "application-eval.scm")
(load "environment.scm")
(load "procedure.scm")

(install-quote-eval)
(install-variable-eval)
(install-begin-eval)
(install-assignment-eval)
(install-lambda-eval)
(install-definition-eval)
(install-if-eval)
(install-cond-eval)
;(install-and-package)
;(install-or-package)
;(install-let-package)
;(install-let*-package)
(install-application-eval)



(define (setup-environment)
  (let ([initial-env (extend-environment (primitive-procedure-names)
                                         (primitive-procedure-objects)
                                         the-empty-environment)])
    ;对与单引号的解释：因为解释器读取的是一个文本，这意味着整个文本就是一个大符号列表
    ;其中每个表达式都是符号
    ;而如果表达式中显式的存在单引号，那么其真实结构是'quote 与 表达式形成的符号表。
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))

(define the-global-environment (setup-environment))


(define input-prompt ";;; M-Eval input: ")
(define output-prompt ";;; M-Eval value: ")

(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ([input (read)])
    (let ([output (interp input the-global-environment)])
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))

(define (prompt-for-input string)
  (newline)
  (newline)
  (display string)
  (newline))

(define (announce-output string)
  (newline)
  (display string)
  (newline))

(define (user-print object)
  (if (compound-procedure? object)
      (display (list 'compound-procedure
                     (procedure-parameters object)
                     (procedure-body object)
                     '<procedure-env>))
      (display object)))
;#lang scheme/load
;(require (planet neil/sicp))

(load "core.scm")

(load "procedure.scm")

(load "lambda.scm")


;对lambda的处理
(define (install-lambda-eval)

  (define lambda-dispatch (make-lambda))

  (define (parameters exp)
    ((lambda-dispatch 'parameters) exp))

  (define (body exp)
    ((lambda-dispatch 'body) exp))
  
  (define (eval exp env)
    (make-procedure (parameters exp)
                    (body exp)
                    env))

  (put eval 'eval 'lambda)
  '(install lambda done))
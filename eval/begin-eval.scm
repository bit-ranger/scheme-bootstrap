(load "eval/core.scm")
(load "eval/analyze.scm")
(load "syntax/begin.scm")

;对begind的处理
(define (install-begin-eval)
  
  (let ((begin-dispatch (make-begin)))
    
    (define actions (begin-dispatch 'actions))
    
    (define (eval exp env)
      (interp-sequence (actions exp)
                       env))
    
    (define (observe exp)
      (analyze-sequence (actions exp)))
    
    (put eval eval-proc-key 'begin)
    (put observe observe-proc-key 'begin)
    '(begin eval installed)))



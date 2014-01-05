(define-module (olscrbl action)
  #:use-module (olscrbl config internal)
  #:export (action-is-final?
            built-in-action?
            run-action))

(define (get-action-data act)
  (hashq-ref actions act))

(define (action-is-final? act)
  (eq? (hashq-ref (get-action-data act) 'type) 'final))

(define (built-in-action? ac)
  (and (symbol? ac)
       (memq ac '(submit drop))))

(define (run-action act data)
  ((hashq-ref (get-action-data act) 'code) act data))

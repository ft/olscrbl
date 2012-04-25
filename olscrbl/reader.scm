(define-module (olscrbl reader)
  :export (reader-get-proc
           reader-set-proc))

(define reader '())

(define (reader-set-proc ft kind proc)
  (let ((ft-reader (or (assoc-ref reader ft)
                       '())))
    (set! ft-reader (assoc-set! ft-reader kind proc))
    (set! reader (assoc-set! reader ft ft-reader))))

(define (reader-get-proc ft kind)
  (assoc-ref (assoc-ref reader ft) kind))

;; Pre- and post-processing for data in olscrbl

(define-module (olscrbl pp)
  #:use-module (srfi srfi-1)
  #:export (make-processing-plant
            process
            process-maybe))

(define (process target callbacks)
  (fold (lambda (cb retval)
          (cb retval))
        target
        callbacks))

(define (process-maybe plant target item)
  (let ((cbs (plant item)))
    (if (null? cbs)
        target
        (process target cbs))))

(define (make-processing-plant)
  (let ((callbacks (make-hash-table)))

    (define (get-callbacks item)
      (hashq-ref callbacks item '()))

    (define (get-keys-with-callbacks keys)
      (fold (lambda (x acc)
              (if (null? (get-callbacks x))
                  acc
                  (cons x acc)))
            '()
            keys))

    (define (add-callback item cb)
      (unless (procedure? cb)
        (error "Callback argument needs to be a procedure!"))
      (let ((old (get-callbacks item)))
        (hashq-set! callbacks item
                    (append old (list cb)))))

    (lambda args
      (apply (case (length args)
               ((1) (if (list? (car args))
                        get-keys-with-callbacks
                        get-callbacks))
               ((2) add-callback)
               (else (error "Invalid use of pre/post-process")))
             args))))

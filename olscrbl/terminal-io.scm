(define-module (olscrbl terminal-io)
  #:use-module (ice-9 format)
  #:use-module (ice-9 threads)
  #:export (io))

(define terminal-io-mutex (make-mutex))

(define (io . args)
  (lock-mutex terminal-io-mutex)
  (apply format #t args)
  (unlock-mutex terminal-io-mutex))

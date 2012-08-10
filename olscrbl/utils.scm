(define-module (olscrbl utils)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-18)
  #:export (die
            open-utf8-file
            timestamp))


(define-syntax die
  (syntax-rules ()
    ((_ fmt args ...)
     (begin
       (format #t fmt args ...)
       (quit 1)))))

(define (open-utf8-file fn mode)
  (let ((port (open-file fn mode)))
    (set-port-encoding! port "UTF-8")
    port))

(define (timestamp)
  (floor (time->seconds (current-time))))

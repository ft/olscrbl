(define-module (olscrbl utils)
  :export (die
           open-utf8-file
           timestamp))

(use-modules (ice-9 format)
             (srfi srfi-18))

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

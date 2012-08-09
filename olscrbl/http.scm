(define-module (olscrbl http)
  :use-module (ice-9 format)
  :use-module (ice-9 pretty-print)
  :use-module (web client)
  :use-module (web request)
  :use-module (web response)
  :use-module (web uri)
  :use-module (srfi srfi-1)
  :use-module (rnrs bytevectors)
  :export (http-post))

;; Meh. Guile has no POST method built-in, yet. So wip up something simple.

(define (http-post uri-string contents)
  (let* ((body (fold (lambda (string prior)
                       (string-append prior string))
                     ""
                     contents))
         (uri (string->uri uri-string))
         (headers `((connection close)
                    (content-length . ,(string-length body))
                    (content-type . (application/x-www-form-urlencoded))))
         (port (open-socket-for-uri uri))
         (r (build-request uri
                           #:version '(1 . 1)
                           #:method 'POST
                           #:port port
                           #:headers headers)))
    (write-request r port)
    (format port body)
    (force-output port)
    (let* ((response (read-response port))
           (body (read-response-body response)))
      (string-split (utf8->string body) #\newline))))

(define-module (olscrbl main)
  #:use-module (ice-9 format)
  #:use-module (ice-9 pretty-print)
  #:use-module (olscrbl definitions)
  #:use-module (olscrbl scrobbler-log)
  #:use-module (olscrbl submit)
  #:use-module (olscrbl config)
  #:use-module (olscrbl config internal)
  #:use-module (olscrbl config utils)
  #:export (olscrbl-main))

(define (olscrbl-main)
  (cond ((= (cnt-accounts) 0)
         (format #t "No accounts defined.\n")
         (quit 0))
        ((= (cnt-active-accounts) 0)
         (format #t "All accounts deactivated!\n")
         (quit 0))
        ((= (cnt-matchers) 0)
         (if (get-opt-unsafe 'note-default-matcher)
             (format #t "No matchers defined. Registering the default.\n"))
         (match-entry #:predicate (code #t))))
  (initialise-scrobbler-reader)
  ;;(pretty-print (@@ (olscrbl reader) reader))
  ;;(pretty-print (get-opt-unsafe 'log-file-type))
  (submit/setup-reader (get-opt-unsafe 'log-file-type))
  ;;(submit/setup-threads accounts)
  (cond ((null? *program-args-non-options*)
         (submit-with-port (current-input-port)))
        (else
         (for-each (lambda (file)
                     (submit-with-file file))
                   *program-args-non-options*))))

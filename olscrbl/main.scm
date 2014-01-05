(define-module (olscrbl main)
  #:use-module (ice-9 format)
  #:use-module (ice-9 pretty-print)
  #:use-module (olscrbl terminal-io)
  #:use-module (olscrbl definitions)
  #:use-module (olscrbl scrobbler-log)
  #:use-module (olscrbl submit)
  #:use-module (olscrbl config)
  #:use-module (olscrbl config internal)
  #:use-module (olscrbl config utils)
  #:export (olscrbl-main))

(define (olscrbl-main)
  (cond ((= (cnt-accounts) 0)
         (io "No accounts defined.\n")
         (quit 0))
        ((= (cnt-active-accounts) 0)
         (io "All accounts deactivated!\n")
         (quit 0))
        ((= (cnt-matchers) 0)
         (if (get-opt-unsafe 'note-default-matcher)
             (io "No matchers defined. Registering the default.\n"))
         (match-entry #:predicate (code #t))))
  ;; Register the default scrobbler-log reader.
  (initialise-scrobbler-reader)
  ;; Use the scrobbler-log reader the user actually configured, which will be
  ;; the default reader most of the time, unless the user defined a custom one.
  (submit/setup-reader (get-opt-unsafe 'log-file-type))
  (cond ((null? *program-args-non-options*)
         (submit-with-port (current-input-port)))
        (else
         (for-each (lambda (file)
                     (submit-with-file file))
                   *program-args-non-options*))))

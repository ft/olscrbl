(use-modules (olscrbl config internal)
             (taptest))

(primitive-load "taptest-config.scm")

(with-fs-test-bundle
 (plan 15)
 (internal/initialise-options)

 (define-test "initialise default options"
   (pass-if-eq? (internal/get-option 'unmatched-entries)
                'keep))

 (define-test "internal/getopt throws on bad option"
   (pass-if-exception 'unknown-option
                      (internal/get-option 'non-existent)))

 (define-test "internal/set-option works"
   (internal/set-option 'unmatched-entries 'drop)
   (pass-if-eq? (internal/get-option 'unmatched-entries)
                'drop))

 (define-test "internal/set-option throws on bad-key"
   (pass-if-exception 'unknown-option
                      (internal/set-option 'non-existent 'foo)))

 (define-test "internal/set-option throws on bad-value"
   (pass-if-exception 'broken-value
                      (internal/set-option 'unmatched-entries 'foo)))

 (define-test "get-opt-unsafe good"
   (internal/set-option 'unmatched-entries 'keep)
   (pass-if-eq? (get-opt-unsafe 'unmatched-entries)
                'keep))

 (define-test "get-opt-unsafe bad"
   (pass-if-false (get-opt-unsafe 'non-existent)))

 (let ((bv (@@ (olscrbl config internal) broken-value?)))
   (for-each (lambda (x)
               (let ((result (car x))
                     (key (cadr x))
                     (value (caddr x)))
                 (define-test (format #f "broken-value?: ~a ? ~a" key value)
                   (pass-if-eq? result
                                (bv key value)))))
             '((#f note-default-matcher #t)
               (#t note-default-matcher 123)
               (#f log-file-type scrobbler-log)
               (#f log-file-type something)
               (#t log-file-type 123)
               (#f unmatched-entries keep)
               (#f unmatched-entries drop)
               (#t unmatched-entries foo-bar-baz)))))

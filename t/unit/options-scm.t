(use-modules (olscrbl config internal)
             (taptest))

(with-test-bundle (options internal)
  (plan 1)
  (define-test "initialise default options"
    (internal/initialise-options)
    (pass-if-eq? (internal/get-option 'unmatched-entries)
                 'keep)))

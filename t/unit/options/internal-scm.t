(use-modules (olscrbl config internal)
             (taptest))

(primitive-load "taptest-config.scm")

(with-fs-test-bundle
 (plan 1)
 (define-test "initialise default options"
   (internal/initialise-options)
   (pass-if-eq? (internal/get-option 'unmatched-entries)
                'keep)))

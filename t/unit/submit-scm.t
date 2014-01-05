(use-modules (olscrbl config)
             (olscrbl config internal)
             (olscrbl submit)
             (taptest))

(primitive-load "taptest-config.scm")

(define gsh (@@ (olscrbl submit) generate-scrobbling-handshake))

(with-fs-test-bundle
 (plan 1)

 (add-account :name 'last-fm
              :user "someone"
              :password "password"
              :uri "post.audioscrobbler.com"
              :port 80)

 (internal/initialise-options)
 (define-test "generate-scrobbling-handshake"
   (pass-if-re-match (string-concatenate
                      `("^http://post\\.audioscrobbler\\.com:80/"
                        "\\?hs=true"
                        "\\&p=1.2.1"
                        "\\&c=" ,(get-opt-unsafe 'client-identifier)
                        "\\&v=" ,(get-opt-unsafe 'client-version)
                        "\\&u=someone"
                        ;; "&t" and "&a" depend on the current time, so just
                        ;; check if the fields yield the correct character set.
                        "\\&t=[0-9][0-9]*"
                        "\\&a=[a-f0-9][a-f0-9]*$"))
                     (gsh 'last-fm))))

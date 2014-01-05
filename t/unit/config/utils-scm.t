(use-modules (olscrbl config)
             (olscrbl config utils)
             (taptest))

(primitive-load "taptest-config.scm")

(with-fs-test-bundle
 (plan 15)

 (define-test "accounts empty #1"
   (pass-if-= (cnt-accounts)
              0))

 (define-test "accounts empty #2"
   (pass-if-equal? (get-accounts)
                   '()))

 (add-account :name 'libre-fm
              :user "someone"
              :password "password"
              :uri "turtle.libre.fm"
              :port 80
              :active #f)

 (define-test "one account"
   (pass-if-= (cnt-accounts)
              1))

 (add-account :name 'last-fm
              :user "someone"
              :password "password"
              :uri "post.audioscrobbler.com"
              :port 80)

 (define-test "two accounts"
   (pass-if-= (cnt-accounts)
              2))

 (define-test "actions empty #1"
   (pass-if-= (cnt-actions)
              0))

 (define-test "actions empty #2"
   (pass-if-equal? (get-actions)
                   '()))

 (add-action #:name 'foobar
             #:type 'side-effect
             #:code (code #t))

 (define-test "one action"
   (pass-if-= (cnt-actions)
              1))

 (add-action #:name 'mumble
             #:type 'side-effect
             #:code (code #f))

 (define-test "two actions"
   (pass-if-= (cnt-actions)
              2))
 (define-test "matchers empty"
   (pass-if-= (cnt-matchers)
              0))

 (match-entry #:artist "Lamb of God")

 (define-test "one matcher"
   (pass-if-= (cnt-matchers)
              1))

 (match-entry #:accounts '(last-fm)
              #:artist "Slayer")

 (define-test "two matchers"
   (pass-if-= (cnt-matchers)
              2))

 (define-test "account-active? yes"
   (pass-if-true (account-active? 'last-fm)))

 (define-test "account-active? no"
   (pass-if-false (account-active? 'libre-fm)))

 (define-test "account-parameter"
   (pass-if-= (account-parameter 'last-fm 'port)
              80))

 (define-test "cnt-active-accounts"
   (pass-if-= (cnt-active-accounts)
              1)))

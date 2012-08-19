(use-modules (olscrbl scrobbler-log)
             (taptest))

(primitive-load "taptest-config.scm")

(with-fs-test-bundle
 (plan 2)
 (let ((record "a\tb\tc\t23\t123\tl\t1234123444\tcafebabeebabefac")
       (parsed '("a" "b" "c" "23" "123" "l" "1234123444" "cafebabeebabefac")))
   (define-test "parse-record"
     (pass-if-equal? parsed
                     (parse-record record)))
   (define-test "produce-record"
     (pass-if-string=? record
                       (produce-record parsed)))))

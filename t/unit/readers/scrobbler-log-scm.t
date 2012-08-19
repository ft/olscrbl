(use-modules (olscrbl scrobbler-log)
             (srfi srfi-1)
             (taptest))

(primitive-load "taptest-config.scm")

(with-fs-test-bundle
 (plan 13)
 (let ((record "a\tb\tc\t23\t123\tl\t1234123444\tcafebabeebabefac")
       (parsed '("a" "b" "c" "23" "123" "l" "1234123444" "cafebabeebabefac"))
       (items '(artist
                album track tracknumber
                duration status timestamp musicbrainz)))
   (define-test "parse-record"
     (pass-if-equal? parsed
                     (parse-record record)))
   (define-test "produce-record"
     (pass-if-string=? record
                       (produce-record parsed)))
   (define-test "valid-data valid"
     (pass-if-true (valid-data? parsed)))
   (define-test "valid-data too-short"
     (pass-if-false (valid-data? (cdr parsed))))
   (define-test "valid-data too-long"
     (pass-if-false (valid-data? (cons "stuff" parsed))))
   (for-each (lambda (data item)
               (define-test (format #f "extract-data `~a'" item)
                 (pass-if-string=? data
                                   (extract-data parsed item))))
             parsed
             items)))

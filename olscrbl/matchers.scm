(define-module (olscrbl matchers)
  #:use-module (olscrbl config utils)
  #:use-module (olscrbl scrobbler-log)
  #:use-module (ice-9 pretty-print)
  #:export (run-matchers
            matcher/album
            matcher/artist
            matcher/track))

(define (matcher/album string)
  #t)

(define (matcher/artist string data)
  (string= string (extract-data data 'artist)))

(define (matcher/track string data)
  (string= string (extract-data data 'track)))

(define (check-predicate matcher data)
  (or (and (thunk? matcher)
           (matcher))
      (and (procedure? matcher)
           (matcher data))
      (and (pair? matcher)
           (let ((prd (car matcher))
                 (arg (cdr matcher)))
             (prd arg data)))))

(define (run-matchers account data)
  (for-all-matchers (cm)
    (let ((accounts (matcher-get-accounts cm))
          (predicates (matcher-get-predicates cm)))
      (if (or (not accounts)
              (memq account accounts))
          (let next-predicate ((remaining-predicates predicates))
            (cond ((null? remaining-predicates)
                   ;; All tests passed, this track should be submitted.
                   (throw 'matchers-done))
                  ((not (check-predicate (car remaining-predicates) data))
                   ;; Test failed, so exist early.
                   #t)
                  (else
                   ;; Keep going.
                   (next-predicate (cdr remaining-predicates)))))))))

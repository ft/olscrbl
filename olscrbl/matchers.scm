(define-module (olscrbl matchers)
  #:use-module (olscrbl config utils)
  #:use-module (olscrbl action)
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

(define (run-predicates ps data)
  (cond ((null? ps) #t)
        ((not (check-predicate (car ps) data)) #f)
        (else (run-predicates (cdr ps) data))))

;; So, here's how matchers work: They are called for each and every entry that
;; is processed. A matcher has a list of predicates, all of which have to
;; return true for the matcher to, in fact, match. When a matcher matches an
;; entry, it associates it with an action, that the system should take. There
;; are a number built-in ones, that determine how an entry is finally handled
;; (those are: submit and drop).
;;
;; In addition to that, a user may define her own actions. The action's type
;; defines how a matcher that uses the action effects the on-going processing
;; of the entry: The ‘final’ type means that after this action, no further
;; matchers will be executed (this mimics the behaviour of the built-in
;; actions); The ‘side-effect’ type signals, that the action is to be carried
;; out purely for the side-effects that it causes. Afterwards other matchers
;; are executed as if the ‘side-effect’ action didn't run at all.
(define (run-matchers account data)
  (for-all-matchers (cm)
    (let ((accounts (matcher-get-accounts cm))
          (predicates (matcher-get-predicates cm)))
      (if (or (not accounts) (memq account accounts))
          (let* ((match? (run-predicates predicates data))
                 (act (if match? (matcher-get-action cm) #f)))
            (cond ((not act) #f)
                  ((built-in-action? act)
                   (throw 'matchers-done act))
                  ((action-is-final? act)
                   (run-action act data)
                   (throw 'matchers-done act))
                  (else
                   (run-action act data))))))))

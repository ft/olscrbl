;; Code that's not directly useful for the user's configuration file, but
;; helpful for dealing with the data structures filled *by* the configuration.

(define-module (olscrbl config utils)
  #:use-module (olscrbl config)
  #:use-module (olscrbl config internal)
  #:use-module (olscrbl reader)
  #:use-module (srfi srfi-1)
  #:export (account-active?
            account-parameter
            cnt-accounts
            cnt-active-accounts
            cnt-actions
            cnt-matchers
            for-all-matchers
            get-accounts
            get-actions
            matcher-get-accounts
            matcher-get-predicates
            register-reader))

(define (account-parameter a p)
  (extract-parameter (get-account a) p))

(define (account-active? a)
  (account-parameter a 'active))

(define (matcher-get-accounts m)
  (car m))

(define (matcher-get-predicates m)
  (cadr m))

;; for-all-matchers:
;;
;; (for-all-matchers (current-matcher)
;;   (pretty-print (matcher-get-accounts current-account)))
;;
;; Iterate over all matchers, put the current matcher into a variable named in
;; the parenthesis of the expression (`current-matcher' in this case), and
;; execute the supplied code. That code may throw `matchers-done', in which
;; case the construct exits the iteration and returns `#f'. If no exception is
;; thrown (which means all matchers were processed) `#t' is returned.
;;
;; You can use `matcher-get-accounts' and `matcher-get-predicates' on
;; `current-matcher' to extract the respective parts from the data structures.
(define-syntax for-all-matchers
  (syntax-rules ()
    ((_ (iter) code ...)
     (catch 'matchers-done
       (lambda ()
         (let next-matcher ((remaining-matchers matchers))
           (cond
            ((null? remaining-matchers) #t)
            (else
             (let ((iter (car remaining-matchers)))
               code ...)
             (next-matcher (cdr remaining-matchers))))))
       (lambda (key . args) #f)))))

(define (cnt-accounts)
  (length (get-accounts)))

(define (cnt-active-accounts)
  (fold (lambda (account accumulator)
          (if (account-active? account)
              (1+ accumulator)
              accumulator))
        0
        (get-accounts)))

(define (get-accounts)
  (keys accounts))

(define (cnt-actions)
  (length (get-actions)))

(define (get-actions)
  (keys actions))

(define (cnt-matchers)
  (length matchers))

(define* (register-reader #:key
                          type
                          read-record
                          parse-record
                          valid-data
                          extract-data
                          produce-record)
  (verify-type 'register-reader type (lambda (t)
                                       (and (not (eq? t #f))
                                            (symbol? t))))
  (verify-type 'register-reader read-record procedure?)
  (verify-type 'register-reader parse-record procedure?)
  (verify-type 'register-reader valid-data procedure?)
  (verify-type 'register-reader extract-data procedure?)
  (verify-type 'register-reader produce-record procedure?)
  (reader-set-proc type 'read-record read-record)
  (reader-set-proc type 'parse-record parse-record)
  (reader-set-proc type 'valid-data valid-data)
  (reader-set-proc type 'extract-data extract-data)
  (reader-set-proc type 'produce-record produce-record))

;; Code that's not directly useful for the user's configuration file, but
;; helpful for dealing with the data structures filled *by* the configuration.

(define-module (olscrbl config utils)
  :use-module (olscrbl config)
  :use-module (olscrbl config internal)
  :export (cnt-accounts
           cnt-actions
           cnt-matchers
           for-all-matchers
           matcher-get-accounts
           matcher-get-predicates))

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

(define (cnt-hash h)
  (hash-fold (lambda (k v p) (1+ p))
             0 h))

(define (cnt-accounts)
  (cnt-hash accounts))

(define (cnt-actions)
  (cnt-hash actions))

(define (cnt-matchers)
  (length matchers))

(define-module (olscrbl data-access)
  #:export (read-record
            parse-record
            valid-data?
            valid-keys
            extract-data
            set-data!
            produce-record))

(define read-record #f)
(define parse-record #f)
(define valid-data? #f)
(define valid-keys #f)
(define extract-data #f)
(define set-data! #f)
(define produce-record #f)

(define-module (olscrbl config internal)
  #:use-module (ice-9 format)
  #:use-module (ice-9 pretty-print)
  #:use-module (olscrbl terminal-io)
  #:use-module (olscrbl definitions)
  #:export (accounts
            actions
            extract-parameter
            matchers
            options
            get-opt-unsafe
            get-account
            keys
            internal/get-option
            internal/initialise-options
            internal/set-option
            verify-type))

(define accounts (make-hash-table 8))
(define actions (make-hash-table))
(define matchers '())
(define options (make-hash-table 3))

(define-syntax with-option
  (syntax-rules ()
    ((_ (key value) code ...)
     (let ((value (hash-get-handle options key)))
       (if value
           (begin
             code ...)
           (throw 'unknown-option))))))

(define (keys hash)
  (hash-fold (lambda (k v prior)
               (cons k prior))
             '()
             hash))

(define (internal/initialise-options)
  (hash-set! options 'client-identifier "ols")
  (hash-set! options 'client-version *program-version*)
  (hash-set! options 'log-file-type 'scrobbler-log)
  (hash-set! options 'unmatched-entries 'keep)
  (hash-set! options 'note-default-matcher #t))

(define valid/unmatched-entries '(keep drop))

(define (broken-value? key value)
  (cond ((eq? key 'unmatched-entries)
         (eq? (memq value valid/unmatched-entries)
              #f))
        ((memq key '(client-version client-identifier))
         (not (string? value)))
        ((eq? key 'note-default-matcher)
         (not (boolean? value)))
        ((eq? key 'log-file-type)
         (not (symbol? value)))
        (else
         (io "Missing verification fct for `~a'.\n" key)
         #f)))

(define (internal/set-option key value)
  (with-option (key oldvalue)
    (if (broken-value? key value)
        (throw 'broken-value))
    (hash-set! options key value)))

(define (internal/get-option key)
  (with-option (key value)
    (cdr value)))

(define (get-opt-unsafe key)
  (hash-ref options key))

(define (get-account a)
  (hashq-ref accounts a))

(define (extract-parameter data p)
  (hashq-ref data p))

(define-syntax verify-type
  (syntax-rules()
    ((_ caller var predicate)
     (if (or (predicate var)
             (thunk? var))
         #t
         (let* ((p (symbol->string (quote predicate)))
                (l (- (string-length p) 1)))
           (error
            "\n -!- ~a: `~a' needs to be a ~a (or a thunk producing one).\n\n"
            (symbol->string caller)
            (symbol->string (quote var))
            (substring p 0 l)))))))

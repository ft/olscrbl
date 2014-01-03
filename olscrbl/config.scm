(define-module (olscrbl config)
  #:use-module (ice-9 pretty-print)
  #:use-module (ice-9 format)
  #:use-module (ice-9 optargs)
  #:use-module (olscrbl config internal)
  #:use-module (olscrbl matchers)
  #:use-module (olscrbl utils)
  #:export (;; functions
            add-account
            add-action
            match-entry
            ;; macros
            code
            get-opt
            set-opt))

;; Let's make use of `:foo' style keyword-read-syntax. `#:foo' still works, but
;; I think for former makes for better readability in user-configuration files.
(read-set! keywords 'prefix)

(define-syntax code
  (syntax-rules ()
    ((_ xx)
     (lambda () xx))))

(define-syntax with-unknown-option-catch
  (syntax-rules ()
    ((_ (opt) code ...)
     (catch 'unknown-option
       (lambda ()
         code ...)
       (lambda (key . args)
         (die "Unknown option: ~s\n" (symbol->string (quote opt))))))))

(define-syntax get-opt
  (syntax-rules ()
    ((_ opt)
     (with-unknown-option-catch (opt)
       (internal/get-option (quote opt))))))

(define-syntax set-opt
  (syntax-rules ()
    ((_ opt val)
     (with-unknown-option-catch (opt)
       (catch 'broken-value
         (lambda ()
           (internal/set-option (quote opt) val))
         (lambda (key . args)
           (die "Broken value for `~a': ~s\n"
                (quote opt) val)))))))

(define valid-account-types '(as12))

(define* (add-account #:key
                      name
                      user
                      password
                      uri
                      (options '())
                      (port 80)
                      (max-submissions 50)
                      (type 'as12)
                      (active #t))
  (verify-type 'add-account name symbol?)
  (verify-type 'add-account options list?)
  (verify-type 'add-account password string?)
  (verify-type 'add-account port integer?)
  (verify-type 'add-account type symbol?)
  (verify-type 'add-account uri string?)
  (verify-type 'add-account max-submissions integer?)
  (verify-type 'add-account user string?)
  (verify-type 'add-account active boolean?)
  (if (not (memq type valid-account-types))
      (die "\n -!- add-account: Invalid account type: `~a'.\n\n"
           (symbol->string type)))
  (let ((new (hashq-set! accounts name (make-hash-table 6))))
    (hashq-set! new 'options options)
    (hashq-set! new 'password password)
    (hashq-set! new 'port port)
    (hashq-set! new 'type type)
    (hashq-set! new 'uri uri)
    (hashq-set! new 'max-submissions max-submissions)
    (hashq-set! new 'user user)
    (hashq-set! new 'active active)))

(define valid-action-types '(rewrite))

(define* (add-action #:key
                     name
                     type
                     code)
  (verify-type 'add-action name symbol?)
  (verify-type 'add-action type symbol?)
  (verify-type 'add-action code thunk?)
  (if (not (memq type valid-action-types))
      (die "\n -!- add-action: Invalid action type: `~a'.\n\n"
           (symbol->string type)))
  (let ((new (hashq-set! actions name (make-hash-table 2))))
    (hashq-set! new 'type type)
    (hashq-set! new 'code code)))

(define-syntax add-predicate-maybe
  (syntax-rules ()
    ((_ var)
     (if var var '()))
    ((_ var pred)
     (if var pred '()))))

(define* (match-entry #:key
                      accounts
                      artist
                      album
                      track
                      predicate)
  (or (eq? accounts #f)
      (verify-type 'match-entry accounts list?))
  (or (eq? artist #f)
      (verify-type 'match-entry artist string?))
  (or (eq? album #f)
      (verify-type 'match-entry album string?))
  (or (eq? track #f)
      (verify-type 'match-entry track string?))
  (let ((new (make-hash-table))
        (predicates
         (filter (lambda (x) (not (null? x)))
                 (list
                  (add-predicate-maybe album (cons matcher/album album))
                  (add-predicate-maybe artist (cons matcher/artist artist))
                  (add-predicate-maybe track (cons matcher/track track))
                  ;; The ‘predicate’ predicate is potentially the most
                  ;; expensive one to call, so do it at the end!
                  (add-predicate-maybe predicate)))))
    (hashq-set! new 'accounts (or accounts #f))
    (hashq-set! new 'predicates predicates)
    (set! matchers (append matchers (list new))))
  matchers)

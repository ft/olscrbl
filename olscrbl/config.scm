(define-module (olscrbl config)
  #:use-module (ice-9 pretty-print)
  #:use-module (ice-9 format)
  #:use-module (ice-9 optargs)
  #:use-module (olscrbl config internal)
  #:use-module (olscrbl matchers)
  #:use-module (olscrbl reader)
  #:use-module (olscrbl utils)
  #:export (;; functions
            add-account
            add-action
            match-entry
            register-reader
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

(define-syntax verify-type
  (syntax-rules()
    ((_ caller var predicate)
     (if (or (predicate var)
             (thunk? var))
         #t
         (let* ((p (symbol->string (quote predicate)))
                (l (- (string-length p) 1)))
           (die
            "\n -!- ~a: `~a' needs to be a ~a (or a thunk producing one).\n\n"
            (symbol->string caller)
            (symbol->string (quote var))
            (substring p 0 l)))))))

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
                      (type 'as12))
  (verify-type 'add-account name symbol?)
  (verify-type 'add-account options list?)
  (verify-type 'add-account password string?)
  (verify-type 'add-account port integer?)
  (verify-type 'add-account type symbol?)
  (verify-type 'add-account uri string?)
  (verify-type 'add-account max-submissions integer?)
  (verify-type 'add-account user string?)
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
    (hashq-set! new 'user user)))

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
  (set! matchers
        (append
         matchers
         (list
          (list
           accounts
           (let ((new '()))
             (if predicate (set! new (cons predicate new)))
             (if track (set! new (cons (cons matcher/track track) new)))
             (if album (set! new (cons (cons matcher/album album) new)))
             (if artist (set! new (cons (cons matcher/artist artist) new)))
             new))))))

(define* (register-reader #:key
                          type
                          read-record
                          parse-record
                          valid-data
                          extract-data)
  (verify-type 'register-reader type (lambda (t)
                                       (and (not (eq? t #f))
                                            (symbol? t))))
  (verify-type 'register-reader read-record procedure?)
  (verify-type 'register-reader parse-record procedure?)
  (verify-type 'register-reader valid-data procedure?)
  (verify-type 'register-reader extract-data procedure?)
  (reader-set-proc type 'read-record read-record)
  (reader-set-proc type 'parse-record parse-record)
  (reader-set-proc type 'valid-data valid-data)
  (reader-set-proc type 'extract-data extract-data))

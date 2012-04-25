(define-module (olscrbl config internal)
  :export (accounts
           actions
           matchers
           options
           get-opt-unsafe
           internal/get-option
           internal/initialise-options
           internal/set-option))

(use-modules (ice-9 format))
(use-modules (ice-9 pretty-print))

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

(define (internal/initialise-options)
  (hash-set! options 'log-file-type 'scrobbler-log)
  (hash-set! options 'unmatched-entries 'keep)
  (hash-set! options 'note-default-matcher #t))

(define valid/unmatched-entries '(keep drop))

(define (broken-value key value)
  (cond ((eq? key 'unmatched-entries)
         (eq? (memq value valid/unmatched-entries)
              #f))
        ((eq? key 'note-default-matcher)
         (not (boolean? value)))
        ((eq? key 'log-file-type)
         (not (symbol? value)))
        (else
         (format #t "Missing verification fct for `~a'.\n" key)
         #f)))

(define (internal/set-option key value)
  (with-option (key oldvalue)
    (if (broken-value key value)
        (throw 'broken-value))
    (hash-set! options key value)))

(define (internal/get-option key)
  (with-option (key value)
    (cdr value)))

(define (get-opt-unsafe key)
  (hash-ref options key))

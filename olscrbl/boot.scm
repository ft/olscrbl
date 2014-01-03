(define-module (olscrbl boot)
  #:use-module (ice-9 format)
  #:use-module (ice-9 getopt-long)
  #:use-module (ice-9 pretty-print)
  #:use-module (olscrbl config)
  #:use-module (olscrbl config internal)
  #:use-module (olscrbl definitions)
  #:use-module (olscrbl main)
  #:export (boot))

(define optspec '((version (single-char #\v))
                  (help (single-char #\h))
                  (config (single-char #\c) (value #t))
                  (filetype (single-char #\f) (value #t))
                  (accounts (single-char #\r) (value #t))
                  (query (single-char #\q) (value #t))
                  (album (single-char #\a) (value #t))
                  (artist (single-char #\A) (value #t))
                  (duration (single-char #\d) (value #t))
                  (musicbrainz (single-char #\M) (value #t))
                  (timestamp (single-char #\T) (value #t))
                  (track (single-char #\t) (value #t))
                  (tracknumber (single-char #\n) (value #t))))

(define (usage)
  (format #t "usage: ~a [OPTION(s)] <fetch|poll>\n" *program-name*))

(define (version)
  (format #t "~a version ~a\n" *program-name* *program-version*))

(define (set-opt-if-option-set setting options opt transformer)
  (let ((value (option-ref options opt #f)))
    (if (string? value)
        (begin (set-opt 'setting (transformer value))))))

;; Handle options, read configuration and call the main program.
(define (boot x)
  (internal/initialise-options)
  (let* ((guile (car x))
         (argv (cdr x))
         (prg (cadr x))
         (options (getopt-long argv optspec))
         (load-quiet (if (string= (or (getenv "OLSCRBL_LOAD_QUIET")
                                      "f")
                                  "t")
                         #t
                         #f)))
    (set! *guile-binary* guile)
    (set! *program-name* prg)
    (set! *program-args* (cdr argv))
    (set! *program-args-non-options* (option-ref options '() '()))
    ;; Some options, like `--help' or `--version' need to be dealt with
    ;; immediately. Others need to be handled after the user's configuration
    ;; file is loaded, so they can override settings.
    (if (option-ref options 'help #f)
        (begin (usage)
               (quit 0)))
    (if (option-ref options 'version #f)
        (begin (version)
               (quit 0)))
    ;; Load the user's init file.
    (let ((file (option-ref options 'config *init-file*)))
      (catch
        #t
        (lambda ()
          ;; Thunk: Try to load the user's init file.
          (if (file-exists? file)
              (begin
                (if (not load-quiet)
                    (format #t "Loading cfg: `~a'...\n" file))
                (primitive-load file))
              (format #t "No configuration file found, at: `~a'\n"
                      file)))
        (lambda (key . args)
          ;; Error-handler: Ask user if we should go on.
          (let ((reason (symbol->string key)))
            (format #t "\n  Caught exception (~a) while\n  reading `~a'.\n\n"
                    (symbol->string key)
                    file)
            (format #t "~a\n\n" args)
            (if (string= reason "quit")
                (quit 1))
            ;; TODO: Need to close&reopen stdin if it's not a terminal
            (format #t "Hit <ENTER> to continue or <CTRL-C> to abort.\n\n")
            (let nc ((c #\a))
              (if (eq? c #\newline)
                  #t
                  (nc (read-char (current-input-port)))))))
        (lambda (key . args)
          ;; Pre-unwind-handler: Dump backtrace.
          (let ((reason (symbol->string key)))
            (if (string= reason "quit")
                (quit 1))
            (display-backtrace (make-stack #t)
                               (current-output-port)))))))
  (set-opt-if-option-set 'log-file-type options 'filetype string->symbol)
  ;; Initialisation done. Load the actual program.
  (olscrbl-main))

(define-module (olscrbl boot)
  #:use-module (ice-9 format)
  #:use-module (ice-9 getopt-long)
  #:use-module (ice-9 pretty-print)
  #:use-module (olscrbl terminal-io)
  #:use-module (olscrbl config)
  #:use-module (olscrbl config internal)
  #:use-module (olscrbl definitions)
  #:use-module (olscrbl main)
  #:export (boot))

(define optspec '((version (single-char #\v))
                  (licence (single-char #\L))
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

(define olscrbl-licence
  '(""
    " Copyright 2011-2013 olscrbl workers, All rights reserved."
    ""
    " Redistribution and use in source and binary forms, with or without"
    " modification, are permitted provided that the following conditions"
    " are met:"
    ""
    " 1. Redistributions of source code must retain the above copyright"
    "    notice, this list of conditions and the following disclaimer."
    " 2. Redistributions in binary form must reproduce the above copyright"
    "    notice, this list of conditions and the following disclaimer in the"
    "    documentation and/or other materials provided with the distribution."
    ""
    " THIS SOFTWARE IS PROVIDED \"AS IS\" AND ANY EXPRESS OR IMPLIED WARRANTIES,"
    " INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY"
    " AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.   IN NO EVENT SHALL"
    " THE AUTHOR OR CONTRIBUTORS OF THE PROJECT BE LIABLE FOR ANY DIRECT,  IN-"
    " DIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLU-"
    " DING,  BUT NOT LIMITED TO,  PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;"
    " LOSS OF USE, DATA, OR PROFITS;  OR BUSINESS INTERRUPTION) HOWEVER CAUSED"
    " AND ON ANY THEORY OF LIABILITY,  WHETHER IN CONTRACT,  STRICT LIABILITY,"
    " OR TORT (INCLUDING NEGLIGENCE  OR  OTHERWISE) ARISING IN ANY WAY OUT OF"
    " THE USE OF THIS SOFTWARE,  EVEN IF ADVISED  OF  THE POSSIBILITY OF SUCH"
    " DAMAGE."
    ""))

(define option-help
  '(("--help, -h"
     "Display this help message.")

    ("--version, -v"
     "Display version information and compile-time"
     "configuation.")

    ("--licence, -L"
     "Display licencing and copyright information.")

    ("--query <QUERY>, -q <QUERY>"
     "Query information from configuration file.")

    ("--accounts <ACCOUNTS>, -r <ACCOUNTS>"
     "Specify a comma separated list of accounts to enable.")

    ("--filetype <TYPE>, -f <TYPE>"
     "Specify the type of the input log file.")

    ("\n  Single-Track submission options:\n")

    ("--album <ALBUM>, -a <ALBUM>"
     "Name of the album of the track.")

    ("--artist <ARTIST>, -A <ARTIST>"
     "Name of the artist of the track.")

    ("--duration <DURATION>, -d <DURATION>"
     "Duration of the track to be submitted.")

    ("--musicbrainz <MUSIC-BRAINZ>, -M <MUSIC-BRAINZ>"
     "MusicBrainz-ID of the track to be submitted.")

    ("--timestamp <TIME-STAMP>, -T <TIME-STAMP>"
     "Unix timestamp in UTC timezone of the time when the"
     "track was played.")

    ("--track <TRACK-NAME>, -t <TRACK-NAME>"
     "Name of the track.")

    ("--tracknumber <TRACK-NUMBER>, -n <TRACK-NUMBER>"
     "Number of the track on the original medium (usually a"
     "compact disc or gramophone record).")))

(define option-max-width 16)

;; `opt-help' is a list describing an option. The first item a string listing
;; the option itself and its one-letter version. The second string is the first
;; line of description for the option. If the first item's string-length is not
;; larger than `width', the second item of `opt-help' is rendered next to the
;; first item. If not, it's rendered on a line of its own.
;;
;; If there are more items, they all should be strings, each representing one
;; line of description for the option.
;;
;;   (generate-help-for-opt 12 '("--foo, -f" "first line" "second"))
;;     => ("      --foo, -f     first line\n"
;;         "                    second\n")
;;
;; `usage' uses this function along with `option-help' to handle the program's
;; `--help' option.
(define (generate-help-for-opt width opt-help)
  (let* ((o (car opt-help))
         (l (string-length o))
         (too-long (> l width))
         (rest (if too-long
                   (cdr opt-help)
                   (cddr opt-help))))
    (cons
     (if (not too-long)
         (format #f "      ~a~v_  ~a~%" o (- width l) (cadr opt-help))
         (format #f "      ~a~%" o))
     (if (null? rest)
         '()
         (map (lambda (s)
                (format #f "      ~v_  ~a~%" width s))
              rest)))))

(define (usage)
  (io "usage: ~a [OPTION(s)]~%~%" *program-name*)
  (io "  Options:~%~%")
  (for-each (lambda (opt)
              (for-each display
                        (generate-help-for-opt option-max-width opt)))
            option-help)
  (newline))

(define (licence)
  (just-version)
  (for-each (lambda (line)
              (display line)
              (newline))
            olscrbl-licence))

(define (just-version)
  (io "~a version ~a~%" *program-name* *program-version*)
  (if (string-null? *program-code-name*)
      ""
      (io "code-name: ~a~%" *program-code-name*)))

(define (build-time desc value)
  (io "  ~32@a: ~a~%"
          desc
          (cond ((boolean? value) (if value "ON" "OFF"))
                (else value))))

(define (version)
  (just-version)
  (newline)
  (build-time "olscrbl configuration directory" *olscrbl-directory*))

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
    (set! *program-name* (basename prg))
    (set! *program-args* (cdr argv))
    (set! *program-args-non-options* (option-ref options '() '()))
    ;; Some options, like `--help' or `--version' need to be dealt with
    ;; immediately. Others need to be handled after the user's configuration
    ;; file is loaded, so they can override settings.
    (if (option-ref options 'help #f)
        (begin (usage)
               (quit 0)))
    (if (option-ref options 'licence #f)
        (begin (licence)
               (quit 0)))
    (if (option-ref options 'version #f)
        (begin (version)
               (quit 0)))
    ;; Load the user's init file.
    (let ((file (option-ref options 'config *init-file*)))
      (if (file-exists? file)
          (begin (unless load-quiet
                   (io "Loading cfg: `~a'...\n" file))
                 (let ((cfg-mod (resolve-module '(olscrbl config))))
                   (save-module-excursion
                    (lambda ()
                      (set-current-module cfg-mod)
                      (primitive-load file)))))
          (io "No configuration file found, at: `~a'\n"
                  file))))
  (set-opt-if-option-set 'log-file-type options 'filetype string->symbol)
  ;; Initialisation done. Load the actual program.
  (olscrbl-main))

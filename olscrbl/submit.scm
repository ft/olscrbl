(define-module (olscrbl submit)
  :use-module (ice-9 rdelim)
  :use-module (olscrbl matchers)
  :use-module (olscrbl reader)
  :export (submit/setup-reader
           submit-with-file
           submit-with-port))

(define read-record #f)
(define parse-record #f)
(define valid-data? #f)
(define extract-data #f)

(define submissions #f)

(define (submit-with-file file)
  (let ((fh (open-input-file file)))
    (submit-with-port fh)
    (close-port fh)))

(define (submit-with-port port)
  (submit-tracks
   (let loop ((s '()))
     (let ((rec (read-record port)))
       (cond
        ((eof-object? rec) s)
        (else
         (let ((dat (parse-record rec)))
           (loop (if (valid-data? dat)
                     (append s (list dat))
                     ;; Ignore invalid entries.
                     s)))))))))

(define (submit-tracks tracks)
  ;; TODO: This will have to be extended to handle multiple accounts
  ;; concurrently in multiple threads.
  (for-each (lambda (trk)
              (format #t "Track: ~s\n" (extract-data trk 'track)))
            (filter (lambda (dat)
                      (not (run-matchers
                            'last-fm ; This is a hack, see TODO above.
                            dat)))
                    tracks)))

(define (submit/setup-reader type)
  (set! read-record (reader-get-proc type 'read-record))
  (set! parse-record (reader-get-proc type 'parse-record))
  (set! valid-data? (reader-get-proc type 'valid-data))
  (set! extract-data (reader-get-proc type 'extract-data)))

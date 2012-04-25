(define-module (olscrbl scrobbler-log)
  :use-module (ice-9 rdelim)
  :use-module (olscrbl config)
  :export (extract-data
           parse-record
           read-record
           valid-data?
           initialise-scrobbler-reader))

(define (read-record port)
  (read-line port 'trim))

(define (parse-record rec)
  (string-split rec #\tab))

(define (valid-data? dat)
  (= (length dat) 8))

(define datidx (make-hash-table 8))
(hash-set! datidx 'artist 0)
(hash-set! datidx 'album 1)
(hash-set! datidx 'track 2)
(hash-set! datidx 'tracknumber 3)
(hash-set! datidx 'duration 4)
(hash-set! datidx 'status 5)
(hash-set! datidx 'timestamp 6)
(hash-set! datidx 'musicbrainz 7)

(define (extract-data dat what)
  (let ((idx (hash-ref datidx what)))
    (cond ((not idx) (throw 'unknown-data-entity))
          (else (list-ref dat (hash-ref datidx what))))))

(define (initialise-scrobbler-reader)
  (register-reader :type 'scrobbler-log
                   :read-record read-record
                   :parse-record parse-record
                   :valid-data valid-data?
                   :extract-data extract-data))

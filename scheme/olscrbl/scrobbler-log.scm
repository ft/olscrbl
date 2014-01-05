(define-module (olscrbl scrobbler-log)
  #:use-module (ice-9 rdelim)
  #:use-module (olscrbl config utils)
  #:export (extract-data
            parse-record
            read-record
            produce-record
            valid-data?
            initialise-scrobbler-reader))

(define (read-record port)
  (read-line port 'trim))

(define (parse-record rec)
  (let ((retval (string-split rec #\tab)))
    (if (valid-data? retval)
        (map (lambda (x)
               (set-data! retval
                          x
                          (string->number (extract-data retval x))))
             '(timestamp duration)))
    retval))

(define (valid-data? dat)
  (= (length dat) 8))

(define (produce-record dat)
  (format #f
          "~a~/~a~/~a~/~a~/~a~/~a~/~a~/~a"
          (extract-data dat 'artist)
          (extract-data dat 'album)
          (extract-data dat 'track)
          (extract-data dat 'tracknumber)
          (extract-data dat 'duration)
          (extract-data dat 'status)
          (extract-data dat 'timestamp)
          (extract-data dat 'musicbrainz)))

(define datidx (make-hash-table 8))
(hashq-set! datidx 'artist 0)
(hashq-set! datidx 'album 1)
(hashq-set! datidx 'track 2)
(hashq-set! datidx 'tracknumber 3)
(hashq-set! datidx 'duration 4)
(hashq-set! datidx 'status 5)
(hashq-set! datidx 'timestamp 6)
(hashq-set! datidx 'musicbrainz 7)

(define (extract-data dat what)
  (let ((idx (hashq-ref datidx what)))
    (cond ((not idx) (throw 'unknown-data-entity))
          (else (list-ref dat (hashq-ref datidx what))))))

(define (valid-keys)
  '(artist album track tracknumber duration status timestamp musicbrainz))

(define (set-data! track key value)
  (set-car! (list-tail track (hashq-ref datidx key)) value))

(define (initialise-scrobbler-reader)
  (register-reader #:type 'scrobbler-log
                   #:read-record read-record
                   #:parse-record parse-record
                   #:valid-data? valid-data?
                   #:valid-keys valid-keys
                   #:extract-data extract-data
                   #:set-data! set-data!
                   #:produce-record produce-record))

(define-module (olscrbl submit)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 streams)
  #:use-module (ice-9 optargs)
  #:use-module (ice-9 pretty-print)
  #:use-module (ice-9 threads)
  #:use-module (web client)
  #:use-module (web response)
  #:use-module (web uri)
  #:use-module (srfi srfi-1)
  #:use-module (olscrbl matchers)
  #:use-module (olscrbl reader)
  #:use-module (olscrbl utils)
  #:use-module (olscrbl md5)
  #:use-module (olscrbl config utils)
  #:use-module (olscrbl config internal)
  #:export (submit/setup-reader
            submit-with-file
            submit-with-port))

(define read-record #f)
(define parse-record #f)
(define valid-data? #f)
(define extract-data #f)
(define produce-record #f)

;; Turns a file name into a port and applies `submit-with-port' to it.
(define (submit-with-file file)
  (let ((fh (open-input-file file)))
    (submit-with-port fh)
    (close-port fh)))

;; This function generates a list of tracks from the input at `port'. It then
;; calls `submit-tracks' with that list as an argument. Invalid records are
;; ignored.
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

;; Call `submit-tracks-for-account' for every active account.
(define (submit-tracks tracks)
  (let* ((active-accounts (filter account-active?
                                  (get-accounts)))
         (n (length active-accounts)))
    (cond ((= n 1)
           (submit-tracks-for-account (car active-accounts) tracks))
          (else
           (n-par-for-each (length active-accounts)
                           (lambda (account)
                             (submit-tracks-for-account account tracks))
                           active-accounts)))))

;; Returns a function that can be used with `make-stream' to create a stream of
;; tracks. Streams like that return chunks of a larger list that are at most
;; `max' entries long.
(define (make-submissions-stream-proc max)
  (lambda (state)
    (let* ((l (length state))
           (n (if (>= l max)
                  max l)))
      (cond ((null? state) #f)
            (else
             (cons (take state n)
                   (drop state n)))))))

(define submission-argument
  "&a[~d]=~a&t[~d]=~a&b[~d]=~a&l[~d]=~a&n[~d]=~a&i[~d]=~a&o[~d]=P&r[~d]=&m[~d]=")

(define (generate-submissions session-id tracks)
  (cdr
   (fold (lambda (new prev)
           (let ((num (car prev))
                 (l (cdr prev)))
             (cons (+ 1 num)
                   (string-append l (format #f submission-argument
                                            num (extract-data new 'artist)
                                            num (extract-data new 'track)
                                            num (extract-data new 'album)
                                            num (extract-data new 'duration)
                                            num (extract-data new 'tracknumber)
                                            num (extract-data new 'timestamp)
                                            num num num)))))
         `(0 . ,(format #f "s=~a&portable=1" session-id))
         tracks)))

(define (submit-tracks-for-account account tracks)
  (let ((uri (string->uri (generate-scrobbling-handshake account))))
    (format #t " -!- handshake: ~s~%" (generate-scrobbling-handshake account))
    (receive (h b) (http-get uri)
      (and (not (= 200 (response-code h)))
           (throw 'scrobble-http-handshake-non-200))
      (format #t " -!- body: ~s~%" b)
      (let* ((body (string-split b #\newline))
             (status (car body)))
        (cond ((not (string=? status "OK"))
               (format #t "Handshake failed (~a).~%" status))
              (else
               (format #t " -!- status: ~a~%" status)
               (do-submit #:account account
                          #:session-id (cadr body)
                          #:submit-uri-string (cadddr body)
                          #:tracks tracks)))))))

(define* (do-submit #:key
                    account
                    session-id
                    submit-uri-string
                    tracks)
  ;; The trick here, is to turn `tracks' into a data stream, from which you get
  ;; a list of at most `max-submissions' tracks each time you pick something
  ;; from it.
  (let* ((proc (make-submissions-stream-proc
                (account-parameter account 'max-submissions)))
         (track-stream (make-stream proc
                                    (filter (lambda (dat)
                                              (not (run-matchers account dat)))
                                            tracks))))
    (format #t " -!- session-id: ~a~%" session-id)
    (format #t " -!- submit-uri-string: ~a~%" submit-uri-string)
    (stream-for-each
     (lambda (chunk)
       (let ((body (generate-submissions session-id chunk)))
         (receive (r response)
             (http-post submit-uri-string
                        #:body body
                        #:headers '((content-type
                                     . (application/x-www-form-urlencoded))))
           response)))
     track-stream)))

(define protocol-charset
  (string->char-set (string-concatenate '("-.:=/?&"
                                          "0123456789"
                                          "abcdefghijklmnopqrstuvwxyz"
                                          "ABCDEFGHIJKLMNOPQRSTUVWXYZ"))))

;; Generates a handshake URI for `account'.
(define (generate-scrobbling-handshake account)
  (let* ((proto-ver "1.2.1")
         (a (get-account account))
         (uri (extract-parameter a 'uri))
         (port (extract-parameter a 'port))
         (user (extract-parameter a 'user))
         (pass (extract-parameter a 'password))
         (client-id "tst")
         (client-ver "0.2.1")
         (ts (timestamp))
         (token (md5 (format #f "~a~d" (md5 pass) ts))))
    (uri-encode
     (format #f "http://~a:~a/?hs=true&p=~a&c=~a&v=~a&u=~a&t=~a&a=~a"
             uri port proto-ver client-id client-ver user ts token)
     #:encoding "utf8"
     #:unescaped-chars protocol-charset)))

(define (submit/setup-reader type)
  (set! read-record (reader-get-proc type 'read-record))
  (set! parse-record (reader-get-proc type 'parse-record))
  (set! valid-data? (reader-get-proc type 'valid-data))
  (set! extract-data (reader-get-proc type 'extract-data))
  (set! produce-record (reader-get-proc type 'produce-record)))

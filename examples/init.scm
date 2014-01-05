;;; Example ‘olscrbl’ initialisation file.   -*- scheme -*-
;;; ...with annotations.

;; The configuration file is loaded in the realm of the main configuration
;; module (olscrbl config), which means that its API is available to us.

;; One part of the API is access to an option value name-space. This uses
;; ‘set-opt’ in order to tell ‘olscrbl’, that it doesn't have to tell us, if
;; it's using the default-matcher (see below for what a matcher, and also the
;; default one, is). This let's the program start less noisy.
(set-opt note-default-matcher #f)

;; Now let's add an account. This one is for the popular last.fm service, which
;; scrobbles via audioscrobbler.com:
(add-account :name 'last-fm
             :user "that-user-name-i-keep-forgetting"
             :password "and-my-user-secret-password"
             :uri "post.audioscrobbler.com"
             :port 80)

;; You can have more than one account. The free service from libre.fm uses the
;; same protocol as last.fm and thus ‘olscrbl’ works with it as well:
(add-account :name 'libre-fm
             :user "that-user-name-i-keep-forgetting"
             :password "and-my-user-secret-password"
             :uri "turtle.libre.fm"
             :port 80
             :max-submissions 2)

;; About matchers: They decide what to do with an entry from a ‘.scrobbler.log’
;; file. The default matcher matches *every* entry and marks them for
;; submission to all accounts you configured. ‘olscrbl’ allows the user
;; complete control over matchers, though. Let's discuss some examples.

;; Note again, that you don't have to define ANY matcher at all, if you just
;; want to submit everything from the log-file, because that is exactly what
;; the default matcher (which is used if no other matcher is defined) does.

;; Let's say, you got this guilty pleasure of listening to music by some pop
;; starlett, and you really don't want your über-trüe metal friends, to know.
;; You can weed those entries out:
(match-entry :artist "Spitney Beers"
             :action 'drop)

;; Now, to submit everything else, use the trivial matcher. Matchers are
;; processed in the order in which they are defined in here, so this matcher is
;; only seen by entries that were *not* weeded out by the previour
;; drop-matcher.
;;
;; Note, that the default :action parameter is 'submit and the default
;; :predicate is always true.
(match-entry)

;; The above example was pretty simple. Let's do something a little more
;; involved. First of all, let's load another module, that let's us examine the
;; date structures, for the entry data that ‘olscrble’ hands around:
(use-modules (olscrbl data-access))

;; The next example doesn't make much sense with the (match-entry) call in
;; place, because that would match everything that's left and so the code below
;; would never see any of the entries at all.

;; Now, if you're like me and listen to - in addition to music - a lot of
;; audio-books, podcasts and the like with your rockbox player, and you only
;; want to scrobble the music you listen to, you got three choices: a) Turn off
;; .scrobbler.log logging in the players preferences before listening to tracks
;; you don't want to scrobble; b) Weed out tracks you don't want to scrobble
;; manually; and c) Weed out tracks in an automated fashion by writing a
;; program, that detects whether or not an entry is music or not.
;;
;; You could obviously strip the ‘.scrobbler.log’ file before feeding it into
;; ‘olscrbl’, but since ‘olscrbl’ already knows how to read ‘.scrobbler.log’
;; files and offers a matching framework, it makes sense to use that instead.
;;
;; Here is an example, that demonstrates how such automation can work.
;;
;; This matcher works like this:
;;
;; 1) Get the name of the artist from track-metadata.
;; 2) See if there is a directory ~/audio/music/<artist>
;;
;; If step two succeeds, olscrbl will submit the track in question. Otherwise
;; it'll be ignored (unless another matcher decides to submit it).
(match-entry
 :predicate (lambda (data)
              ;; The :predicate key-word let's us define arbitrary scheme code,
              ;; to decide whether or not entries should match. It takes a
              ;; function of one argument (which will be set to the internal
              ;; data, that you can examine with the appropriate API calls,
              ;; like ‘extract-data’). This may be a named function, or an
              ;; anonymous function, like it's done here.
              (let* ((artist (extract-data data 'artist))
                     (prefix (string-concatenate
                              (list (getenv "HOME") "/audio/music/")))
                     (directory (string-concatenate (list prefix artist))))
                (file-exists? directory))))

;; Lastly, there is one other issue with offline track submission to these
;; scrobbling services. And that is that sometimes, you carry around your
;; rockbox player for way too long without scrobbling the data from
;; ‘.scrobbler.log’. The scrobbling services will then silently ignore tracks
;; that are too old.
;;
;; The maximum age is somewhere around half a month, as far as I know. So let's
;; try to work around that.

;; First some names for data, we'll use...
(define back-off (* 2 24 60 60))        ; Seconds in two days.
(define offset (* 7 back-off))          ; Seconds in two weeks.
(define right-now (current-time))

;; Here is a function that determines if a time-stamp is too old.
(define (too-old? ts)
  (> (- right-now ts)
     offset))

;; And finally, the pre-processor definition. A post-processor is just a
;; function, that takes a value and returns a - possibly altered - version of
;; it. This one checks if the original time-stamp is too old, and if so
;; replaces it with a time-stamp that's two days old. If the original is *not*
;; too old, it's kept as it was.
(pre-process 'timestamp (lambda (ts)
                          (if (too-old? ts)
                              (- right-now back-off)
                              ts)))

(define-module (olscrbl definitions)
  #:use-module (olscrbl build-configuration)
  #:export (*home-directory*
            *olscrbl-directory*
            *init-file*
            *guile-binary*
            *program-args*
            *program-args-non-options*
            *program-name*
            *program-version*))

(define *guile-binary* "*init*")
(define *program-args* '())
(define *program-name* "*init*")
(define *program-version* "*unversioned*")

(define *home-directory* (getenv "HOME"))

(define *olscrbl-directory*
  (string-concatenate
   (if (not build-cfg/use-xdg-paths)
       (list *home-directory* "/.olscrbl")
       (let ((xdg (getenv "XDG_CONFIG_HOME"))
             (dir "/olscrbl"))
         (if xdg
             (list xdg dir)
             (list *home-directory* "/.config" dir))))))

(define *init-file*
  (string-concatenate (list *olscrbl-directory* "/init.scm")))

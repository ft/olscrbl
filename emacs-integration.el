;; Load this file into emacs for proper indentation of the project's macros
;; within emacs' `scheme-mode'.
;;
;; This file is part of `olscrbl'.

(mapc (lambda (thing)
        (put (car thing)
             'scheme-indent-function
             (cdr thing)))
      '((with-unknown-option-catch . defun)))

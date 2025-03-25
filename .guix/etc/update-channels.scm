#!/usr/bin/env -S guix repl --
!#
(use-modules (ice-9 pretty-print)
             (guix scripts describe))

(define (main args)
  "Output updated channels description to STDOUT.
ARGS are the command-line arguments, which are presently ignored."
  (let* ((allchns (read (open-input-string
                         (with-output-to-string
                           (lambda () (guix-describe "-f" "channels"))))))
         ;; only guix channel is needed; ignore rest (if any)
         ;; NOTE: if we `eval' we could use record accessors from (guix channel)
         (chns (filter (lambda (x)
                         (or (eq? x 'list)
                             (and (list? x)
                                  (eq? (car x) 'channel)
                                  (equal? (cadr (assq 'name (cdr x)))
                                          '(quote guix)))))
                       allchns)))
    (display ";; NOTE: Automatically generated via 'make guix-profile-update'")
    (newline)
    (pretty-print chns)))

;; invoke main optionally
(let* ((cmdln (command-line))
       (script (car cmdln)))
  ;; when loading file in interpreter, do nothing
  (unless (member (basename script) '("guile" "guix"))
    (main cmdln)))


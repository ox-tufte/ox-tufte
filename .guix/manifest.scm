(add-to-load-path (string-append (dirname (current-filename)) "/modules"))

(use-modules (gnu packages emacs)
             (guix packages)
             (weary-travelers packages ox-tufte-package))

(concatenate-manifests
 (list (packages->manifest (list emacs-ox-tufte-dev emacs))))



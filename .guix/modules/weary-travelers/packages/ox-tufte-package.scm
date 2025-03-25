;;; Guix package definition for git-email
;;;
;;; Copyright (C) 2021-2023  all contributors <~yoctocell/git-email-devel@lists.sr.ht>
;;; Copyright (C) 2024  Suhail <suhail@bayesians.ca>
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

(define-module (weary-travelers packages ox-tufte-package)
  #:use-module (guix packages)
  #:use-module (guix build emacs-utils)
  #:use-module (guix build utils)
  #:use-module (guix build-system emacs)
  #:use-module (guix gexp)
  #:use-module (guix profiles)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages emacs-xyz)
  ;;#:use-module (gnu packages base)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim))

;;; Commentary:
;;;
;;; This file contains the package definitinon for git-email.  Run
;;; `guix shell -D -f guix.scm' to create a development environment.
;;;
;;; Code:

(define* (git-output source-dir #:rest args)
  "Execute 'git ARGS ...' command and return its output without trailing
newspace."
  (with-directory-excursion source-dir
    (let* ((port   (apply open-pipe* OPEN_READ "git" args))
           (output (read-string port)))
      (close-port port)
      (string-trim-right output #\newline))))

(define (current-commit source-dir)
  "The commit corresponding to HEAD.
SOURCE-DIR is some path within an existing git repository."
  (git-output source-dir "rev-parse" "HEAD"))

(define (current-revision source-dir)
  "Returns (version,revision) tuple.
SOURCE-DIR is some path within an existing git repository."
  (list-head (string-split (git-output source-dir "describe" "--tags") #\-) 2))

(define-public emacs-ox-tufte-dev
  (let* ((source-dir (emacs-batch-script
                      '(princ
                        (directory-file-name
                         (expand-file-name
                          (locate-dominating-file default-directory ".git"))))))
         (commit (current-commit source-dir))
         (ver-rev (current-revision source-dir))
         (version (car ver-rev))
         (revision (cadr ver-rev)))
   (package
     (name "emacs-ox-tufte-dev")
     (version (git-version version revision commit))
     (source (local-file source-dir
                         #:recursive? #t
                         #:select? (git-predicate source-dir)))
     (build-system emacs-build-system)
     (arguments
      (list
       #:include #~(cons "^src/" %default-include)
       #:tests? #t
       #:test-command #~(list "eldev" "--use-emacsloadpath" "-dtTC" "test")))
     (native-inputs (list emacs-buttercup emacs-eldev))
     (propagated-inputs (list emacs-org))
     (home-page "https://github.com/ox-tufte/ox-tufte")
     (synopsis "Tufte HTML Org mode export backend")
     (description
      "This is an export backend for Org mode that exports buffers to HTML that
is compatible with Tufte
CSS (@url{https://edwardtufte.github.io/tufte-css/}).")
     (license license:gpl3+))))

emacs-ox-tufte-dev

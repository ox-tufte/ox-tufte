;;; ox-tufte-test.el --- Tests for ox-tufte.el -*- lexical-binding: t; -*-

;; Copyright (C) 2023      The Bayesians Inc.

;; Author: The Bayesians Inc.
;; URL: https://github.com/ox-tufte/ox-tufte

;; This file is not part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(eval-when-compile
  (require 'ert))

(require 'ox-tufte)
(ox-tufte-init)


;;; utilities
(defmacro org-test-with-temp-text (text &rest body)
  "Run BODY in a temporary buffer with Org mode as the active mode holding TEXT.
If the string \"<point>\" appears in TEXT then remove it and
place the point there before running BODY, otherwise place the
point at the beginning of the inserted text."
  (declare (indent 1) (debug t))
  `(let ((inside-text (if (stringp ,text) ,text (eval ,text)))
	     (org-mode-hook nil))
     (with-temp-buffer
       (org-mode)
       (let ((point (string-match "<point>" inside-text)))
	     (if point
	         (progn
	           (insert (replace-match "" nil nil inside-text))
	           (goto-char (1+ (match-beginning 0))))
	       (insert inside-text)
	       (goto-char (point-min))))
       (font-lock-ensure (point-min) (point-max))
       ,@body)))

(defmacro org-tufte-test-in-exported-buffer (text body-only &rest body)
  "Execute BODY in buffer containing exported result of TEXT.
BODY-ONLY controls whether entire html page is exported or only
the body."
  `(org-test-with-temp-text
    ,text
    (let ((export-buffer "*Org Tufte Export*")
          (org-export-show-temporary-export-buffer nil))
      (org-tufte-export-as-html
       nil nil nil ,body-only nil)
      (with-current-buffer export-buffer
        ,@body))))

(defun org-tufte-test-debug (text &optional body-only)
  "Debug the result of exporting TEXT.
BODY-ONLY controls whether entire html page is exported or only
the body."
  (should
   (org-tufte-test-in-exported-buffer
    text body-only
    (progn
      (message "'%s'" (buffer-string))
      t))))


;;; tests
(ert-deftest ox-tufte/mathjax-path-none ()
  "Test that MathJax does not load when not needed."
  (should-not
   (org-tufte-test-in-exported-buffer
    "No LaTeX here." nil
    (let ((case-fold-search t))
      (search-forward "MathJax" nil t)))))

(ert-deftest ox-tufte/mathjax-as-default ()
 "Test that MathJax is the default LaTeX renderer."
 (should
  (org-tufte-test-in-exported-buffer
   "$x$" nil
   (let ((case-fold-search t))
     (search-forward "MathJax" nil t)))))


(ert-deftest ox-tufte/footnotes-section-disabled-default ()
 "No Footnotes section by default."
 (should-not
  (org-tufte-test-in-exported-buffer
   "pre[fn::sidenote] post" t
   (let ((case-fold-search t))
      (search-forward "Footnotes: " nil t)))))

(ert-deftest ox-tufte/footnotes-section-configurable-per-file ()
 "Footnotes section can be enabled as needed."
 (should
  (org-tufte-test-in-exported-buffer
   "#+OPTIONS: footnotes-section-p:t
pre[fn::sidenote] post" t
   (let ((case-fold-search t))
     (search-forward "Footnotes: " nil t))))
 (should-not
  (org-tufte-test-in-exported-buffer
   "#+OPTIONS: footnotes-section-p:nil
pre[fn::sidenote] post" t
   (let ((case-fold-search t))
     (search-forward "Footnotes: " nil t)))))


(ert-deftest ox-tufte/marginnote-as-link/limitations/nested-links-unsupported ()
  "Angle and plain links (including image links) are unsupported in
marginnote-as-link syntax."
  (should
   (org-tufte-test-in-exported-buffer
    "pre[[mn:][<file:./image.png>​]] post" t
    (let ((case-fold-search t))
      (search-forward "&lt;file:" nil t))))
  (should-not
   (org-tufte-test-in-exported-buffer
    "pre[[mn:][note file:./image.png]] post" t
    (let ((case-fold-search t))
      (search-forward "<img " nil t)))))

(ert-deftest ox-tufte/marginnote-as-link/macros-supported ()
  "Org macros are supported in marginnote-as-link syntax."
  (should
   (org-tufte-test-in-exported-buffer
    "#+MACRO: prefix $1 macro
pre[[mn:][pre {{{prefix(text)}}}]] post" t
    (let ((case-fold-search t))
      (search-forward "pre text macro" nil t)))))

(ert-deftest ox-tufte/marginnote-as-macro/links-supported ()
  "Links are supported in marginnote-as-macro syntax."
  (should
   (org-tufte-test-in-exported-buffer
    "pre{{{marginnote(<file:./image.png>​)}}} post" t
    (let ((case-fold-search t))
      (search-forward "<img " nil t)))))

(ert-deftest ox-tufte/marginnote/limitation/standalone-img-needs-zero-width-space ()
  "Standalone images in inline marginnotes require escaping."
  (should-not
   (org-tufte-test-in-exported-buffer
    "pre{{{marginnote(<file:./image.png>​)}}} post" t
    (let ((case-fold-search t))
      (search-forward "<figure " nil t))))
  (should
   (org-tufte-test-in-exported-buffer
    "pre{{{marginnote(<file:./image.png>)}}} post" t
    (let ((case-fold-search t))
      (search-forward "<figure " nil t)))))

(ert-deftest ox-tufte/marginnote-as-macro/limitation/nested-macros-supported ()
  "Nested macros are unsupported in marginnote-as-macro syntax."
  (should
   (org-tufte-test-in-exported-buffer
    "#+MACRO: prefix $1 macro
pre {{{marginnote(pre {{{prefix(text)}}})}}} post" t
    (let ((case-fold-search t))
      (search-forward "</span>)}}}" nil t)))))


(provide 'ox-tufte-test)
;;; ox-tufte-test.el ends here

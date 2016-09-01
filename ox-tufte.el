;;; ox-tufte.el --- Tufte

;; Copyright (C) 2016 Matthew Lee Hinman

;; Author: M. Lee Hinman
;; Keywords: org, tufte, html

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

;;; Commentary:

;; TODO: commentary

;; TODO:
;; - fullwidth class on some figures
;; - support #+CAPTION on inline images
;; - image quilts?

;;; Code:

(require 'ox)



;;; User-Configurable Variables

(defgroup org-export-tufte nil
  "Options specific to Tufte export back-end."
  :tag "Org Tufte"
  :group 'org-export
  :version "24.4"
  :package-version '(Org . "8.0"))


;;; Define Back-End

(org-export-define-derived-backend 'tufte-html 'html
  :filters-alist '((:filter-parse-tree . org-md-separate-elements))
  :menu-entry
  '(?T "Export to Tufte-HTML"
       ((?T "To temporary buffer"
            (lambda (a s v b) (org-tufte-export-to-buffer a s v)))
        (?t "To file" (lambda (a s v b) (org-tufte-export-to-file a s v)))
        (?o "To file and open"
            (lambda (a s v b)
              (if a (org-tufte-export-to-file t s v)
                (org-open-file (org-tufte-export-to-file nil s v)))))))
  :translate-alist '((footnote-reference . org-tufte-footnote-reference)
                     (src-block . org-tufte-src-block)
                     (link . org-tufte-maybe-margin-note-link)
                     (quote-block . org-tufte-quote-block)
                     (verse-block . org-tufte-verse-block)))


;;; Transcode Functions

(defun org-tufte-quote-block (quote-block contents info)
  "Transform a quote block into an epigraph in Tufte HTML style"
  (format "<div class=\"epigraph\"><blockquote>\n%s\n%s</blockquote></div>"
          contents
          (if (org-element-property :name quote-block)
              (format "<footer>%s</footer>"
                      (org-element-property :name quote-block))
            "")))

(defun org-tufte-verse-block (verse-block contents info)
  "Transcode a VERSE-BLOCK element from Org to HTML.
CONTENTS is verse block contents.  INFO is a plist holding
contextual information."
  ;; Replace each newline character with line break.  Also replace
  ;; each blank line with a line break.
  (setq contents (replace-regexp-in-string
      "^ *\\\\\\\\$" (format "%s\n" (org-html-close-tag "br" nil info))
      (replace-regexp-in-string
       "\\(\\\\\\\\\\)?[ \t]*\n"
       (format "%s\n" (org-html-close-tag "br" nil info)) contents)))
  ;; Replace each white space at beginning of a line with a
  ;; non-breaking space.
  (while (string-match "^[ \t]+" contents)
    (let* ((num-ws (length (match-string 0 contents)))
     (ws (let (out) (dotimes (i num-ws out)
          (setq out (concat out "&#xa0;"))))))
      (setq contents (replace-match ws nil t contents))))
  (format "<div class=\"epigraph\"><blockquote>\n%s\n%s</blockquote></div>"
          contents
          (if (org-element-property :name verse-block)
              (format "<footer>%s</footer>"
                      (org-element-property :name verse-block))
            "")))

(defun org-tufte-footnote-reference (footnote-reference contents info)
  "Create a footnote according to the tufte css format.
FOOTNOTE-REFERENCE is the org element, CONTENTS is nil. INFO is a
plist holding contextual information."
  (format
   (concat "<label for=\"%s\" class=\"margin-toggle sidenote-number\"></label>"
           "<input type=\"checkbox\" id=\"%s\" class=\"margin-toggle\"/>"
           "<span class=\"sidenote\">%s</span>")
          (org-export-get-footnote-number footnote-reference info)
          (org-export-get-footnote-number footnote-reference info)
          (org-trim
           (org-export-data
            (org-export-get-footnote-definition footnote-reference info)
            info))))

(defun org-tufte-maybe-margin-note-link (link desc info)
  "Render LINK as a margin note if it starts with `mn:', for
  example, `[[mn:1][this is some text]]' is margin note 1 that
  will show \"this is some text\" in the margin.

If it does not, it will be passed onto the original function in
order to be handled properly. DESC is the description part of the
link. INFO is a plist holding contextual information."
  (let ((path (split-string (org-element-property :path link) ":")))
    (if (and (string= (org-element-property :type link) "fuzzy")
             (string= (car path) "mn"))
        (format
         (concat "<label for=\"%s\" class=\"margin-toggle\">&#8853;</label>"
                 "<input type=\"checkbox\" id=\"%s\" class=\"margin-toggle\"/>"
                 "<span class=\"marginnote\">%s</span>")
                (cadr path) (cadr path)
                desc)
      (org-html-link link desc info))))

(defun org-tufte-src-block (src-block contents info)
  "Transcode SRC-BLOCK element into Tufte HTML format. CONTENTS
is nil. INFO is a plist used as a communication channel."
  (format "<pre class=\"code\">%s</pre>"
          (org-html-format-code src-block info)))


;;; Export functions

;;;###autoload
(defun org-tufte-export-to-buffer (&optional async subtreep visible-only)
  "Export current buffer to a Tufte HTML buffer.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting buffer should be accessible
through the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

Export is done in a buffer named \"*Org Tufte Export*\", which will
be displayed when `org-export-show-temporary-export-buffer' is
non-nil."
  (interactive)
  (let (;; need to bind this because tufte treats footnotes specially, so we
        ;; don't want to display them at the bottom
        (org-html-footnotes-section "<!-- %s --><!-- %s -->"))
    (org-export-to-buffer 'tufte-html "*Org Tufte Export*"
      async subtreep visible-only nil nil (lambda () (text-mode)))))

;;;###autoload
(defun org-tufte-export-to-file (&optional async subtreep visible-only)
  "Export current buffer to a Tufte HTML file.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting file should be accessible through
the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

Return output file's name."
  (interactive)
  (let ((outfile (org-export-output-file-name ".html" subtreep))
        ;; need to bind this because tufte treats footnotes specially, so we
        ;; don't want to display them at the bottom
        (org-html-footnotes-section "<!-- %s --><!-- %s -->"))
    (org-export-to-file 'tufte-html outfile async subtreep visible-only)))

(provide 'ox-tufte)

;;; ox-gfm.el ends here

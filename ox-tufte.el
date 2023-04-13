;;; ox-tufte.el --- Tufte HTML org-mode export backend

;; Copyright (C) 2016 Matthew Lee Hinman

;; Author: M. Lee Hinman
;; Description: An org exporter for Tufte HTML
;; Keywords: org, tufte, html
;; Version: 1.0.0
;; Package-Requires: ((org "8.2") (emacs "24"))
;; URL: https://github.com/dakrone/ox-tufte

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

;; This is an export backend for Org-mode that exports buffers to HTML that
;; is compatible with Tufte CSS - https://edwardtufte.github.io/tufte-css/ out of
;; the box (meaning no CSS modifications needed).

;;; Code:

(require 'ox)
(require 'ox-html)
(eval-when-compile (require 'cl-lib)) ;; for cl-assert
(require 'shr)


;;; User-Configurable Variables

(defgroup org-export-tufte nil
  "Options specific to Tufte export back-end."
  :tag "Org Tufte"
  :group 'org-export
  :version "24.4"
  :package-version '(Org . "8.0"))

(defcustom org-tufte-include-footnotes-at-bottom nil
  "Non-nil means to include footnotes at the bottom of the page
  in addition to being included as sidenotes. Sidenotes are not
  shown on very narrow screens (phones), so it may be useful to
  additionally include them at the bottom."
  :group 'org-export-tufte
  :type 'boolean)


;;; Define Back-End

(org-export-define-derived-backend 'tufte-html 'html
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
                     ;; (src-block . org-tufte-src-block)
                     (link . org-tufte-maybe-margin-note-link)
                     (quote-block . org-tufte-quote-block)
                     (verse-block . org-tufte-verse-block)))


;;; Transcode Functions

(defun ox-tufte/utils/string-fragment-to-xml (str)
  "Parse string fragment via `libxml'.
STR is the xml fragment.

For the inverse, use `shr-dom-to-xml'."
  (cl-assert (libxml-available-p))
  (with-temp-buffer
    (insert str)
    ;; we really want to use `libxml-parse-xml-region', but that's too
    ;; strict. `libxml-parse-html-region' is more lax (and that's good for us),
    ;; but it creates <html> and <body> tags when missing. since we'll only be
    ;; using this function on html fragments, we can assume these elements are
    ;; always added and thus are safe to strip away
    (caddr  ;; strip <html> tag
     (caddr ;; strip <body> tag
      (libxml-parse-html-region (point-min) (point-max))))))

(defun org-tufte-quote-block (quote-block contents info)
  "Transform a quote block into an epigraph in Tufte HTML style"
  (let* ((ox-tufte/ox-html-qb-str (org-html-quote-block quote-block contents info))
         (ox-tufte/ox-html-qb-dom
          (ox-tufte/utils/string-fragment-to-xml ox-tufte/ox-html-qb-str))
         (ox-tufte/qb-name (org-element-property :name quote-block))
         (ox-tufte/footer-content-maybe
          (if ox-tufte/qb-name
              (format "<footer>%s</footer>" ox-tufte/qb-name)
            nil)))
    (when ox-tufte/footer-content-maybe
      (push (ox-tufte/utils/string-fragment-to-xml ox-tufte/footer-content-maybe)
            (cdr (last ox-tufte/ox-html-qb-dom))))
    (format "<div class=\"epigraph\">%s</div>"
            (if ox-tufte/footer-content-maybe ;; then we would've modified qb-dom
                (shr-dom-to-xml ox-tufte/ox-html-qb-dom)
              ox-tufte/ox-html-qb-str))
    ))

(defun org-tufte-verse-block (verse-block contents info)
  "Transcode a VERSE-BLOCK element from Org to HTML.
CONTENTS is verse block contents.  INFO is a plist holding
contextual information."
  (let* ((ox-tufte/ox-html-vb-str (org-html-verse-block verse-block contents info))
         (ox-tufte/vb-name (org-element-property :name verse-block))
         (ox-tufte/footer-content
          (if ox-tufte/vb-name
              (format "<footer>%s</footer>" ox-tufte/vb-name)
            "")))
    (format "<div class=\"verse\"><blockquote>\n%s\n%s</blockquote></div>"
          ox-tufte/ox-html-vb-str
          ox-tufte/footer-content)
    ))

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
   (let ((fn-data (org-trim
                   (org-export-data
                    (org-export-get-footnote-definition footnote-reference info)
                    info))))
     ;; footnotes must have spurious <p> tags removed or they will not work
     (replace-regexp-in-string "</?p.*>" "" fn-data))))

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
         (replace-regexp-in-string "</?p.*>" "" desc))
      (org-html-link link desc info))))

(defun org-tufte-src-block (src-block contents info)
  "Transcode SRC-BLOCK element into Tufte HTML format. CONTENTS
is nil. INFO is a plist used as a communication channel."
  (format "<pre class=\"code\"><code>%s</code></pre>"
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
        (org-html-footnotes-section (if org-tufte-include-footnotes-at-bottom
                                        org-html-footnotes-section
                                      "<!-- %s --><!-- %s -->")))
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
        (org-html-footnotes-section (if org-tufte-include-footnotes-at-bottom
                                        org-html-footnotes-section
                                      "<!-- %s --><!-- %s -->")))
    (org-export-to-file 'tufte-html outfile async subtreep visible-only)))


;;; publishing function

;;;###autoload
(defun org-html-publish-to-tufte-html (plist filename pub-dir)
  "Publish an org file to Tufte-styled HTML.

PLIST is the property list for the given project.  FILENAME is
the filename of the Org file to be published.  PUB-DIR is the
publishing directory.

Return output file name."
  (org-publish-org-to 'tufte-html filename
                      (concat "." (or (plist-get plist :html-extension)
                                      org-html-extension
                                      "html"))
                      plist pub-dir))

(provide 'ox-tufte)

;;; ox-tufte.el ends here

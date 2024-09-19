;;; ox-tufte.el --- Tufte HTML org-mode export backend -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2024 The Bayesians Inc.
;; Copyright (C) 2016-2022 Matthew Lee Hinman

;; Author: The Bayesians Inc.
;;         M. Lee Hinman
;; Maintainer: The Bayesians Inc.
;; Description: An org exporter for Tufte HTML
;; Keywords: org, tufte, html, outlines, hypermedia, calendar, wp
;; Package-Version: 4.2.1.50-git
;; Package-Requires: ((emacs "27.1") (org "9.5"))
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

;;; Commentary:

;; This is an export backend for Org-mode that exports buffers to HTML that
;; is compatible with Tufte CSS - <https://edwardtufte.github.io/tufte-css/>.
;; The design goal is to "minimally" change the HTML structure as generated by
;; `ox-html' (with additional CSS as needed) to get behaviour that is equivalent
;; to Tufte CSS.

;;; Code:

(require 'ox)
(require 'ox-html)

;;;; initialization:
;;;;; marginnote syntax support
(org-babel-lob-ingest ;; for marginnote-as-babel-call syntax
 (concat (file-name-directory (locate-library "ox-tufte")) "src/README.org"))
;;;;; reproducible identifiers
(require 'org)
(require 'ox-html)
;; HACK: doing below once seems to be needed if `org-export-as' hasn't been
;; invoked previously
(org-export-string-as "" 'html t nil)



;;; Define Back-End
(org-export-define-derived-backend 'tufte-html 'html
  :menu-entry
  '(?T "Export to Tufte-HTML"
       ((?H "As HTML buffer" org-tufte-export-as-html)
        (?h "As HTML file" org-tufte-export-to-html)
        (?o "As HTML file and open"
            (lambda (a s v b)
              (if a (org-tufte-export-to-html t s v b)
                (org-open-file (org-tufte-export-to-html nil s v b)))))))
  :options-alist
  '((:footnotes-section-p nil "footnotes-section-p"
                          org-tufte-include-footnotes-at-bottom)
    ;; Recommended overrides for `ox-html'
    (:html-checkbox-type nil nil org-tufte-html-checkbox-type)
    ;; Essential overrides: recommended not to alter.  Thus their KEYWORDS and
    ;; OPTIONS are set to nil and disabled.
    (:html-divs nil nil org-tufte-html-sections)
    (:html-container nil nil "section")
    (:html-doctype nil nil "html5")
    (:html-html5-fancy nil nil t))
  :translate-alist '((footnote-reference . org-tufte-footnote-reference)
                     (link . org-tufte-maybe-margin-note-link)
                     (quote-block . org-tufte-quote-block)
                     (special-block . org-tufte-special-block)
                     (verse-block . org-tufte-verse-block)))


;;; User-Configurable Variables
(defgroup org-export-tufte nil
  "Options for exporting Org mode files to Tufte-CSS themed HTML."
  :tag "Org Export Tufte HTML"
  :group 'org-export)

(defcustom org-tufte-feature-more-expressive-inline-marginnotes t
  "Non-nil enables marginnote-as-macro and marginnote-as-babelcall syntax."
  :group 'org-export-tufte
  :type 'boolean
  :safe #'booleanp)

(defcustom org-tufte-include-footnotes-at-bottom nil
  "Non-nil means to include footnotes at the bottom of the page.
This is in addition to being included as sidenotes.  Sidenotes are not shown on
very narrow screens (phones), so it may be useful to additionally include them
at the bottom."
  :group 'org-export-tufte
  :type 'boolean
  :safe #'booleanp)

(defcustom org-tufte-margin-note-symbol "&#8853;"
  "The symbol that is used as a viewability-toggle on small screens.
Neither marginnote-as-macro nor marginnote-as-babel-call have
access to the communication channel (not unless they invoke
something like `org-export-get-environment' which could get
expensive).  As such we don't include this in the
`:options-alist' to limit confusion.

Those wanting to set this option within the Org mode file can
enable `org-export-allow-bind-keywords' and then use something
like `#+BIND: org-tufte-margin-note-symbol \"replacement\"' to
define \"replacement\" as the local value for
`org-tufte-margin-note-symbol'."
  :group 'org-export-tufte
  :type 'string
  :safe #'stringp)

;;;; `ox-html' overrides
(defcustom org-tufte-html-checkbox-type 'html
  "The type of checkboxes to use for Tufte HTML export.
See `org-html-checkbox-types' for the values used for each
option."
  :group 'org-export-tufte
  :package-version '(ox-tufte . "4.0.0")
  :type '(choice
	      (const :tag "ASCII characters" ascii)
	      (const :tag "Unicode characters" unicode)
	      (const :tag "HTML checkboxes" html)))

(defcustom org-tufte-html-sections
  '((preamble "header" "preamble") ;; `header' i/o  `div'
    (content "article" "content") ;; `article' for `tufte.css'
    (postamble "footer" "postamble")) ;; footer i/o `div'
  "Alist of the three section elements for Tufte HTML export.
The car of each entry is one of `preamble', `content' or `postamble'.
The cdrs of each entry are the ELEMENT_TYPE and ID for each
section of the exported document.

Note that changing the default may break the associated CSS.  The
ELEMENT_TYPE of the `content' entry must be \"article\"."
  :group 'org-export-tufte
  :package-version '(ox-tufte . "4.0.0")
  :type '(list :greedy t
	           (list :tag "Preamble"
		             (const :format "" preamble)
		             (string :tag "element") (string :tag "     id"))
	           (list :tag "Content"
		             (const :format "" content)
		             (string :tag "element") (string :tag "     id"))
	           (list :tag "Postamble" (const :format "" postamble)
		             (string :tag "     id") (string :tag "element"))))
;;;###autoload
(put 'org-tufte-html-sections 'safe-local-variable
     (lambda (x)
       (string= (car (alist-get 'content x))
                "article")))

;;;; advanced
(defcustom org-tufte-randid-limit 10000000
  "Upper limit when generating random IDs.
This has to be a positive integer.  With the default value of
10000000, there is ~0.2% chance of collision with 200 references."
  :group 'org-export-tufte
  :type 'integer
  :safe (lambda (x)
          (and (integerp x)
               (> x 0)))
  :set (lambda (sym val)
         (if (funcall
              (plist-get (symbol-plist 'org-tufte-randid-limit)
                         'safe-local-variable)
              val)
             (set-default-toplevel-value sym val)
           (error "`org-tufte-randid-limit' must be a positive integer"))))
(defcustom org-tufte-export-as-advice-depth 100
  "Depth at which to install `org-export-as' advice.
The default of 100 ensures that it is the innermost advice.
Please use `setopt' in order to modify this value."
  :group 'org-export-tufte
  :type 'integer
  :safe (lambda (x)
          (and (integerp x)
               (>= x -100)
               (<= x 100)))
  :set (lambda (sym val)
         (let ((safeval (or
                         (and (funcall
                               (plist-get
                                (symbol-plist 'org-tufte-export-as-advice-depth)
                                'safe-local-variable)
                               val)
                              val)
                         100)))
           (advice-remove #'org-export-as #'org-tufte-export-as-advice)
           (advice-add #'org-export-as :around
                       #'org-tufte-export-as-advice `((depth . ,safeval)))
           (set-default-toplevel-value sym safeval))))


;;; Utility Functions
;;;; marginalia
(defun ox-tufte--utils-filter-tags (str)
  "Remove <p>, <div> and <figure> tags from STR.
Sidenotes and margin notes must have these tags removed to conform with
the html structure that tufte.css expects."
  (replace-regexp-in-string "</?p.*?>\\|</?div.*?>\\|</?figure.*?>" "" str))

(defun ox-tufte--utils-margin-note-macro (&rest args)
  "Return HTML snippet treating each arg in ARGS as a separate line."
  (let ((note (string-join args "\\\n")))
    (concat
     "@@html:"
     (ox-tufte--utils-margin-note note)
     "@@")))

(defun ox-tufte--utils-margin-note (desc)
  "Return HTML snippet after interpreting DESC as a margin note.
This intended to be called via the `marginnote' library-of-babel function."
  (if org-tufte-feature-more-expressive-inline-marginnotes
      (let* ((ox-tufte--mn-macro-templates org-macro-templates)
             ;; ^ copy buffer-local variable
             (exported-str
              (let* ((org-export-global-macros ;; make buffer macros accessible
                      (append ox-tufte--mn-macro-templates org-export-global-macros))
                     ;; footnotes nested within marginalia aren't supported
                     (org-html-footnotes-section "<!-- %s --><!-- %s -->"))
                (org-export-string-as desc 'html t
                                      '(:html-checkbox-type
                                        org-tufte-html-checkbox-type))))
             (exported-newline-fix (replace-regexp-in-string
                                    "\n" " "
                                    (replace-regexp-in-string
                                     "\\\\\n" "<br>"
                                     exported-str)))
             (exported-para-fix (ox-tufte--utils-filter-tags exported-newline-fix)))
        (ox-tufte--utils-margin-note-snippet exported-para-fix))
    ;; if expressive-inline-marginnotes isn't enabled, silently fail
    ""))

(defun ox-tufte--utils-margin-note-snippet (text &optional idtag blob)
  "Generate html snippet for margin-note with TEXT.
TEXT shouldn't have any <p> tags (or behaviour is undefined).  If
<p> tags are needed, use BLOB which must be an HTML snippet of a
containing element with `marginnote' class.  BLOB is ignored
unless TEXT is nil.

IDTAG is used in the construction of the `id' that connects a
margin-notes visibility-toggle with the margin-note."
  (let ((mnid (format "mn-%s.%s" (or idtag "auto") (ox-tufte--utils-randid)))
        (content (if text
                     (format "<span class='marginnote'>%s</span>" text)
                   blob)))
    (format
     (concat
      "<label for='%s' class='margin-toggle'>"
      org-tufte-margin-note-symbol
      "</label>"
      "<input type='checkbox' id='%s' class='margin-toggle'>"
      "%s")
     mnid mnid
     content)))

(defun ox-tufte--utils-randid ()
  "Give a random number below the `org-tufte-randid-limit'."
  (random org-tufte-randid-limit))


;;;; ox-html
(defvar ox-tufte--sema-in-tufte-export nil
  "Currently in the midst of an export.")
(defvar ox-tufte--store-confirm-babel-evaluate nil
  "Store value of `org-confirm-babel-evaluate'.")

(defun ox-tufte--allow-mn-babel-call-maybe (lang body)
  "Permit evaluation of marginnote babel-call.
LANG is the language of the code block whose text is BODY,"
  (if (and org-tufte-feature-more-expressive-inline-marginnotes
           (string= lang "elisp")
           (string= body "(require 'ox-tufte)
(ox-tufte--utils-margin-note input)"))
      nil ;; i.e., don't seek confirmation from user
    (if (functionp ox-tufte--store-confirm-babel-evaluate)
        (funcall ox-tufte--store-confirm-babel-evaluate lang body)
      ox-tufte--store-confirm-babel-evaluate)))

(defun org-tufte-export-as-advice (fun backend &optional s v b p)
  "Evaluate FUN `org-export-as' in appropriate environment.
Arguments (S V B P) are the same as the corresponding positional
arguments needed by org-export-as.  When BACKEND is derived from
`tufte-html' this advice ensures the export is carried out in an
environment where `ox-tufte--sema-in-tufte-export' is t.
Depending on the value of
`org-tufte-feature-more-expressive-inline-marginnotes' this
advice may additionally temporarily override the value of
`org-confirm-babel-evaluate' in order to allow the `marginnote'
babel block."
  (random "ox-tufte") ;; initialize the random seed
  (let* ((ox-tufte-p (org-export-derived-backend-p backend 'tufte-html))
         (ox-tufte-first-call-p (and ox-tufte-p
                                     (not ox-tufte--sema-in-tufte-export)))
         (ox-tufte--sema-in-tufte-export (or ox-tufte-p
                                             ox-tufte--sema-in-tufte-export))
         (p+ (if ox-tufte--sema-in-tufte-export
                 (append p ;; later values triumph for this plist
                         '(;; we don't override `:html-divs' and
                           ;; `:html-checkbox-type' since it's possible for them
                           ;; to still be valid when altered
                           :html-container "section"
                           :html-doctype "html5"
                           :html-html5-fancy t))
               p))
         (ox-tufte--sema-in-tufte-export (or ox-tufte-p
                                             ox-tufte--sema-in-tufte-export)))
    (if (not (and  ox-tufte-first-call-p
                   org-tufte-feature-more-expressive-inline-marginnotes))
        (funcall fun backend s v b p+)
      ;; o.w. in first call to tufte-html w/ more-expressive-syntax enabled, so
      ;; setup environment before evaluating
      (let ((org-export-global-macros ;; could be done in `org-export-before-processing-functions'
             (cons '("marginnote" . ox-tufte--utils-margin-note-macro)
                   org-export-global-macros))
            (ox-tufte--store-confirm-babel-evaluate org-confirm-babel-evaluate)
            (org-confirm-babel-evaluate #'ox-tufte--allow-mn-babel-call-maybe)
            (ox-tufte/tmp/lob-pre org-babel-library-of-babel))
        ;; allow evaluation of blocks within mn-as-macro or mn-as-babel-call
        (let ((inhibit-message t)) ;; silence only the lob ingestion messages
          (org-babel-lob-ingest buffer-file-name))
        (let ((output (funcall fun backend s v b p+)))
          (setq org-babel-library-of-babel ox-tufte/tmp/lob-pre)
          output)))))
;; NOTE: ^ no need to `advice-add' `org-tufte-export-as-advice', since it gets
;; added by the `org-tufte-export-as-advice-depth' defcustom on load.

(defun ox-tufte--utils-get-export-output-extension (plist)
  "Get export filename extension based on PLIST."
  (concat
   (when (> (length org-html-extension) 0) ".")
   (or (plist-get plist :html-extension)
       org-html-extension
       "html")))


;;; Transcode Functions
;;;; quote-block
(defun org-tufte-quote-block (quote-block contents info)
  "Transform a quote block into an epigraph in Tufte HTML style.
QUOTE-BLOCK CONTENTS INFO are as they are in `org-html-quote-block'."
  (let* ((ox-tufte/ox-html-qb-str (org-html-quote-block quote-block contents info))
         (ox-tufte/qb-caption (org-export-data
                               (org-export-get-caption quote-block) info))
         (ox-tufte/footer-content-maybe
          (if (org-string-nw-p ox-tufte/qb-caption)
              (format "<footer>%s</footer>" ox-tufte/qb-caption)
            nil)))
    (if ox-tufte/footer-content-maybe
        (replace-regexp-in-string
         "</blockquote>\\'"
         (concat ox-tufte/footer-content-maybe "</blockquote>")
         ox-tufte/ox-html-qb-str t t)
      ox-tufte/ox-html-qb-str)))

;;;; verse-block
(defun org-tufte-verse-block (verse-block contents info)
  "Transcode a VERSE-BLOCK element from Org to HTML.
CONTENTS is verse block contents.  INFO is a plist holding
contextual information."
  (let* ((ox-tufte/ox-html-vb-str (org-html-verse-block verse-block contents info))
         (ox-tufte/vb-caption (org-export-data
                               (org-export-get-caption verse-block) info))
         (ox-tufte/footer-content
          (if (org-string-nw-p ox-tufte/vb-caption)
              (format "<footer>%s</footer>" ox-tufte/vb-caption)
            "")))
    (format "<div class='verse'><blockquote>\n%s\n%s</blockquote></div>"
            ox-tufte/ox-html-vb-str
            ox-tufte/footer-content)))

;;;; footnotes as sidenotes
(defun org-tufte-footnote-section-advice (fun info)
  "Modify `org-html-footnote-section' based on `:footnotes-section-p'.
FUN is `org-html-footnote-section' and INFO is the
plist (\"communication channel\")."
  (if (and ox-tufte--sema-in-tufte-export
           (not (plist-get info :footnotes-section-p)))
      ""
    (funcall fun info)))
(advice-add 'org-html-footnote-section
            :around #'org-tufte-footnote-section-advice)
;; ox-html: definition: id="fn.<id>"; href="#fnr.<id>"
(defun org-tufte-footnote-reference (footnote-reference _contents info)
  "Create a footnote according to the tufte css format.
FOOTNOTE-REFERENCE is the org element, CONTENTS is nil.  INFO is a
plist holding contextual information.

Modified from `org-html-footnote-reference' in `org-html'."
  (concat
   ;; Insert separator between two footnotes in a row.
   (let ((prev (org-export-get-previous-element footnote-reference info)))
     (when (eq (org-element-type prev) 'footnote-reference)
       (plist-get info :html-footnote-separator)))
   (let* ((ox-tufte/fn-num
           (org-export-get-footnote-number footnote-reference info))
          (ox-tufte/uid (ox-tufte--utils-randid))
          (ox-tufte/fn-inputid (format "fnr-in.%d.%s" ox-tufte/fn-num ox-tufte/uid))
          (ox-tufte/fn-labelid ;; first reference acts as back-reference
           (if (org-export-footnote-first-reference-p footnote-reference info)
               (format "fnr.%d" ox-tufte/fn-num) ;; this conforms to `ox-html.el'
             (format "fnr.%d.%s" ox-tufte/fn-num ox-tufte/uid)))
          (ox-tufte/fn-def
           (org-export-get-footnote-definition footnote-reference info))
          (ox-tufte/fn-data
           (org-trim (org-export-data ox-tufte/fn-def info)))
          (ox-tufte/fn-data-unpar
           ;; footnotes must have spurious <p> tags removed or they will not work
           (ox-tufte--utils-filter-tags ox-tufte/fn-data)))
     (format
      (concat
       "<label id='%s' for='%s' class='margin-toggle sidenote-number'><sup class='numeral'>%s</sup></label>"
       "<input type='checkbox' id='%s' class='margin-toggle'>"
       "<span class='sidenote'><sup class='numeral'>%s</sup>%s</span>")
      ox-tufte/fn-labelid ox-tufte/fn-inputid ox-tufte/fn-num
      ox-tufte/fn-inputid
      ox-tufte/fn-num ox-tufte/fn-data-unpar))))

;;;; special-block
(defun org-tufte-special-block (special-block contents info)
  "Add support for block margin-note special blocks.
Pass SPECIAL-BLOCK CONTENTS and INFO to `org-html-special-block' otherwise."
  (let ((block-type (org-element-property :type special-block)))
    (cond
     ((string= block-type "marginnote")
      (ox-tufte--utils-margin-note-snippet
       nil nil (org-html-special-block special-block contents info)))
     ;; add support for captions on figures that `ox-html' lacks
     ((and (string= block-type "figure")
           (org-html--has-caption-p special-block info)
           ;; FIXME: tufte-css v1.8.0 doesn't support captions on iframe-wrapper
           (not (member "iframe-wrapper"
                        (split-string
                         (plist-get (org-export-read-attribute :attr_html
                                                               special-block)
                                    :class)
                         " "))))
      (let* ((caption (let ((raw (org-export-data
                                  (org-export-get-caption special-block) info)))
                        (if (not (org-string-nw-p raw)) raw
                          ;; FIXME: it would be nice to be able to count figure
                          ;; as an image and number accordingly
                          raw
                          ;; (concat "<span class=\"figure-number\">"
                          ;;         (format (org-html--translate "Figure %d:" info)
                          ;;                 (org-export-get-ordinal
                          ;;                  (org-element-map special-block 'link
                          ;;                    #'identity info t)
                          ;;                  info '(link) #'org-html-standalone-image-p))
                          ;;         " </span>"
                          ;;         raw)
                          )))
             (figcaption (format "<figcaption>%s</figcaption>" caption))
             ;; FIXME: might be more robust to parse-replace-serialize the HTML
             ;; instead.
             (o-h-sb-str (org-html-special-block special-block contents info)))
        (replace-regexp-in-string "</figure>\\'" (concat figcaption "</figure>")
                                  o-h-sb-str t t)))
     (t (org-html-special-block special-block contents info)))))

;;;; margin-note as link
(defun org-tufte-maybe-margin-note-link (link desc info)
  "Render LINK as a margin note if it begins with `mn:'.
For example, `[[mn:1][this is some text]]' is margin note 1 that
will show \"this is some text\" in the margin.

If it does not, it will be passed onto the original function in
order to be handled properly. DESC is the description part of the
link. INFO is a plist holding contextual information.

Defining margin-note link in this manner, as opposed to via
`org-link-set-parameters', ensures that margin-notes are only
handled when occurring as regular links and not as angle or plain
links. Additionally, it ensures that we only handle margin-notes
for HTML backend without having an opinion on how to treat them
for other backends."
  (if-let ((path (org-element-property :path link))
           (pathelems (split-string path ":"))
           (type (downcase (org-element-property :type link)))
           ((and (string= type "fuzzy")
                 (string= (car pathelems) "mn")))
           (tag (cadr pathelems)))
      (ox-tufte--utils-margin-note-snippet
       (ox-tufte--utils-filter-tags (or desc ""))
       (if (string= tag "") nil tag))
    (if-let ((fn (plist-get (alist-get type org-link-parameters
                                       nil nil #'string=)
                            :export)))
        (funcall fn path desc 'tufte-html info)
      (org-html-link link desc info))))


;;; Export commands

;;;###autoload
(defun org-tufte-export-as-html
    (&optional async subtreep visible-only body-only ext-plist)
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

When optional argument BODY-ONLY is non-nil, only write code
between \"<body>\" and \"</body>\" tags.

EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.

Export is done in a buffer named \"*Org Tufte Export*\", which will
be displayed when `org-export-show-temporary-export-buffer' is
non-nil."
  (interactive)
  (org-export-to-buffer 'tufte-html "*Org Tufte Export*"
    async subtreep visible-only body-only ext-plist
    (lambda () (set-auto-mode t))))

;;;###autoload
(defun org-tufte-convert-region-to-html ()
  "Assume the current region has Org syntax, and convert it to Tufte HTML.
This can be used in any buffer.  For example, you can write an
itemized list in Org syntax in an HTML buffer and use this command
to convert it."
  (interactive)
  (org-export-replace-region-by 'tufte-html))

;;;###autoload
(defun org-tufte-export-to-html
    (&optional async subtreep visible-only body-only ext-plist)
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

When optional argument BODY-ONLY is non-nil, only write code
between \"<body>\" and \"</body>\" tags.

EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.

Return output file's name."
  (interactive)
  (let ((file (org-export-output-file-name
               (ox-tufte--utils-get-export-output-extension ext-plist)
               subtreep)))
    (org-export-to-file 'tufte-html file
      async subtreep visible-only body-only ext-plist)))


;;; Publishing function

;;;###autoload
(defun org-tufte-publish-to-html (plist filename pub-dir)
  "Publish an org file to Tufte-styled HTML.

PLIST is the property list for the given project.  FILENAME is
the filename of the Org file to be published.  PUB-DIR is the
publishing directory.

Return output file name."
  (org-publish-org-to
   'tufte-html filename
   (ox-tufte--utils-get-export-output-extension plist)
   plist pub-dir))

(provide 'ox-tufte)

;;; ox-tufte.el ends here

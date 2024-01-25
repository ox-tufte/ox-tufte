;;; ox-tufte-test.el --- Tests for ox-tufte.el -*- lexical-binding: t; -*-

;; Copyright (C) 2023, 2024      The Bayesians Inc.

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
  (require 'ert)
  (require 'buttercup))

(require 'ox-tufte)


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
(defmacro org-html-test-in-exported-buffer (text body-only &rest body)
  "Execute BODY in buffer containing exported result of TEXT.
BODY-ONLY controls whether entire html page is exported or only
the body."
  `(org-test-with-temp-text
    ,text
    (let ((export-buffer "*Org HTML Export*")
          (org-export-show-temporary-export-buffer nil))
      (org-html-export-as-html
       nil nil nil ,body-only nil)
      (with-current-buffer export-buffer
        ,@body))))

(defmacro org-tufte-test-debug (text &optional body-only html)
  "Debug the result of exporting TEXT.
BODY-ONLY controls whether entire html page is exported or only
the body.  If HTML is t then the output of `ox-html' is shown."
  `(should
    (,(if html 'org-html-test-in-exported-buffer
        'org-tufte-test-in-exported-buffer)
     ,text ,body-only
     (progn
       (message "'%s'" (buffer-string))
       t))))


;;; General tests
;;;; mathjax
(ert-deftest ox-tufte/mathjax-path-none ()
  "If no LaTeX in page, then MathJax isn't loaded."
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


;;;; html5
(ert-deftest ox-tufte/html/uses-html5-tags ()
  "Ox-tufte uses HTML5 tags."
  (should
   (org-tufte-test-in-exported-buffer
    "* Heading" nil
    (let ((case-fold-search t))
      (search-forward "<!DOCTYPE html>" nil t))))
  (should
   (org-tufte-test-in-exported-buffer
    "* Heading" nil
    (let ((case-fold-search t))
      (search-forward "<article id=\"content\" class=\"content\">" nil t))))
  (should
   (org-tufte-test-in-exported-buffer
    "* Heading" nil
    (let ((case-fold-search t))
      (search-forward "Heading</h2>
</section>" nil t))))
  (should
   (org-tufte-test-in-exported-buffer
    "<file:./image.png>" t
    (let ((case-fold-search t))
      (search-forward "<figure " nil t)))))

(describe "HTML5: uses HTML checkboxes"
  (it "uses HTML checkboxes by default"
    (expect (org-export-string-as "- [ ] first \n- [ ] second"
                                  'tufte-html t)
            :to-match (rx "<input type='checkbox'"))))

(ert-deftest ox-tufte/html/forces-use-of-html5-tags ()
  "Ox-tufte only has defined behaviour for HTML5 so forces it."
  (should ;; sanity-check: default ox-html doesn't output figures
   (let ((res (org-export-string-as "<file:./image.png>" 'html t nil)))
     (not (string-search "<figure " res))))
  (should ;; sanity-check: ox-html with doctype alone doesn't output figures
   (let ((res (org-export-string-as "<file:./image.png>" 'html t
                                    '(:html-doctype "xhtml-strict"))))
     (not (string-search "<figure " res))))
  (should ;; sanity-check: ox-html can output figures
   (let ((res (org-export-string-as "<file:./image.png>" 'html t
                                    (list :html-doctype "html5"
                                          :html-html5-fancy t))))
     (string-search "<figure " res)))
  (should
   (let ((res (org-export-string-as "<file:./image.png>" 'tufte-html t nil)))
     (string-search "<figure " res)))
  (should
   (let ((res (org-export-string-as "<file:./image.png>" 'tufte-html t
                                    (list
                                     :html-doctype "xhtml-strict"
                                     :html-html5-fancy nil))))
     (string-search "<figure " res))))


;;;; `ox-html' substitutes
(describe "Substitute: `org-html-checkbox-type' => `org-tufte-html-checkbox-type'"
  (it "observes `org-tufte-html-checkbox-type'"
    (let ((org-tufte-html-checkbox-type 'ascii))
      (expect (org-export-string-as "- [ ] first \n- [ ] second"
                                    'tufte-html t)
              :to-match (rx "<code>["))))
  (it "ignores `org-html-checkbox-type'"
    (let ((org-html-checkbox-type 'ascii))
      (expect (org-export-string-as "- [ ] first \n- [ ] second"
                                    'tufte-html t)
              :not :to-match (rx "<code>[")))))

(describe "Substitute: `org-html-divs' => `org-tufte-html-sections'"
  :var ((html-sections '((preamble "header" "preamble")
                         (content "article" "canary")
                         (postamble "footer" "postamble"))))
  (it "respects the value of `org-tufte-html-sections'"
    (let ((org-tufte-html-sections html-sections))
      (expect (org-export-string-as "hello" 'tufte-html)
              :to-match (rx "<article id=\"canary\""))))
  (it "ignores the value of `org-html-divs'"
    (let ((org-html-divs html-sections))
      (expect (org-export-string-as "hello" 'tufte-html)
              :not :to-match (rx "<article id=\"canary\"")))))


;;; Footnotes
(ert-deftest ox-tufte/footnotes-section/design/consistent-with-ox-html ()
  "Ensure concordance of footnotes between `ox-tufte' and `ox-html'."
  (should ;; empty sidenotes okay, since empty footnotes are
   (and
    (org-tufte-test-in-exported-buffer
     "[fn::]" t
     (let ((case-fold-search t))
       (search-forward
        "class='sidenote'><sup class='numeral'>1</sup></span>" nil t)))
    (org-html-test-in-exported-buffer
     "[fn::]" t
     (let ((case-fold-search t))
       (search-forward
        "1</a></sup>" nil t))))))

(ert-deftest ox-tufte/footnotes-section-disabled-default ()
  "No Footnotes section by default."
  (should-not
   (org-tufte-test-in-exported-buffer
    "pre[fn::sidenote] post" t
    (let ((case-fold-search t))
      (search-forward "class=\"footnotes\">Footnotes: </h2>" nil t)))))

(ert-deftest ox-tufte/footnotes-section-configurable-per-file ()
  "Footnotes section can be enabled as needed."
  (should
   (org-tufte-test-in-exported-buffer
    "#+OPTIONS: footnotes-section-p:t
pre[fn::sidenote] post" t
    (let ((case-fold-search t))
      (search-forward "class=\"footnotes\">Footnotes: </h2>" nil t))))
  (let ((org-export-allow-bind-keywords t))
    (should
     (org-tufte-test-in-exported-buffer
      "#+BIND: org-tufte-include-footnotes-at-bottom t
pre[fn::sidenote] post" t
      (let ((case-fold-search t))
        (search-forward "class=\"footnotes\">Footnotes: </h2>" nil t)))))
  (should-not
   (org-tufte-test-in-exported-buffer
    "#+OPTIONS: footnotes-section-p:nil
pre[fn::sidenote] post" t
    (let ((case-fold-search t))
      (search-forward "class=\"footnotes\">Footnotes: </h2>" nil t)))))


;;; Inline marginnotes
(ert-deftest ox-tufte/marginnote-variations/consistency ()
  "Ensure the different margin-note syntax behave consistently."
  (should
   (org-tufte-test-in-exported-buffer
    "[[mn:]]" t
    (let ((case-fold-search t))
      (search-forward "class='marginnote'>" nil t))))
  (should
   (org-tufte-test-in-exported-buffer
    "{{{marginnote()}}}" t
    (let ((case-fold-search t))
      (search-forward "class='marginnote'>" nil t))))
  (should
   (org-tufte-test-in-exported-buffer
    "call_marginnote(\"\")" t
    (let ((case-fold-search t))
      (search-forward "class='marginnote'>" nil t)))))

(ert-deftest ox-tufte/marginnote-variations/org-export-string-as ()
  "Support marginnote-as-macro syntax within `org-export-string-as'."
  (should
   (let ((output-str (org-export-string-as
                      "{{{marginnote(note)}}}" 'tufte-html t)))
     (string-search "<span class='marginnote'> note </span>" output-str)))
  (should
   (let ((output-str (org-export-string-as
                      "call_marginnote(\"note\")" 'tufte-html t)))
     (string-search "<span class='marginnote'> note </span>" output-str)))
  (should
   (let ((output-str (org-export-string-as
                      "[[mn:][note]]" 'tufte-html t)))
     (string-search "<span class='marginnote'>note</span>" output-str))))

(ert-deftest ox-tufte/marginnote-symbol ()
  "Marginnote symbol can be tweaked."
  (should
   (let* ((org-tufte-margin-note-symbol "AAA")
          (marker-str (concat "class='margin-toggle'>"
                              org-tufte-margin-note-symbol
                              "</label>"))
          (macro-str (org-export-string-as
                      "{{{marginnote(note)}}}" 'tufte-html t))
          (link-str (org-export-string-as
                     "[[mn:][note]]" 'tufte-html t))
          (babel-str (org-export-string-as
                      "call_marginnote(\"note\")" 'tufte-html t)))
     (and (string-search marker-str macro-str)
          (string-search marker-str link-str)
          (string-search marker-str babel-str))))
  (should
   (let* ((org-export-allow-bind-keywords t)
          (keyword-str "#+BIND: org-tufte-margin-note-symbol \"BBB\"\n")
          (marker-str (concat "class='margin-toggle'>"
                              "BBB"
                              "</label>"))
          (macro-str (org-export-string-as
                      (concat keyword-str "{{{marginnote(note)}}}")
                      'tufte-html t))
          (link-str (org-export-string-as
                     (concat keyword-str "[[mn:][note]]")
                     'tufte-html t))
          (babel-str (org-export-string-as
                      (concat keyword-str "call_marginnote(\"note\")")
                      'tufte-html t)))
     (or (string-search marker-str macro-str)
         (string-search marker-str link-str)
         (string-search marker-str babel-str)))))


;;;; mn-as-link syntax
(ert-deftest ox-tufte/marginnote-as-link/design/only-as-regular-links ()
  "Angle and plain links for marginnotes shouldn't work."
  (should-not
   (org-tufte-test-in-exported-buffer
    "<mn:some text>" t
    (let ((case-fold-search t))
      (search-forward "class='marginnote'>" nil t))))
  (should-not
   (org-tufte-test-in-exported-buffer
    "mn:some text" t
    (let ((case-fold-search t))
      (search-forward "class='marginnote'>" nil t))))
  (should
   (org-tufte-test-in-exported-buffer
    "[[mn:][some text]]" t
    (let ((case-fold-search t))
      (search-forward "class='marginnote'>some text</span>" nil t)))))

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

(ert-deftest ox-tufte/marginnote-as-link/limitation/newlines-supported-only-via-raw-html ()
  "Newlines are supported in marginnote-as-link syntax."
  (should-not
   (org-tufte-test-in-exported-buffer
    "pre[[mn:][hello \\\\
world]] post" t
    (let ((case-fold-search t))
      (search-forward "<br>world" nil t))))
  (should-not
   (org-tufte-test-in-exported-buffer
    "pre[[mn:][hello
world]] post" t
    (let ((case-fold-search t))
      (search-forward "<br>world" nil t))))
  (should
   (org-tufte-test-in-exported-buffer
    "pre[[mn:][hello @@html:<br>@@ world]] post" t
    (let ((case-fold-search t))
      (search-forward "hello <br> world" nil t)))))

(ert-deftest ox-tufte/marginnote-as-link/macros-supported ()
  "Org macros are supported in marginnote-as-link syntax."
  (should
   (org-tufte-test-in-exported-buffer
    "#+MACRO: prefix $1 macro
pre[[mn:][pre {{{prefix(text)}}}]] post" t
    (let ((case-fold-search t))
      (search-forward "pre text macro" nil t)))))

(ert-deftest ox-tufte/marginnote-as-link/nested-babel-calls ()
  "Marginnote-as-link supports nested babel calls."
  (let ((org-confirm-babel-evaluate nil))
    (should
     (org-tufte-test-in-exported-buffer
      "pre [[mn:][in macro call_nested-call() eom]] post
* resource :noexport:
#+name: nested-call
#+begin_src elisp :results value
  \"nested call\"
#+end_src" t
      (let ((case-fold-search t))
        (search-forward "<code>nested call</code> eom</span>" nil t))))))


;;;; non-link syntax
(ert-deftest ox-tufte/marginnote-extended/resolved/standalone-img-dont-need-zero-width-space ()
  "Standalone images in inline marginnotes don't require escaping."
  (skip-unless org-tufte-feature-more-expressive-inline-marginnotes)
  (should-not
   (org-tufte-test-in-exported-buffer
    "pre{{{marginnote(<file:./image.png>​)}}} post" t
    (let ((case-fold-search t))
      (search-forward "<figure " nil t))))
  (should-not
   (org-tufte-test-in-exported-buffer
    "pre{{{marginnote(<file:./image.png>)}}} post" t
    (let ((case-fold-search t))
      (search-forward "<figure " nil t)))))

(ert-deftest ox-tufte/marginnote-extended/raw-html-supported ()
  "Raw HTML supported."
  (skip-unless org-tufte-feature-more-expressive-inline-marginnotes)
  (should
   (org-tufte-test-in-exported-buffer
    " pre {{{marginnote(hello @@html:<br>@@ world)}}} post" t
    (let ((case-fold-search t))
      (search-forward "hello <br> world </span>" nil t))))
  (should
   (org-tufte-test-in-exported-buffer
    " pre call_marginnote(\"hello @@html:<br>@@ world\") post" t
    (let ((case-fold-search t))
      (search-forward "hello <br> world </span>" nil t)))))


;;;; mn-as-macro syntax
(ert-deftest ox-tufte/marginnote-as-macro/limitation/nested-macros-unsupported ()
  "Nested macros are unsupported in marginnote-as-macro syntax."
  (skip-unless org-tufte-feature-more-expressive-inline-marginnotes)
  (should
   (org-tufte-test-in-exported-buffer
    "#+MACRO: prefix $1 macro
pre {{{marginnote(pre {{{prefix(text)}}})}}} post" t
    (let ((case-fold-search t))
      (search-forward "</span>)}}}" nil t)))))

(ert-deftest ox-tufte/marginnote-as-macro/inconvenience/commas-need-escaping ()
  "Nested macros are unsupported in marginnote-as-macro syntax."
  (skip-unless org-tufte-feature-more-expressive-inline-marginnotes)
  (should-not
   (org-tufte-test-in-exported-buffer
    " pre {{{marginnote(hello, world)}}} post" t
    (let ((case-fold-search t))
      (search-forward "hello, world" nil t)))))

(ert-deftest ox-tufte/marginnote-as-macro/links-supported ()
  "Links are supported in marginnote-as-macro syntax."
  (skip-unless org-tufte-feature-more-expressive-inline-marginnotes)
  (should
   (org-tufte-test-in-exported-buffer
    "pre{{{marginnote(<file:./image.png>​)}}} post" t
    (let ((case-fold-search t))
      (search-forward "<img " nil t)))))

(ert-deftest ox-tufte/marginnote-as-macro/nested-babel-calls ()
  "marginnote-as-macro supports nested babel calls w/ LOB ingestion."
  (skip-unless org-tufte-feature-more-expressive-inline-marginnotes)
  (let ((org-confirm-babel-evaluate nil))
    (should
     (org-tufte-test-in-exported-buffer
      "pre {{{marginnote(in macro call_nested-call() eom)}}} post
* resource :noexport:
#+name: nested-call
#+begin_src elisp :results value
  \"nested call\"
#+end_src" t
      (let ((case-fold-search t))
        (search-forward "<code>nested call</code> eom </span>" nil t))))))

(ert-deftest ox-tufte/marginnote-as-macro/newlines-supported ()
  "Newlines are supported in marginnote-as-macro syntax via ','."
  (should
   (org-tufte-test-in-exported-buffer
    "pre {{{marginnote(hello,world)}}} post" t
    (let ((case-fold-search t))
      (search-forward "hello<br>world </span>" nil t)))))


;;;; mn-as-babel-call syntax
(ert-deftest ox-tufte/marginnote-as-babel-call/inconvenience/text-has-to-be-quoted ()
  "Text has to be quoted when using marginnote-as-babel-call syntax."
  (should-error
   (org-tufte-test-in-exported-buffer
    "pre call_marginnote(hello world) post" t)))

(ert-deftest ox-tufte/marginnote-as-babel-call/silently-fail-when-expressive-syntax-disabled ()
  "Silently elide marginnote-as-babel-call when expressive syntax disabled."
  (let ((org-tufte-feature-more-expressive-inline-marginnotes nil)
        (org-confirm-babel-evaluate nil))
    (should
     (org-tufte-test-in-exported-buffer
      "pre call_marginnote(\"hello world\") post" t
      (let ((case-fold-search t))
        (or (search-forward "pre  post" nil t)
            (search-forward "pre post" nil t)))))))

(ert-deftest ox-tufte/marginnote-as-babel-call/links-supported ()
  "Links are supported in marginnote-as-babel-call syntax."
  (skip-unless org-tufte-feature-more-expressive-inline-marginnotes)
  (should
   (org-tufte-test-in-exported-buffer
    "pre call_marginnote(\"[[./image.png]]\") post" t
    (let ((case-fold-search t))
      (search-forward "<img " nil t)))))

(ert-deftest ox-tufte/marginnote-as-babel-call/nested-macros-supported ()
  "Marginnote-as-babel-call supports nested macros."
  (skip-unless org-tufte-feature-more-expressive-inline-marginnotes)
  (should
   (org-tufte-test-in-exported-buffer
    "#+MACRO: prefix $1 macro
pre call_marginnote(\"pre {{{prefix(text)}}}\") post" t
    (let ((case-fold-search t))
      (search-forward "text macro </span> post" nil t)))))

(ert-deftest ox-tufte/marginnote-as-babel-call/nested-babel-calls ()
  "Marginnote-as-babel-call supports nested babel calls w/ LOB ingestion."
  (skip-unless org-tufte-feature-more-expressive-inline-marginnotes)
  (let ((org-confirm-babel-evaluate nil))
    (should
     (org-tufte-test-in-exported-buffer
      "pre call_marginnote(\"in macro call_nested-call() eom\") post
* resource :noexport:
#+name: nested-call
#+begin_src elisp :results value
  \"nested call\"
#+end_src" t
      (let ((case-fold-search t))
        (search-forward "<code>nested call</code> eom </span>" nil t))))))

(ert-deftest ox-tufte/marginnote-as-babel-call/newlines-supported ()
  "Newlines are supported in marginnote-as-babel-call syntax via '\\'."
  (should
   (org-tufte-test-in-exported-buffer
    "pre call_marginnote(\"hello \\\\
world\") post" t
    (let ((case-fold-search t))
      (search-forward "hello <br>world </span>" nil t)))))


(provide 'ox-tufte-test)
;;; ox-tufte-test.el ends here

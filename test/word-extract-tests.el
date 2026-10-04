;;; word-extract-tests.el --- Tests for word-extract  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Blaine Mooers

;; This file is part of word-extract and is distributed under the terms of
;; the GNU General Public License version 3 or later.

;;; Commentary:

;; ERT tests for word-extract.  Run them in batch with:
;;
;;     make test
;;
;; or directly:
;;
;;     emacs -Q --batch -L . -l word-extract.el \
;;           -l test/word-extract-tests.el -f ert-run-tests-batch-and-exit
;;
;; The tests read two fixture files under test/fixtures/.  The date tests bind
;; `word-extract--today-midnight' to a fixed day so they stay deterministic.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'word-extract)

(defvar word-extract-tests--dir
  (file-name-directory (or load-file-name buffer-file-name default-directory))
  "Directory holding this test file.")

(defun word-extract-tests--fixtures ()
  "Return the absolute path of the fixtures directory."
  (expand-file-name "fixtures/" word-extract-tests--dir))

(defun word-extract-tests--file (rel)
  "Return the absolute path of fixture REL."
  (expand-file-name rel (word-extract-tests--fixtures)))

(defun word-extract-tests--sep ()
  "Return the 2026-09 fixture file."
  (word-extract-tests--file "2026-09/sample-2026-09-14.tex"))

(defun word-extract-tests--oct ()
  "Return the 2026-10 fixture file."
  (word-extract-tests--file "2026-10/sample-2026-10-02.tex"))

(defun word-extract-tests--bodies (results)
  "Return the :body strings of RESULTS."
  (mapcar (lambda (pl) (plist-get pl :body)) results))

;;;; Matching and context

(ert-deftest word-extract-test-whole-word-prose-context ()
  "A whole-word match returns the containing prose paragraph."
  (let ((word-extract-match-style 'word)
        (word-extract-context-lines nil))
    (let ((res (word-extract--scan-file (word-extract-tests--sep) '("book"))))
      ;; One paragraph mentions book (twice); the heading and \index are skipped.
      (should (= (length res) 1))
      (should (string= (plist-get (car res) :body)
                       (concat "I keep thinking about a book on crystallography.\n"
                               "A book like that could collect my notes into one place.")))
      (should (string= (plist-get (car res) :heading) "A book idea"))
      (should (= (plist-get (car res) :line) 7)))))

(ert-deftest word-extract-test-structural-lines-skipped ()
  "Matches inside a heading or an \\index entry are not returned."
  (let ((word-extract-match-style 'word))
    (let ((res (word-extract--scan-file (word-extract-tests--sep) '("book"))))
      (dolist (body (word-extract-tests--bodies res))
        (should-not (string-match-p "\\\\subsubsection" body))
        (should-not (string-match-p "\\\\index" body))))))

(ert-deftest word-extract-test-whole-word-excludes-substring ()
  "Whole-word matching does not match book inside notebook."
  (let ((word-extract-match-style 'word))
    (let ((bodies (word-extract-tests--bodies
                   (word-extract--scan-file (word-extract-tests--sep) '("book")))))
      (should-not (seq-some (lambda (b) (string-match-p "notebook" b)) bodies)))))

(ert-deftest word-extract-test-substring-includes-notebook ()
  "Substring matching catches book inside notebook as a second mention."
  (let ((word-extract-match-style 'substring))
    (let ((res (word-extract--scan-file (word-extract-tests--sep) '("book"))))
      (should (= (length res) 2))
      (should (seq-some (lambda (b) (string-match-p "notebook" b))
                        (word-extract-tests--bodies res))))))

(ert-deftest word-extract-test-multiple-words ()
  "Several words are searched at once, each mention recorded separately."
  (let ((word-extract-match-style 'word))
    (let ((res (word-extract--scan-file (word-extract-tests--sep)
                                        '("book" "pages"))))
      (should (= (length res) 2))
      (should (member "pages"
                      (apply #'append (mapcar (lambda (pl) (plist-get pl :words)) res)))))))

(ert-deftest word-extract-test-case-fold ()
  "Case-insensitive matching is on by default."
  (let ((word-extract-match-style 'word)
        (word-extract-case-fold t))
    (should (= (length (word-extract--scan-file (word-extract-tests--sep)
                                                '("BOOK")))
               1))))

(ert-deftest word-extract-test-context-lines ()
  "An integer `word-extract-context-lines' limits context to N lines each side."
  (let ((word-extract-match-style 'word)
        (word-extract-context-lines 0))
    (let ((res (word-extract--scan-file (word-extract-tests--sep)
                                        '("crystallography"))))
      (should (= (length res) 1))
      (should (string= (plist-get (car res) :body)
                       "I keep thinking about a book on crystallography.")))))

;;;; Gathering across files

(ert-deftest word-extract-test-gather-across-files ()
  "Mentions are gathered from every file, in file order."
  (let ((word-extract-match-style 'word)
        (files (list (word-extract-tests--sep) (word-extract-tests--oct))))
    (let ((res (word-extract--gather files '("book"))))
      (should (= (length res) 2))
      (should (string= (file-name-nondirectory (plist-get (nth 0 res) :file))
                       "sample-2026-09-14.tex"))
      (should (string= (file-name-nondirectory (plist-get (nth 1 res) :file))
                       "sample-2026-10-02.tex"))
      (should (string-match-p "sketched another book outline"
                              (plist-get (nth 1 res) :body))))))

;;;; Results buffer robustness

(ert-deftest word-extract-test-result-mode-hook-error-does-not-abort ()
  "A failing mode hook does not abort extraction or run in the scratch buffer."
  (require 'tex-mode)
  (let ((word-extract-match-style 'word)
        (ran nil))
    (let ((fn (lambda () (setq ran t) (error "boom from a mode hook"))))
      (add-hook 'latex-mode-hook fn)
      (add-hook 'text-mode-hook fn)
      (unwind-protect
          (progn
            (word-extract--run (list (word-extract-tests--sep)) '("book"))
            (with-current-buffer word-extract-result-buffer
              (should (save-excursion (goto-char (point-min))
                                      (search-forward "crystallography" nil t))))
            (should-not ran))
        (remove-hook 'latex-mode-hook fn)
        (remove-hook 'text-mode-hook fn)))))

;;;; Dates in file names

(ert-deftest word-extract-test-file-date-parsing ()
  "Both ISO and day-month-name-year file names parse to the right date."
  (should (equal (word-extract--file-date "sample-2026-09-14.tex")
                 (word-extract--encode 2026 9 14)))
  (should (equal (word-extract--file-date "26September2026.tex")
                 (word-extract--encode 2026 9 26)))
  (should (equal (word-extract--file-date "3Oct2026.tex")
                 (word-extract--encode 2026 10 3)))
  (should (null (word-extract--file-date "notes.tex"))))

(ert-deftest word-extract-test-files-in-range ()
  "A date range filters the fixtures and spans two monthly subfolders."
  (let ((root (word-extract-tests--fixtures))
        (word-extract-sort-files t))
    (let ((sep (word-extract-files-in-range
                (word-extract--encode 2026 9 1)
                (word-extract--encode 2026 9 30)
                root)))
      (should (= (length sep) 1))
      (should (string= (file-name-nondirectory (car sep))
                       "sample-2026-09-14.tex")))
    (let ((both (word-extract-files-in-range
                 (word-extract--encode 2026 9 1)
                 (word-extract--encode 2026 10 31)
                 root)))
      (should (= (length both) 2)))))

(ert-deftest word-extract-test-last-days-mocked-today ()
  "Last-N-days uses a fixed today and includes the right files."
  (let ((dir (make-temp-file "word-extract-named-" t))
        (word-extract-sort-files t))
    (unwind-protect
        (progn
          (dolist (n '("20September2026.tex" "26September2026.tex" "1October2026.tex"))
            (with-temp-file (expand-file-name n dir) (insert "")))
          (cl-letf (((symbol-function 'word-extract--today-midnight)
                     (lambda () (word-extract--encode 2026 10 2))))
            (should (= (length (word-extract-files-last-days 7 dir)) 2))
            (should (= (length (word-extract-files-last-days 2 dir)) 1))))
      (delete-directory dir t))))

;;;; Org output

(ert-deftest word-extract-test-org-output-parses ()
  "The org subtree written out is readable by Org's own API."
  (skip-unless (require 'org nil t))
  (let* ((word-extract-match-style 'word)
         (files (list (word-extract-tests--sep) (word-extract-tests--oct)))
         (tmp (make-temp-file "word-extract-org-" nil ".org")))
    (unwind-protect
        (progn
          (word-extract-to-org files '("book") tmp "Book mentions")
          (with-current-buffer (find-file-noselect tmp)
            (org-mode)
            (let ((tops (org-map-entries
                         (lambda () (nth 4 (org-heading-components))) "LEVEL=1"))
                  (dates (org-map-entries
                          (lambda () (org-entry-get (point) "DATE")) "LEVEL=2")))
              (should (= (length tops) 1))
              (should (= (length dates) 2))
              (should (member "2026-09-14" dates)))
            (goto-char (point-min))
            (should (search-forward "crystallography" nil t))
            (set-buffer-modified-p nil)
            (kill-buffer)))
      (when (file-exists-p tmp) (delete-file tmp)))))

(provide 'word-extract-tests)

;;; word-extract-tests.el ends here

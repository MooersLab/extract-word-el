;;; word-extract.el --- Retrieve the prose context around a word over a date range  -*- lexical-binding: t; -*-

;; Author: Blaine Mooers <blaine-mooers@ou.edu>
;; Maintainer: Blaine Mooers <blaine-mooers@ou.edu>
;; Version: 1.0
;; Keywords: tex, convenience, matching, wp
;; Package-Requires: ((emacs "26.1"))
;; URL: https://github.com/MooersLab/extract-word-el

;; Copyright (C) 2026 Blaine Mooers

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; word-extract finds a word wherever it appears in a set of LaTeX files and
;; copies the surrounding prose so you can read it in context.  It is the
;; companion of index-extract for the common situation in which you did not
;; tag the passage with an \index key.  It shares that package's design: one
;; document file per day, the files organized into monthly subfolders, and
;; commands that select files either by picking folders or by a date range
;; read from the file names.
;;
;; The motivating use is to find every place you mentioned a word across a
;; stretch of days, for example every entry in September that mentions "book",
;; so you can recover the book ideas you have not yet turned into projects.
;;
;; COMMANDS
;; --------
;; M-x word-extract
;;     Pick the files to scan (whole monthly subfolders and/or single files),
;;     then the word or words, and collect each mention with its surrounding
;;     prose into the *word-extract* buffer.
;;
;; M-x word-extract-last-days
;;     Scan every dated file from the last N days (default 7).  Files are found
;;     by searching your log root recursively and keeping those whose name
;;     holds a date within range.  With a prefix argument (C-u) the result is
;;     written to an org file instead of the buffer.
;;
;; M-x word-extract-range
;;     Like `word-extract-last-days' but with an explicit start and end date.
;;     With a prefix argument (C-u) the result goes to an org file.
;;
;; M-x word-extract-to-org
;;     Pick files and words, then append the mentions to an org file as one
;;     subtree: a top-level heading with one child heading per mention.
;;
;; In the *word-extract* buffer:
;;   C-c C-s  save the buffer to a file
;;   C-c C-w  copy just the context passages (no label lines) to the kill ring
;;   C-c C-o  write these same results to an org file (subtree)
;;   C-c C-k  run again with the same files but new words
;;   C-c C-f  run again with the same words but a new file selection
;;
;; WHAT COUNTS AS A MENTION AND ITS CONTEXT
;; ----------------------------------------
;; A mention is an occurrence of a word in a prose line.  By default a whole
;; word is matched, so "book" matches "book" and "Book" but not "notebook" or
;; "books" (see `word-extract-match-style').  A match that lands on a
;; structural line, that is a line beginning with a LaTeX command or a comment
;; such as a heading, an \index entry, or a \begin, is skipped, so the context
;; stays prose (see `word-extract-boundary-regexp').  The context is the
;; paragraph containing the mention, bounded by blank lines and structural
;; lines; set `word-extract-context-lines' to an integer to take that many
;; lines on each side instead.  Several mentions in one paragraph collapse to a
;; single entry whose words are merged.
;;
;; FILENAME DATES
;; --------------
;; The date commands read the date out of each file's name.  Two styles are
;; recognised: an ISO date such as 2026-09-26, and a day-month-name-year date
;; such as 26September2026 (the style this log uses).  Full month names and the
;; common three-letter abbreviations both work, and the month name is matched
;; without regard to case.  See `word-extract-date-regexp' and
;; `word-extract-date-named-regexp'.
;;
;; SETUP
;; -----
;;   (require 'word-extract)
;;   (setq word-extract-default-directory "~/2026words/Content/"
;;         word-extract-root-directory    "~/2026words/Content/"
;;         word-extract-default-days      7)

;;; Code:

(require 'subr-x)
(require 'seq)
(require 'time-date)

;; Optional Org integration; only called when Org is loaded.
(declare-function org-reveal "org" (&optional siblings))
(declare-function org-read-date "org"
                  (&optional with-time to-time from-string prompt
                             default-time default-input inactive))

(defgroup word-extract nil
  "Retrieve the prose context around a word over a date range."
  :group 'tex
  :prefix "word-extract-")

(defcustom word-extract-default-directory nil
  "Directory where the file prompts start.
Nil means start in `default-directory'.  For a monthly log you might set this
to, for example, \"~/2026words/Content/\"."
  :type '(choice (const :tag "Current directory" nil) directory))

(defcustom word-extract-root-directory nil
  "Root folder the date commands search recursively for dated files.
Nil falls back to `word-extract-default-directory', and failing that you are
prompted once.  Set it to the folder that holds your monthly subfolders so
`word-extract-last-days' never has to ask."
  :type '(choice (const :tag "Fall back / prompt" nil) directory))

(defcustom word-extract-default-days 7
  "Default number of days for `word-extract-last-days'."
  :type 'integer)

(defcustom word-extract-file-glob "*.tex"
  "Shell wildcard used when adding a whole directory of files."
  :type 'string)

(defcustom word-extract-date-regexp
  "\\([0-9]\\{4\\}\\)-\\([0-9]\\{1,2\\}\\)-\\([0-9]\\{1,2\\}\\)"
  "Regexp matching an ISO YYYY-MM-DD date in a file name.
Groups 1, 2 and 3 are the year, month and day."
  :type 'regexp)

(defcustom word-extract-date-named-regexp
  (concat "\\([0-9]\\{1,2\\}\\)"
          "\\(January\\|February\\|March\\|April\\|May\\|June\\|July"
          "\\|August\\|September\\|October\\|November\\|December"
          "\\|Jan\\|Feb\\|Mar\\|Apr\\|Jun\\|Jul\\|Aug\\|Sept\\|Sep"
          "\\|Oct\\|Nov\\|Dec\\)"
          "\\([0-9]\\{4\\}\\)")
  "Regexp matching a DayMonthNameYear date in a file name.
For example it matches the 26September2026 in \"26September2026.tex\".
Group 1 is the day, group 2 the month name, and group 3 the year.
Matching ignores case (see `word-extract--file-date')."
  :type 'regexp)

(defconst word-extract--month-names
  '(("january" . 1) ("february" . 2) ("march" . 3) ("april" . 4)
    ("may" . 5) ("june" . 6) ("july" . 7) ("august" . 8)
    ("september" . 9) ("october" . 10) ("november" . 11) ("december" . 12)
    ("jan" . 1) ("feb" . 2) ("mar" . 3) ("apr" . 4) ("jun" . 6)
    ("jul" . 7) ("aug" . 8) ("sep" . 9) ("sept" . 9) ("oct" . 10)
    ("nov" . 11) ("dec" . 12))
  "Alist mapping lower-case English month names and abbreviations to numbers.")

(defcustom word-extract-match-style 'word
  "How a requested word is matched against the text.
`word'       the word must appear as a whole word.
`substring'  the word may appear anywhere, so \"book\" matches \"notebook\".
`regexp'     the word is treated as an Emacs regexp."
  :type '(choice (const word) (const substring) (const regexp)))

(defcustom word-extract-case-fold t
  "If non-nil, word matching ignores case."
  :type 'boolean)

(defcustom word-extract-include-heading t
  "If non-nil, label each mention with the nearest preceding section heading."
  :type 'boolean)

(defcustom word-extract-context-lines nil
  "How much context to retrieve around a mention.
Nil takes the whole paragraph containing the mention, bounded by blank lines
and structural lines.  An integer N instead takes N lines on each side of the
line that holds the mention."
  :type '(choice (const :tag "Containing paragraph" nil) integer))

(defcustom word-extract-boundary-regexp "^[ \t]*\\(?:%\\|\\\\\\)"
  "Lines matching this regexp are structural and bound a context paragraph.
By default a structural line is one that begins with a comment, the percent
sign, or a LaTeX command introduced by a backslash, such as a sectioning
command, an \\index entry, or a \\begin.  A mention that lands on such a line
is skipped, and a paragraph of context never crosses one, so the retrieved
context is prose."
  :type 'regexp)

(defcustom word-extract-heading-regexp
  (concat "^[ \t]*\\\\\\(?:sub\\)*section\\*?{\\([^}]*\\)}"
          "\\|^[ \t]*\\\\\\(?:chapter\\|paragraph\\|subparagraph\\)\\*?{\\([^}]*\\)}")
  "Regexp that matches a sectioning command.
Group 1 or group 2 holds the section title."
  :type 'regexp)

(defcustom word-extract-result-buffer "*word-extract*"
  "Name of the buffer that collects the mentions and their context."
  :type 'string)

(defcustom word-extract-result-major-mode #'latex-mode
  "Major mode for the results buffer, or nil for no special mode.
The mode is turned on with its mode hooks suppressed, so a heavy
`latex-mode-hook', for example one that starts an LSP server, does not run in
this scratch buffer.  Set this to nil if you would rather the results buffer
stay in Fundamental mode."
  :type '(choice (const :tag "None" nil) function))

(defcustom word-extract-org-file nil
  "Default org file for the org-output commands.  Nil means always prompt."
  :type '(choice (const :tag "Always prompt" nil) file))

(defcustom word-extract-sort-files t
  "If non-nil, scan files in sorted (alphabetical) order.
Date-stamped file names then come out in chronological order."
  :type 'boolean)

(defvar word-extract--last-files nil
  "File list used by the most recent extraction.")

(defvar word-extract--last-words nil
  "Word list used by the most recent extraction.")

(defvar word-extract--last-results nil
  "Mention plists produced by the most recent extraction.")

(defun word-extract--words-string (words)
  "Return WORDS as a single comma-separated string."
  (mapconcat #'identity words ", "))

;;;; Dates in file names

(defun word-extract--encode (year month day)
  "Return a Lisp time value for midnight on YEAR, MONTH, and DAY."
  (encode-time (list 0 0 0 day month year nil -1 nil)))

(defun word-extract--month-number (name)
  "Return the month number 1 to 12 for NAME, or nil if NAME is not a month."
  (cdr (assoc (downcase name) word-extract--month-names)))

(defun word-extract--file-date (file)
  "Return the date encoded in FILE's name as a Lisp time value, or nil.
Two filename styles are recognised: an ISO date such as 2026-09-26 (see
`word-extract-date-regexp') and a day-month-name-year date such as
26September2026 (see `word-extract-date-named-regexp').  The month name is
matched without regard to case."
  (let ((name (file-name-nondirectory file))
        (case-fold-search t))
    (cond
     ((string-match word-extract-date-regexp name)
      (word-extract--encode (string-to-number (match-string 1 name))
                            (string-to-number (match-string 2 name))
                            (string-to-number (match-string 3 name))))
     ((string-match word-extract-date-named-regexp name)
      (let ((month (word-extract--month-number (match-string 2 name))))
        (when month
          (word-extract--encode (string-to-number (match-string 3 name))
                                month
                                (string-to-number (match-string 1 name)))))))))

(defun word-extract--today-midnight ()
  "Return a Lisp time value for midnight today."
  (let ((n (decode-time)))
    (word-extract--encode (nth 5 n) (nth 4 n) (nth 3 n))))

(defun word-extract--resolve-root ()
  "Return the log root folder for the date commands."
  (or word-extract-root-directory
      word-extract-default-directory
      (read-directory-name "Log root folder (searched recursively): "
                           default-directory)))

;;;; File selection

(defun word-extract--files-in-directory (dir recursive)
  "Return the files in DIR matching `word-extract-file-glob'.
If RECURSIVE is non-nil, descend into subdirectories."
  (let ((dir (file-name-as-directory (expand-file-name dir)))
        (re (wildcard-to-regexp word-extract-file-glob)))
    (if recursive
        (directory-files-recursively dir re)
      (directory-files dir t re))))

(defun word-extract-read-files ()
  "Interactively build and return the list of .tex files to scan.
You can add whole directories (one per monthly subfolder) and single files, so
a week that spans two or more months is easy to cover."
  (let ((files '())
        (start (or word-extract-default-directory default-directory))
        (done nil))
    (while (not done)
      (let ((choice
             (car (read-multiple-choice
                   (format "Add to scan list (%d file(s) so far): " (length files))
                   '((?d "directory" "Add every matching file in a folder")
                     (?f "file"      "Add a single file")
                     (?l "list"      "Show the files chosen so far")
                     (?x "done"      "Finish and start scanning"))))))
        (pcase choice
          (?d (let* ((dir (read-directory-name "Folder to scan: " start))
                     (rec (y-or-n-p "Include its subfolders too? "))
                     (new (word-extract--files-in-directory dir rec)))
                (setq start dir)
                (if new
                    (progn (setq files (append files new))
                           (message "Added %d file(s) from %s" (length new) dir))
                  (message "No %s files in %s" word-extract-file-glob dir))))
          (?f (let ((f (read-file-name "File to add: " start nil t)))
                (when (and f (file-readable-p f))
                  (push (expand-file-name f) files)
                  (message "Added %s" f))))
          (?l (message "%s"
                       (if files
                           (mapconcat #'identity (reverse files) "\n")
                         "(none chosen yet)")))
          (?x (setq done t)))))
    (setq files (delete-dups (mapcar #'expand-file-name files)))
    (when word-extract-sort-files
      (setq files (sort files #'string<)))
    (unless files (user-error "No files selected"))
    files))

(defun word-extract-files-in-range (start end &optional root)
  "Return dated .tex files under ROOT whose filename date is in [START, END].
START and END are Lisp time values and the range is inclusive.  ROOT defaults
to the log root (see `word-extract--resolve-root') and is searched
recursively, so files from any monthly subfolder are considered."
  (let* ((root (file-name-as-directory
                (expand-file-name (or root (word-extract--resolve-root)))))
         (cands (directory-files-recursively
                 root (wildcard-to-regexp word-extract-file-glob)))
         (hits '()))
    (dolist (f cands)
      (let ((d (word-extract--file-date f)))
        (when (and d
                   (not (time-less-p d start))
                   (not (time-less-p end d)))
          (push f hits))))
    (setq hits (delete-dups hits))
    (when word-extract-sort-files (setq hits (sort hits #'string<)))
    hits))

(defun word-extract-files-last-days (days &optional root)
  "Return dated files under ROOT from the last DAYS days, ending today.
DAYS counts today and the DAYS-1 days before it."
  (let* ((end (word-extract--today-midnight))
         (start (time-subtract end (days-to-time (1- (max 1 days))))))
    (word-extract-files-in-range start end root)))

;;;; Words

(defun word-extract-read-words ()
  "Prompt for one or more words to find, returning them as a list."
  (let* ((raw (completing-read-multiple
               "Word(s) to find (comma-separated): " nil))
         (words (seq-remove #'string-empty-p
                            (delete-dups (mapcar #'string-trim (remq nil raw))))))
    (unless words (user-error "No words given"))
    words))

(defun word-extract--search-regexp (words)
  "Return a single search regexp matching any of WORDS per the match style."
  (let ((alts (mapcar (lambda (w)
                        (if (eq word-extract-match-style 'regexp)
                            w
                          (regexp-quote w)))
                      words)))
    (let ((body (concat "\\(?:" (mapconcat #'identity alts "\\|") "\\)")))
      (if (eq word-extract-match-style 'word)
          (concat "\\<" body "\\>")
        body))))

;;;; Context extraction

(defun word-extract--boundary-line-p ()
  "Return non-nil if the current line is blank or a structural line."
  (or (looking-at-p "^[ \t]*$")
      (looking-at-p word-extract-boundary-regexp)))

(defun word-extract--context-bounds (match-pos)
  "Return (START . END) for the context around MATCH-POS.
With `word-extract-context-lines' an integer, take that many lines on each side
of the line holding MATCH-POS.  Otherwise take the paragraph of prose lines
around it, bounded by blank lines and structural lines."
  (save-excursion
    (goto-char match-pos)
    (beginning-of-line)
    (if (integerp word-extract-context-lines)
        (let* ((n (max 0 word-extract-context-lines))
               (start (save-excursion (forward-line (- n)) (point)))
               (end (save-excursion
                      (forward-line n)
                      (line-end-position))))
          (cons start end))
      (let ((start (point))
            (end (line-end-position)))
        ;; Extend upward over prose lines.
        (while (and (> start (point-min))
                    (save-excursion
                      (goto-char start) (forward-line -1)
                      (not (word-extract--boundary-line-p))))
          (setq start (save-excursion (goto-char start) (forward-line -1) (point))))
        ;; Extend downward over prose lines.
        (while (save-excursion
                 (goto-char end)
                 (and (not (eobp))
                      (progn (forward-line 1)
                             (and (not (eobp))
                                  (not (word-extract--boundary-line-p))))))
          (setq end (save-excursion (goto-char end) (forward-line 1)
                                    (line-end-position))))
        (cons start end)))))

(defun word-extract--current-heading ()
  "Return the title of the nearest sectioning command before point, or nil."
  (save-excursion
    (when (re-search-backward word-extract-heading-regexp nil t)
      (string-trim (or (match-string 1) (match-string 2))))))

(defun word-extract--scan-file (file words)
  "Return a list of mention plists for WORDS found in FILE.
Each plist has :file :line :heading :words :body.  Matches on structural lines
are skipped, and several mentions in one context collapse to one plist whose
words are merged."
  (let ((results '())
        (seen (make-hash-table :test 'eql))
        (regexp (word-extract--search-regexp words))
        (case-fold-search word-extract-case-fold))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (while (re-search-forward regexp nil t)
        (let ((mbeg (match-beginning 0))
              (mend (match-end 0))
              (matched (match-string 0)))
          (unless (save-excursion (goto-char mbeg) (beginning-of-line)
                                  (word-extract--boundary-line-p))
            (let* ((bounds (word-extract--context-bounds mbeg))
                   (start (car bounds))
                   (end (cdr bounds))
                   (prev (gethash start seen)))
              (if prev
                  (plist-put prev :words
                             (delete-dups (append (plist-get prev :words)
                                                  (list matched))))
                (let ((pl (list :file file
                                :line (line-number-at-pos mbeg)
                                :heading (and word-extract-include-heading
                                              (save-excursion
                                                (goto-char mbeg)
                                                (word-extract--current-heading)))
                                :words (list matched)
                                :body (string-trim
                                       (buffer-substring-no-properties start end)))))
                  (puthash start pl seen)
                  (push pl results)))))
          (when (= mbeg mend)
            (goto-char (min (point-max) (1+ mend))))))
      (nreverse results))))

(defun word-extract--gather (files words)
  "Scan FILES for WORDS and return the list of mention plists, in file order."
  (let ((ordered (if word-extract-sort-files
                     (sort (copy-sequence files) #'string<)
                   files))
        (all '()))
    (dolist (f ordered)
      (when (file-readable-p f)
        (setq all (append all (word-extract--scan-file f words)))))
    all))

;;;; Results buffer (LaTeX comments)

(defun word-extract--format (results)
  "Insert the formatted RESULTS (a list of mention plists) at point."
  (dolist (pl results)
    (let ((file (plist-get pl :file))
          (line (plist-get pl :line))
          (heading (plist-get pl :heading))
          (words (plist-get pl :words))
          (body (plist-get pl :body)))
      (insert (format "%%%% ---- %s : line %d : word%s %s%s ----\n"
                      (file-name-nondirectory file)
                      line
                      (if (cdr words) "s" "")
                      (mapconcat (lambda (w) (format "\"%s\"" w)) words ", ")
                      (if heading (format " : [%s]" heading) "")))
      (insert (if (string-empty-p body) "%% (no context found)\n" (concat body "\n")))
      (insert "\n"))))

(defun word-extract--enable-result-mode ()
  "Turn on `word-extract-result-major-mode' in the current buffer.
The mode hooks are suppressed, so a heavy `latex-mode-hook', for example one
that starts an LSP server, does not run in this scratch results buffer.  If
the mode cannot be enabled, fall back to `fundamental-mode' and report the
problem rather than aborting the extraction."
  (when word-extract-result-major-mode
    (condition-case err
        ;; Bind `delayed-mode-hooks' so the hooks suppressed here are discarded
        ;; on exit and never flushed into a later buffer.
        (let ((delayed-mode-hooks nil))
          (delay-mode-hooks (funcall word-extract-result-major-mode)))
      (error
       (ignore-errors (fundamental-mode))
       (message "word-extract: could not enable %s (%s); using fundamental-mode"
                word-extract-result-major-mode
                (error-message-string err))))))

(defun word-extract--run (files words)
  "Gather mentions of WORDS from FILES into `word-extract-result-buffer'.
Return the list of mention plists."
  (setq word-extract--last-files files
        word-extract--last-words words)
  (let* ((all (word-extract--gather files words))
         (nfiles (length (seq-filter #'file-readable-p files)))
         (buf (get-buffer-create word-extract-result-buffer)))
    (setq word-extract--last-results all)
    (with-current-buffer buf
      (erase-buffer)
      (word-extract--enable-result-mode)
      (word-extract-result-mode 1)
      (insert (format "%%%% word-extract: %d mention(s) of %s in %d file(s)\n"
                      (length all) (word-extract--words-string words) nfiles))
      (insert (format "%%%% generated %s\n\n" (format-time-string "%Y-%m-%d %H:%M")))
      (word-extract--format all)
      (goto-char (point-min)))
    (display-buffer buf)
    (message "word-extract: %d mention(s) from %d file(s)" (length all) nfiles)
    all))

(defvar word-extract-result-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "C-c C-s") #'word-extract-save-results)
    (define-key m (kbd "C-c C-w") #'word-extract-copy-bodies)
    (define-key m (kbd "C-c C-o") #'word-extract-results-to-org)
    (define-key m (kbd "C-c C-k") #'word-extract-again-words)
    (define-key m (kbd "C-c C-f") #'word-extract-again-files)
    m)
  "Keymap for `word-extract-result-mode'.")

(define-minor-mode word-extract-result-mode
  "Minor mode for the `word-extract' results buffer."
  :lighter " WrdX")

(defun word-extract-copy-bodies ()
  "Copy just the context passages (no \"%%\" label lines) to the kill ring."
  (interactive)
  (let ((lines '()))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (unless (looking-at-p "^%%")
          (push (buffer-substring-no-properties
                 (line-beginning-position) (line-end-position))
                lines))
        (forward-line 1)))
    (kill-new (string-trim (mapconcat #'identity (nreverse lines) "\n")))
    (message "Copied the context passages to the kill ring")))

(defun word-extract-save-results (file)
  "Write the results buffer to FILE."
  (interactive
   (list (read-file-name "Save mentions to: "
                         (or word-extract-default-directory default-directory))))
  (write-region (point-min) (point-max) file)
  (message "Saved to %s" file))

;;;; Org output

(defun word-extract--read-org-file ()
  "Read the target org file for the org-output commands."
  (read-file-name
   "Org file to append to: "
   (and word-extract-org-file (file-name-directory word-extract-org-file))
   word-extract-org-file nil
   (and word-extract-org-file (file-name-nondirectory word-extract-org-file))))

(defun word-extract--org-clean (s)
  "Make S safe for an org headline: single line, no leading stars."
  (setq s (replace-regexp-in-string "[\n\r]+" " " (or s "")))
  (setq s (replace-regexp-in-string "\\`\\*+" "" s))
  (string-trim s))

(defun word-extract--write-org (results words files org-file top-heading)
  "Append RESULTS to ORG-FILE as one org subtree under TOP-HEADING.
WORDS and FILES are used only for the top heading's properties.  Return nil."
  (with-current-buffer (find-file-noselect org-file)
    (goto-char (point-max))
    (unless (bolp) (insert "\n"))
    (insert "\n")
    (let ((top-start (point))
          (stamp (format-time-string "[%Y-%m-%d %a %H:%M]")))
      (insert (format "* %s\n" (word-extract--org-clean top-heading)))
      (insert ":PROPERTIES:\n")
      (insert (format ":WORDS: %s\n" (word-extract--words-string words)))
      (insert (format ":FILES: %d\n" (length (seq-filter #'file-readable-p files))))
      (insert (format ":MENTIONS: %d\n" (length results)))
      (insert (format ":GENERATED: %s\n" stamp))
      (insert ":END:\n")
      (dolist (pl results)
        (let* ((file (plist-get pl :file))
               (date (word-extract--file-date file))
               (datestr (and date (format-time-string "%Y-%m-%d" date)))
               (heading (or (plist-get pl :heading) (car (plist-get pl :words))))
               (title (word-extract--org-clean
                       (if datestr (format "%s  (%s)" heading datestr) heading)))
               (body (plist-get pl :body)))
          (insert (format "** %s\n" title))
          (insert ":PROPERTIES:\n")
          (insert (format ":SOURCE: %s\n" (file-name-nondirectory file)))
          (insert (format ":LINE: %d\n" (plist-get pl :line)))
          (insert (format ":WORDS: %s\n" (word-extract--words-string (plist-get pl :words))))
          (when datestr (insert (format ":DATE: %s\n" datestr)))
          (insert ":END:\n")
          (unless (string-empty-p body) (insert body "\n"))))
      (save-buffer)
      (goto-char top-start)
      (when (and (fboundp 'org-mode) (not (derived-mode-p 'org-mode)))
        (org-mode))
      (when (derived-mode-p 'org-mode) (ignore-errors (org-reveal))))
    (pop-to-buffer (current-buffer)))
  nil)

;;;; Main entry points

;;;###autoload
(defun word-extract (files words)
  "Find WORDS in FILES and collect each mention with its context into a buffer.
Interactively, prompt first for the files to scan (you may add several monthly
subfolders and individual files) and then for the word or words.  The mentions
are collected into `word-extract-result-buffer' in file order."
  (interactive
   (let* ((files (word-extract-read-files))
          (words (word-extract-read-words)))
     (list files words)))
  (word-extract--run files words))

;;;###autoload
(defun word-extract-last-days (days words &optional to-org)
  "Find WORDS in files dated within the last DAYS days.
The log root (`word-extract-root-directory') is searched recursively and files
whose name holds a date in range are kept, so you need not pick folders and a
week spanning two months just works.  The ISO and day-month-name-year filename
styles are both recognised.  With a prefix argument (non-nil TO-ORG), write the
result to an org file instead of the buffer."
  (interactive
   (let* ((days (read-number "Number of days back (including today): "
                             word-extract-default-days))
          (files (word-extract-files-last-days days)))
     (unless files (user-error "No dated files in the last %d day(s)" days))
     (list days (word-extract-read-words) current-prefix-arg)))
  (let ((files (word-extract-files-last-days days)))
    (unless files (user-error "No dated files in the last %d day(s)" days))
    (if to-org
        (word-extract-to-org files words (word-extract--read-org-file)
                             (format "Last %d days: %s"
                                     days (word-extract--words-string words)))
      (word-extract--run files words))))

;;;###autoload
(defun word-extract-range (start end words &optional to-org)
  "Find WORDS in files whose filename date is in [START, END].
START and END are Lisp time values (read interactively).  The log root is
searched recursively.  With a prefix argument (non-nil TO-ORG), write to an
org file."
  (interactive
   (let* ((start (word-extract--read-date "Start date"))
          (end (word-extract--read-date "End date"))
          (files (word-extract-files-in-range start end)))
     (unless files (user-error "No dated files in that range"))
     (list start end (word-extract-read-words) current-prefix-arg)))
  (let ((files (word-extract-files-in-range start end)))
    (unless files (user-error "No dated files in that range"))
    (if to-org
        (word-extract-to-org files words (word-extract--read-org-file)
                             (format "%s to %s: %s"
                                     (format-time-string "%Y-%m-%d" start)
                                     (format-time-string "%Y-%m-%d" end)
                                     (word-extract--words-string words)))
      (word-extract--run files words))))

;;;###autoload
(defun word-extract-to-org (files words org-file &optional top-heading)
  "Find WORDS in FILES and append the mentions to ORG-FILE.
The mentions form one org subtree: a top-level heading (TOP-HEADING) with one
child heading per mention.  Each child carries :SOURCE:, :LINE:, :WORDS: and
:DATE: properties and the context passage as its body."
  (interactive
   (let* ((files (word-extract-read-files))
          (words (word-extract-read-words))
          (org-file (word-extract--read-org-file)))
     (list files words org-file nil)))
  (setq word-extract--last-files files
        word-extract--last-words words)
  (let ((all (word-extract--gather files words)))
    (setq word-extract--last-results all)
    (word-extract--write-org
     all words files org-file
     (or top-heading (format "Word extract: %s" (word-extract--words-string words))))
    (message "word-extract: wrote %d mention(s) to %s" (length all) org-file)
    all))

;;;###autoload
(defun word-extract-results-to-org (org-file)
  "Append the most recent extraction results to ORG-FILE as an org subtree."
  (interactive (list (word-extract--read-org-file)))
  (unless word-extract--last-results
    (user-error "No extraction results yet; run word-extract first"))
  (word-extract--write-org
   word-extract--last-results word-extract--last-words word-extract--last-files
   org-file
   (format "Word extract: %s" (word-extract--words-string word-extract--last-words)))
  (message "word-extract: wrote %d mention(s) to %s"
           (length word-extract--last-results) org-file))

(defun word-extract--read-date (prompt)
  "Read a date with PROMPT and return it as a Lisp time value.
Uses `org-read-date' if available, otherwise reads a YYYY-MM-DD string."
  (if (fboundp 'org-read-date)
      (org-read-date nil t nil prompt)
    (let ((s (read-string (concat prompt " (YYYY-MM-DD): "))))
      (if (string-match word-extract-date-regexp s)
          (word-extract--encode (string-to-number (match-string 1 s))
                                (string-to-number (match-string 2 s))
                                (string-to-number (match-string 3 s)))
        (user-error "Could not parse date: %s" s)))))

;;;###autoload
(defun word-extract-again-words ()
  "Run `word-extract' again with the same files but new words."
  (interactive)
  (unless word-extract--last-files (user-error "No previous file list"))
  (word-extract--run word-extract--last-files
                     (word-extract-read-words)))

;;;###autoload
(defun word-extract-again-files ()
  "Run `word-extract' again with the same words but a new file selection."
  (interactive)
  (unless word-extract--last-words (user-error "No previous word list"))
  (word-extract--run (word-extract-read-files) word-extract--last-words))

(provide 'word-extract)

;;; word-extract.el ends here

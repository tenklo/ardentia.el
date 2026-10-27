;;; ardentia-stats.el --- Read-only dashboards about the agent team -*- lexical-binding: t -*-

;; Copyright (C) 2026 Karsten Klöss

;; Author: Karsten Klöss
;; Assisted-by: Claude Code:Claude Opus 5.5
;; URL: https://github.com/tenklo/ardentia.el
;; Keywords: outlines, tools
;; Package-Requires: ((emacs "30.1"))
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

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

;; `ardentia-stats' shows the buffer `*ardentia-stats*': dashboards about
;; what the agent team does, read from one JSON file that an external tool
;; writes (`ardentia-stats-file'; the format is described in README.md,
;; "Agent stats").  This module only renders; it computes nothing.  Every
;; dashboard is a section with a title, an optional note and a generic
;; table whose columns carry a type (text, int, tokens, usd, percent,
;; duration, date), so a new dashboard needs no change here.
;;
;; Sections fold with TAB and S-TAB like the groups of the task list.  RET
;; on a row that carries an org ID visits that heading in another window.
;; The buffer re-renders by itself when the file changes on disk: one
;; global timer compares the modification time and size of the file with
;; those of the last render (`ardentia-stats-check-interval').  A missing,
;; unreadable or malformed file, or an unknown format version, is shown
;; plainly in the buffer; nothing is signalled and no data is invented.
;;
;; The functions in the "Pure parts" section have no side effects, so they
;; can be tested directly.

;;; Code:

(require 'seq)
(require 'cl-lib)
(require 'subr-x)
(require 'parse-time)
(require 'ardentia-core)

(declare-function ardentia "ardentia" ())
(declare-function org-id-goto "org-id" (id))

(defcustom ardentia-stats-file "~/.cache/agent-stats/summary.json"
  "JSON file the agent stats view renders.
An external tool writes it; the format is described in README.md,
section \"Agent stats\"."
  :type 'file
  :group 'ardentia)

(defcustom ardentia-stats-render-command nil
  "Command that writes `ardentia-stats-file' with a given window.
A list (PROGRAM ARG...) or a string split like a shell command line.
The view runs it with `--days N --weeks M' appended, N and M being the
number of days and of weeks the dashboards should cover, when you
change the window with \\<ardentia-stats-mode-map>\\[ardentia-stats-set-days] or \\[ardentia-stats-set-weeks].  PROGRAM is
expanded with `expand-file-name' when it names a path, else looked up
with `executable-find'.  Nil means the window cannot be changed from
Emacs."
  :type '(choice (const :tag "Not set" nil)
                 (repeat :tag "Program and arguments" string)
                 (string :tag "Command line"))
  :group 'ardentia)

(defcustom ardentia-stats-section-order nil
  "Keys of the sections in the order you want them, first to last.
Sections whose key is not listed follow in the order of the file.
\\<ardentia-stats-mode-map>\\[ardentia-stats-move-section-up] and \\[ardentia-stats-move-section-down] change it and save it."
  :type '(repeat string)
  :group 'ardentia)

(defface ardentia-stats-heading
  '((t :inherit font-lock-type-face :weight bold))
  "The title of a section of the agent stats view and its fold indicator.
Looks like `ardentia-group', the group rows of the task list."
  :group 'ardentia-faces)

(defface ardentia-stats-note
  '((t :inherit shadow))
  "Notes and the generation time in the agent stats view."
  :group 'ardentia-faces)

(defface ardentia-stats-column
  '((t :weight bold))
  "The column titles of the tables in the agent stats view."
  :group 'ardentia-faces)

(defconst ardentia-stats--version 1
  "The format version of `ardentia-stats-file' this code reads.")

(defconst ardentia-stats--text-width 50
  "Text cells longer than this many columns are cut with an ellipsis.")

(defconst ardentia-stats--bar-width 8
  "Width of the bar of a percent cell, in character cells.")

(defconst ardentia-stats--numeric-types '("int" "tokens" "usd" "percent" "duration")
  "Column types whose cells are right-aligned.")

;;;; Pure parts

(defun ardentia-stats--bar (fraction)
  "Return a bar of `ardentia-stats--bar-width' cells for FRACTION (0..1).
Values outside 0..1 are clamped.  Eighth blocks make the last cell."
  (let* ((width ardentia-stats--bar-width)
         (eighths (round (* (min 1.0 (max 0.0 (float fraction))) width 8)))
         (full (/ eighths 8))
         (rest (% eighths 8)))
    (concat (make-string full ?█)
            (if (> rest 0) (string (aref "▏▎▍▌▋▊▉" (1- rest))) "")
            (make-string (- width full (if (> rest 0) 1 0)) ?\s))))

(defun ardentia-stats--format-duration (seconds)
  "Format SECONDS as \"42s\", \"4m\", \"1h12m\" or \"2d3h\"."
  (let ((s (round seconds)))
    (cond ((< s 60) (format "%ds" s))
          ((< s 3600) (format "%dm" (/ s 60)))
          ((< s 86400) (format "%dh%02dm" (/ s 3600) (/ (% s 3600) 60)))
          (t (format "%dd%dh" (/ s 86400) (/ (% s 86400) 3600))))))

(defun ardentia-stats--format-cell (value type)
  "Return the text of the cell VALUE in a column of TYPE (a string).
Nil (JSON null) gives \"-\".  A value that does not fit its type, and
any unknown type, is shown with `format' \"%s\"."
  (let ((num (and (numberp value) (>= value 0))))
    (cond ((null value) "-")
          ((and (equal type "int") (numberp value)) (format "%d" (round value)))
          ((and (equal type "tokens") num) (ardentia--format-tokens value))
          ((and (equal type "usd") (numberp value)) (format "$%.2f" value))
          ((and (equal type "percent") (numberp value))
           (format "%4s %s" (format "%d%%" (round (* 100 value)))
                   (ardentia-stats--bar value)))
          ((and (equal type "duration") num)
           (ardentia-stats--format-duration value))
          (t (format "%s" value)))))

(defun ardentia-stats--right-p (type)
  "Return non-nil when cells of a column of TYPE are right-aligned."
  (member type ardentia-stats--numeric-types))

(defun ardentia-stats--truncate (string)
  "Cut STRING to `ardentia-stats--text-width' columns with an ellipsis."
  (if (> (string-width string) ardentia-stats--text-width)
      (truncate-string-to-width string ardentia-stats--text-width nil nil "…")
    string))

(defun ardentia-stats--pad (string width right)
  "Pad STRING with spaces to WIDTH columns; on the left when RIGHT."
  (let ((fill (make-string (max 0 (- width (string-width string))) ?\s)))
    (if right (concat fill string) (concat string fill))))

(defun ardentia-stats--table (columns rows)
  "Return the lines of the table of COLUMNS and ROWS.
COLUMNS and ROWS are the parsed JSON of a section.  The result is a
list of (TEXT . ID): first the column titles (ID nil), then one line per
row with the row's ID, or nil.  Numbers are right-aligned, text is
left-aligned and cut at `ardentia-stats--text-width'."
  (let* ((types (mapcar (lambda (c) (ardentia--stringify (alist-get 'type c)))
                        columns))
         (names (mapcar (lambda (c) (format "%s" (or (alist-get 'name c) "")))
                        columns))
         (cells (mapcar
                 (lambda (row)
                   (let ((values (alist-get 'cells row)))
                     (seq-map-indexed
                      (lambda (type i)
                        (let ((text (ardentia-stats--format-cell
                                     (nth i values) type)))
                          (if (ardentia-stats--right-p type)
                              text
                            (ardentia-stats--truncate text))))
                      types)))
                 rows))
         (widths (seq-map-indexed
                  (lambda (name i)
                    (apply #'max (string-width name)
                           (mapcar (lambda (r) (string-width (nth i r))) cells)))
                  names))
         (line (lambda (texts)
                 (string-trim-right
                  (string-join
                   (seq-map-indexed
                    (lambda (text i)
                      (ardentia-stats--pad text (nth i widths)
                                           (ardentia-stats--right-p (nth i types))))
                    texts)
                   "  ")))))
    (cons (cons (propertize (funcall line names) 'face 'ardentia-stats-column) nil)
          (seq-mapn (lambda (row texts)
                      (let ((id (alist-get 'id row)))
                        (cons (funcall line texts)
                              (and (stringp id) (not (string-empty-p id)) id))))
                    rows cells))))

(defun ardentia-stats--parse (string)
  "Parse the JSON STRING of the stats file.
Return (:data ALIST) or (:error MESSAGE).  The error covers bad JSON, a
top level that is not an object, a version other than
`ardentia-stats--version' and sections that are not a list."
  (pcase (condition-case err
             (json-parse-string string :object-type 'alist :array-type 'list
                                :null-object nil :false-object "false")
           (error (list :error (format "bad JSON: %s"
                                       (error-message-string err)))))
    ((and `(:error ,_) result) result)
    ((and data (guard (not (and (consp data) (consp (car data))
                                (symbolp (caar data))))))
     (list :error "bad format: the top level is not a JSON object"))
    ((and data (guard (not (equal (alist-get 'version data)
                                  ardentia-stats--version))))
     (list :error (format "format version %s; this ardentia reads version %d only.  \
Update ardentia or check the producer"
                          (or (alist-get 'version data) "missing")
                          ardentia-stats--version)))
    ((and data (guard (not (listp (alist-get 'sections data)))))
     (list :error "bad format: \"sections\" is not a list"))
    (data (list :data data))))

(defun ardentia-stats--read (file)
  "Read and parse the stats FILE.
Return (:data ALIST) or (:error MESSAGE); never signal."
  (cond ((not (file-exists-p file)) (list :error "file not found"))
        (t (pcase (condition-case err
                      (with-temp-buffer
                        (let ((coding-system-for-read 'utf-8))
                          (insert-file-contents file))
                        (buffer-string))
                    (error (list :error (format "cannot read: %s"
                                                (error-message-string err)))))
             ((and `(:error ,_) result) result)
             (text (ardentia-stats--parse text))))))

(defun ardentia-stats--age (time now)
  "Return how long before NOW TIME was, as \"12 min ago\"."
  (let ((s (float-time (time-subtract now time))))
    (cond ((< s -60) "in the future")
          ((< s 60) "just now")
          ((< s 3600) (format "%d min ago" (floor s 60)))
          ((< s 86400) (format "%d h ago" (floor s 3600)))
          (t (format "%d d ago" (floor s 86400))))))

(defun ardentia-stats--generated-text (generated now)
  "Return the text for the GENERATED stamp of the file at NOW.
A parsable ISO 8601 stamp gives local \"YYYY-MM-DD HH:MM (12 min ago)\",
anything else the raw value, nil \"unknown\"."
  (let ((time (and (stringp generated)
                   (ignore-errors (parse-iso8601-time-string generated)))))
    (cond (time (format "%s (%s)" (format-time-string "%Y-%m-%d %H:%M" time)
                        (ardentia-stats--age time now)))
          (generated (format "%s" generated))
          (t "unknown"))))

(defun ardentia-stats--section-key (section index)
  "Return the fold key of SECTION, the INDEXth: its key, or \"#INDEX\"."
  (let ((key (alist-get 'key section)))
    (if (and key (not (equal key ""))) (format "%s" key) (format "#%d" index))))

(defun ardentia-stats--ordered (sections order)
  "Return SECTIONS as (SECTION . INDEX) pairs, sorted by the key list ORDER.
INDEX is the position in SECTIONS.  Keys missing from ORDER follow in
their original order."
  (let* ((pairs (seq-map-indexed (lambda (section i) (cons section i)) sections))
         (rank (lambda (pair)
                 (or (seq-position order (ardentia-stats--section-key (car pair) (cdr pair)))
                     (length order)))))
    (sort pairs (lambda (a b) (< (funcall rank a) (funcall rank b))))))

(defun ardentia-stats--swap-key (keys key delta)
  "Return KEYS with KEY moved by DELTA places (-1 up, 1 down), or nil at the end."
  (let* ((i (seq-position keys key))
         (j (and i (+ i delta))))
    (when (and j (>= j 0) (< j (length keys)))
      (let ((v (vconcat keys)))
        (cl-rotatef (aref v i) (aref v j))
        (append v nil)))))

;;;; Buffer

(defvar-local ardentia-stats--result nil
  "The result of the last read: (:data ALIST) or (:error MESSAGE).")

(defvar-local ardentia-stats--stamp nil
  "The file and its modification time and size at the last render.")

(defvar-local ardentia-stats--folded nil
  "Keys of the folded sections of this buffer, kept over refreshes.")

(defun ardentia-stats--file ()
  "Return the expanded `ardentia-stats-file'."
  (expand-file-name ardentia-stats-file))

(defun ardentia-stats--file-stamp (file)
  "Return FILE with its modification time and size, or FILE alone."
  (let ((attrs (file-attributes file)))
    (cons file (and attrs (list (file-attribute-modification-time attrs)
                                (file-attribute-size attrs))))))

(defun ardentia-stats--insert-error (file message)
  "Insert the text that says FILE has no usable data because of MESSAGE."
  (insert (propertize "No agent stats to show." 'face 'ardentia-stats-heading)
          "\n\n"
          (format "File: %s\n" (abbreviate-file-name file))
          (format "Problem: %s\n\n" message)
          "The file is produced by an external tool; see the README of\n"
          "ardentia, section \"Agent stats\", for the format.  Set\n"
          "`ardentia-stats-file' to read another file.  g reads it again.\n"))

(defun ardentia-stats--insert-section (section index)
  "Insert the SECTION, the INDEXth, folded or not."
  (let* ((key (ardentia-stats--section-key section index))
         (folded (member key ardentia-stats--folded))
         (start (point))
         (title (format "%s" (or (alist-get 'title section) key)))
         (note (alist-get 'note section))
         (rows (alist-get 'rows section)))
    (insert (propertize (format "%s %s" (if folded "▸" "▾") title)
                        'face 'ardentia-stats-heading
                        'ardentia-stats-heading t)
            (if folded (format " (%d rows)" (length rows)) "")
            "\n")
    (unless folded
      (when note
        (insert "  " (propertize (format "%s" note) 'face 'ardentia-stats-note) "\n"))
      (if (null rows)
          (insert "  " (propertize "(no rows)" 'face 'ardentia-stats-note) "\n")
        (dolist (line (ardentia-stats--table (alist-get 'columns section) rows))
          (insert (propertize (concat "  " (car line)) 'ardentia-stats-id (cdr line))
                  "\n"))))
    ;; The blank line after an open section belongs to it, so folded
    ;; sections stand directly under each other.
    (unless folded (insert "\n"))
    (put-text-property start (point) 'ardentia-stats-section key)))

(defun ardentia-stats--insert (result file)
  "Insert the view of RESULT, read from FILE, into the current buffer."
  (if-let* ((message (plist-get result :error)))
      (ardentia-stats--insert-error file message)
    (let* ((data (plist-get result :data))
           (note (alist-get 'note data))
           (sections (alist-get 'sections data)))
      (when note
        (insert (propertize (format "%s" note) 'face 'ardentia-stats-note) "\n\n"))
      (if (null sections)
          (insert "The file has no sections.\n")
        (dolist (pair (ardentia-stats--ordered sections ardentia-stats-section-order))
          (ardentia-stats--insert-section (car pair) (cdr pair)))))))

(defun ardentia-stats--position (pos)
  "Return where POS is: (SECTION-KEY LINE-OFFSET COLUMN), or (nil LINE COLUMN)."
  (save-excursion
    (goto-char pos)
    (let ((key (get-text-property (line-beginning-position) 'ardentia-stats-section))
          (column (current-column)))
      (if key
          (let ((line (line-number-at-pos)))
            (ardentia-stats--goto-section key)
            (list key (- line (line-number-at-pos)) column))
        (list nil (line-number-at-pos) column)))))

(defun ardentia-stats--goto-section (key)
  "Move point to the heading of the section KEY; return point or nil."
  (let ((pos (point-min)) found)
    (while (and (not found) pos)
      (if (and (get-text-property pos 'ardentia-stats-heading)
               (equal (get-text-property pos 'ardentia-stats-section) key))
          (setq found pos)
        (setq pos (next-single-property-change pos 'ardentia-stats-heading))))
    (when found (goto-char found))))

(defun ardentia-stats--restore (position)
  "Return the buffer position for POSITION of `ardentia-stats--position'."
  (save-excursion
    (pcase-let ((`(,key ,line ,column) position))
      (goto-char (point-min))
      (if (and key (ardentia-stats--goto-section key))
          (let ((heading (point)))
            (forward-line line)
            (unless (equal (get-text-property (point) 'ardentia-stats-section) key)
              (goto-char heading)))
        (forward-line (1- line)))
      (move-to-column column)
      (point))))

(defun ardentia-stats--render ()
  "Read `ardentia-stats-file' and draw the buffer again.
Point, and the point of every window on the buffer, stay in their
section."
  (let* ((file (ardentia-stats--file))
         (windows (get-buffer-window-list (current-buffer) nil t))
         (here (ardentia-stats--position (point)))
         (there (mapcar (lambda (w) (cons w (ardentia-stats--position (window-point w))))
                        windows))
         (inhibit-read-only t))
    (setq ardentia-stats--stamp (ardentia-stats--file-stamp file)
          ardentia-stats--result (ardentia-stats--read file))
    (erase-buffer)
    (ardentia-stats--insert ardentia-stats--result file)
    (goto-char (ardentia-stats--restore here))
    (dolist (w there)
      (set-window-point (car w) (ardentia-stats--restore (cdr w))))))

(defun ardentia-stats--view-value (key data)
  "Return the positive integer under KEY in the `view' object of DATA, or nil."
  (let ((v (alist-get key (alist-get 'view data))))
    (and (integerp v) (> v 0) v)))

(defun ardentia-stats--header-line ()
  "Return the header line: window, generation time, its age and a key hint."
  (let* ((data (plist-get ardentia-stats--result :data))
         (days (ardentia-stats--view-value 'days data))
         (weeks (ardentia-stats--view-value 'weeks data)))
    (concat " Agent stats | "
            (if (and days weeks) (format "%d days, %d weeks | " days weeks) "")
            (if data
                (concat "generated "
                        (ardentia-stats--generated-text (alist-get 'generated data)
                                                        (current-time)))
              (propertize "no data" 'face 'error))
            (propertize " | TAB fold, RET visit, d days, w weeks, b chart, g refresh, ? keys"
                        'face 'ardentia-stats-note))))

;;;; Automatic refresh

(defvar ardentia-stats--timer nil
  "The one global timer that re-renders stats buffers whose file changed.")

;; The `:set' of `ardentia-stats-check-interval' runs at once when the
;; option is already bound, i.e. when the file is loaded again: everything
;; it calls must be defined above it.
(defvar ardentia-stats-check-interval)

(defun ardentia-stats--buffers ()
  "Return the live agent stats buffers."
  (seq-filter (lambda (b) (with-current-buffer b
                            (derived-mode-p 'ardentia-stats-mode)))
              (buffer-list)))

(defun ardentia-stats--tick ()
  "Re-render the stats buffers whose file changed; stop when there are none."
  (condition-case err
      (let ((buffers (ardentia-stats--buffers)))
        (if (null buffers)
            (ardentia-stats--timer-stop)
          (dolist (b buffers)
            (with-current-buffer b
              (unless (equal ardentia-stats--stamp
                             (ardentia-stats--file-stamp (ardentia-stats--file)))
                (ardentia-stats--render))))))
    (error (message "ardentia: stats refresh: %s" (error-message-string err)))))

(defun ardentia-stats--timer-stop ()
  "Cancel the check timer."
  (when (timerp ardentia-stats--timer)
    (cancel-timer ardentia-stats--timer))
  (setq ardentia-stats--timer nil))

(defun ardentia-stats--timer-start ()
  "Start the check timer unless disabled or already running."
  (when (and (numberp ardentia-stats-check-interval)
             (> ardentia-stats-check-interval 0)
             (not (timerp ardentia-stats--timer)))
    (setq ardentia-stats--timer
          (run-at-time ardentia-stats-check-interval ardentia-stats-check-interval
                       #'ardentia-stats--tick))))

(defcustom ardentia-stats-check-interval 5
  "Seconds between checks whether `ardentia-stats-file' changed on disk.
A check only compares the modification time and size of the file; the
buffer is drawn again when they changed.  Nil or 0 disables it.  The
timer starts when the view is opened and stops once its buffer is gone."
  :type '(choice (number :tag "Seconds")
                 (const :tag "Disabled" nil))
  :set (lambda (sym val)
         (set-default sym val)
         (when (timerp ardentia-stats--timer)
           (ardentia-stats--timer-stop)
           (when (ardentia-stats--buffers)
             (ardentia-stats--timer-start))))
  :group 'ardentia)

;;;; Commands

(defun ardentia-stats-refresh ()
  "Read the stats file again and redraw."
  (interactive)
  (ardentia-stats--render))

(defun ardentia-stats--section-at-point ()
  "Return the key of the section at point, or nil."
  (get-text-property (line-beginning-position) 'ardentia-stats-section))

(defun ardentia-stats-fold-toggle ()
  "Fold or unfold the section at point; point goes to its heading."
  (interactive)
  (let ((key (or (ardentia-stats--section-at-point)
                 (user-error "No section here"))))
    (setq ardentia-stats--folded
          (if (member key ardentia-stats--folded)
              (delete key ardentia-stats--folded)
            (cons key ardentia-stats--folded)))
    (ardentia-stats--goto-section key)
    (ardentia-stats--render)))

(defun ardentia-stats--keys ()
  "Return the keys of all sections in the buffer, in order."
  (let ((pos (point-min)) keys)
    (while pos
      (when (get-text-property pos 'ardentia-stats-heading)
        (push (get-text-property pos 'ardentia-stats-section) keys))
      (setq pos (next-single-property-change pos 'ardentia-stats-heading)))
    (nreverse keys)))

(defun ardentia-stats-fold-all ()
  "Fold all sections when any is open, else unfold all."
  (interactive)
  (let ((keys (or (ardentia-stats--keys) (user-error "No sections here")))
        (key (ardentia-stats--section-at-point)))
    (setq ardentia-stats--folded
          (if (seq-some (lambda (k) (not (member k ardentia-stats--folded))) keys)
              (seq-union keys ardentia-stats--folded)
            (seq-difference ardentia-stats--folded keys)))
    (when key (ardentia-stats--goto-section key))
    (ardentia-stats--render)))

(defun ardentia-stats--move-section (delta)
  "Move the section at point by DELTA places and save the order."
  (let* ((key (or (ardentia-stats--section-at-point) (user-error "No section here")))
         (keys (ardentia-stats--keys))
         (new (ardentia-stats--swap-key keys key delta)))
    (unless new (user-error "Section is already at the %s" (if (< delta 0) "top" "bottom")))
    (customize-save-variable 'ardentia-stats-section-order new)
    (ardentia-stats--goto-section key)
    (ardentia-stats--render)
    (ardentia-stats--goto-section key)))

(defun ardentia-stats-move-section-up ()
  "Move the section at point up one place; the order is saved."
  (interactive)
  (ardentia-stats--move-section -1))

(defun ardentia-stats-move-section-down ()
  "Move the section at point down one place; the order is saved."
  (interactive)
  (ardentia-stats--move-section 1))

(defun ardentia-stats--heading-positions ()
  "Return the positions of the section headings, in order."
  (let ((pos (point-min)) result)
    (while pos
      (when (get-text-property pos 'ardentia-stats-heading)
        (push pos result))
      (setq pos (next-single-property-change pos 'ardentia-stats-heading)))
    (nreverse result)))

(defun ardentia-stats-next-section ()
  "Move to the heading of the next section."
  (interactive)
  (let ((bol (line-beginning-position)))
    (if-let* ((pos (seq-find (lambda (p) (> p bol)) (ardentia-stats--heading-positions))))
        (goto-char pos)
      (message "No next section"))))

(defun ardentia-stats-previous-section ()
  "Move to the heading of the previous section."
  (interactive)
  (let ((bol (line-beginning-position)))
    (if-let* ((pos (seq-find (lambda (p) (< p bol))
                             (reverse (ardentia-stats--heading-positions)))))
        (goto-char pos)
      (message "No previous section"))))

(defun ardentia-stats--goto-id (id)
  "Visit the org heading with ID in another window."
  (require 'org-id)
  (let ((display-buffer-overriding-action
         '((display-buffer-use-some-window display-buffer-pop-up-window)
           (inhibit-same-window . t))))
    (condition-case err
        (org-id-goto id)
      (error (user-error "Cannot visit ID %s: %s" id (error-message-string err))))))

(defun ardentia-stats-visit ()
  "Visit the task of the row at point in another window.
On a section heading fold or unfold the section.  Rows without an ID
only say so."
  (interactive)
  (let ((bol (line-beginning-position)))
    (cond ((get-text-property bol 'ardentia-stats-heading)
           (ardentia-stats-fold-toggle))
          ((get-text-property bol 'ardentia-stats-id)
           (ardentia-stats--goto-id (get-text-property bol 'ardentia-stats-id)))
          (t (message "No task on this line")))))

(defun ardentia-stats-browse ()
  "Open the chart page named in the stats file with `browse-url-of-file'."
  (interactive)
  (let ((html (alist-get 'html (plist-get ardentia-stats--result :data))))
    (cond ((not (plist-get ardentia-stats--result :data))
           (message "No stats data, so no chart page"))
          ((not (and (stringp html) (not (string-empty-p html))))
           (message "The stats file names no chart page"))
          ((not (file-exists-p html))
           (message "Chart page not found: %s" html))
          (t (browse-url-of-file html)))))

;;;; Window

(defconst ardentia-stats--default-days 30
  "Days assumed for the window when the file does not say.")

(defconst ardentia-stats--default-weeks 12
  "Weeks assumed for the window when the file does not say.")

(defun ardentia-stats--render-argv (days weeks)
  "Return the command line that renders DAYS and WEEKS, or nil if none is set."
  (when-let* ((command ardentia-stats-render-command)
              (words (if (stringp command) (split-string-and-unquote command) command))
              (program (car words)))
    (append (list (if (or (file-name-directory program) (string-prefix-p "~" program))
                      (expand-file-name program)
                    (or (executable-find program) program)))
            (cdr words)
            (list "--days" (number-to-string days) "--weeks" (number-to-string weeks)))))

(defun ardentia-stats--sentinel (days weeks)
  "Return a process sentinel that reports the outcome of rendering DAYS, WEEKS."
  (lambda (process _event)
    (when (memq (process-status process) '(exit signal))
      (let* ((buffer (process-buffer process))
             (status (process-exit-status process))
             (output (and (buffer-live-p buffer)
                          (with-current-buffer buffer (string-trim (buffer-string))))))
        (if (and (eq (process-status process) 'exit) (zerop status))
            (message "Agent stats rendered for %d days, %d weeks" days weeks)
          (message "ardentia: stats render failed (%s %s)%s"
                   (process-status process) status
                   (if (and output (not (string-empty-p output)))
                       (concat ": " (car (last (split-string output "\n"))))
                     "")))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(defun ardentia-stats--apply-window (days weeks)
  "Run `ardentia-stats-render-command' for DAYS and WEEKS, without waiting.
The buffer follows by itself when the command rewrites the file."
  (let ((argv (or (ardentia-stats--render-argv days weeks)
                  (user-error "Set `ardentia-stats-render-command' to change the window"))))
    (condition-case err
        (make-process :name "ardentia-stats-render"
                      :buffer (generate-new-buffer " *ardentia-stats-render*")
                      :command argv :noquery t :connection-type 'pipe
                      :sentinel (ardentia-stats--sentinel days weeks))
      (error (user-error "Cannot run the stats render command: %s"
                         (error-message-string err))))
    (message "Rendering agent stats for %d days, %d weeks..." days weeks)))

(defun ardentia-stats--read-count (prompt presets current)
  "Read a positive integer with PROMPT, offering PRESETS; CURRENT is the default."
  (let ((text (completing-read prompt (mapcar #'number-to-string presets)
                               nil nil nil nil (number-to-string current))))
    (if (string-match-p "\\`[0-9]+\\'" text)
        (let ((n (string-to-number text)))
          (if (> n 0) n (user-error "Not a positive whole number: %s" text)))
      (user-error "Not a positive whole number: %s" text))))

(defun ardentia-stats--current (key default)
  "Return the window value KEY of the loaded data, or DEFAULT."
  (or (ardentia-stats--view-value key (plist-get ardentia-stats--result :data))
      default))

(defun ardentia-stats-set-days ()
  "Ask for the number of days the dashboards cover and render them again."
  (interactive)
  (unless ardentia-stats-render-command
    (user-error "Set `ardentia-stats-render-command' to change the window"))
  (let ((weeks (ardentia-stats--current 'weeks ardentia-stats--default-weeks))
        (days (ardentia-stats--read-count
               "Days: " '(7 14 30 90)
               (ardentia-stats--current 'days ardentia-stats--default-days))))
    (ardentia-stats--apply-window days weeks)))

(defun ardentia-stats-set-weeks ()
  "Ask for the number of weeks the dashboards cover and render them again."
  (interactive)
  (unless ardentia-stats-render-command
    (user-error "Set `ardentia-stats-render-command' to change the window"))
  (let ((days (ardentia-stats--current 'days ardentia-stats--default-days))
        (weeks (ardentia-stats--read-count
                "Weeks: " '(4 12 26 52)
                (ardentia-stats--current 'weeks ardentia-stats--default-weeks))))
    (ardentia-stats--apply-window days weeks)))

;;;; Keys and mode

(ardentia-define-keys-and-menu
    ardentia-stats-mode-map ardentia-stats-menu
  :keymap-doc "Keymap for `ardentia-stats-mode'."
  :menu-doc "Keys of the ardentia agent stats view."
  :groups
  (("Row at point"
    ("RET" "Visit task (fold on a heading)" ardentia-stats-visit))
   ("Sections"
    ("TAB" "Fold/unfold section" ardentia-stats-fold-toggle)
    ("<backtab>" "Fold/unfold all sections" ardentia-stats-fold-all)
    ("n" "Next section" ardentia-stats-next-section)
    ("p" "Previous section" ardentia-stats-previous-section)
    ("M-<up>" "Move section up" ardentia-stats-move-section-up)
    ("M-<down>" "Move section down" ardentia-stats-move-section-down))
   ("View"
    ("g" "Refresh" ardentia-stats-refresh)
    ("d" "Days covered" ardentia-stats-set-days)
    ("w" "Weeks covered" ardentia-stats-set-weeks)
    ("b" "Chart page in browser" ardentia-stats-browse)
    ("q" "Quit" quit-window))))

(define-derived-mode ardentia-stats-mode special-mode "Ardentia-Stats"
  "Major mode showing the agent stats dashboards of `ardentia-stats-file'.
Read-only; it re-renders when the file changes on disk.

\\{ardentia-stats-mode-map}"
  (setq truncate-lines t)
  (setq header-line-format '(:eval (ardentia-stats--header-line)))
  (ardentia-stats--timer-start))

;;;###autoload
(defun ardentia-stats ()
  "Show the agent stats dashboards in `*ardentia-stats*'.
The data come from `ardentia-stats-file', which an external tool writes."
  (interactive)
  (let ((buf (get-buffer-create "*ardentia-stats*")))
    (with-current-buffer buf
      (unless (derived-mode-p 'ardentia-stats-mode)
        (ardentia-stats-mode))
      (ardentia-stats--render))
    ;; The mode only starts the timer for a new buffer; an existing one, or
    ;; a buffer that predates loading this code, gets it here.
    (ardentia-stats--timer-start)
    (pop-to-buffer-same-window buf)))

(provide 'ardentia-stats)
;;; ardentia-stats.el ends here

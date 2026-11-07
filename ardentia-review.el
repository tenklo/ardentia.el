;;; ardentia-review.el --- Review buffer for ardentia -*- lexical-binding: t -*-

;; Copyright (C) 2026 Karsten Klöss

;; Author: Karsten Klöss
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

;; `ardentia-review-task' shows one :humanreview: task in a read-only org
;; buffer.  The first thing under its heading is the decision card (see
;; below); everything else comes after it, folded (TAB unfolds as usual):
;; file and status (TODO state, priority, tags), properties, the newest
;; run report from the LOGBOOK, the calls made "in your name" (the
;; `- Decided in your name:' lines of that report), the notes the human
;; wrote after it, the child headings (result sections with their content,
;; and open TODOs "for you") and the original task text.  The buffer's `default-directory'
;; is the task file's directory, so relative file links in sections
;; work.  Everything is read from the visiting buffer, not from disk.
;;
;; `z' or `C-c C-z' (`ardentia-review-add-note') opens org's note window
;; for the reviewed task.  `C-c C-c' stores the note in the LOGBOOK of
;; the real heading, saves the file when it was unmodified, and renders
;; the buffer again, so the note shows under "Your notes after the
;; report".  `C-c C-k' cancels.
;;
;; The card: for each open child TODO titled "Decide: ..." its question,
;; the options as `- A: text -- trade-off', the recommendation and what
;; the task does until the human answers (parsed from the child's body,
;; see `ardentia--card-parse'); a card that lacks a part shows what there
;; is plus a line saying so.  A task without such a child shows the
;; `- For you:' text of its newest run report with its sub-items, or
;; "nothing to do -- accept?".
;;
;; `d' (`ardentia-review-decide') picks an option of a card, with an
;; optional note: the note "Decision on TITLE: L -- OPTION[; NOTE]" goes to
;; the task, the Decide child is set DONE and the task goes back to the
;; agents (see `ardentia-answer.el', which also does the batch answers of
;; the task list).
;;
;; The buffer is a view, not a copy of the task: its heading carries no
;; TODO keyword and no tags, and the refile, copy, cut and archive
;; commands of org refuse to work in it (a refiled review buffer once
;; created duplicate tasks).  `o' visits the real heading.
;;
;; Tags, schedule, deadline, TODO state and priority of the task are
;; edited with the keys of org-agenda (`:', `C-c C-s', `C-c C-d',
;; `C-c C-t', `,' `+' `-'), on the real heading; the buffer is rendered
;; again afterwards.

;;; Code:

(require 'org)
(require 'org-indent)
(require 'seq)
(require 'subr-x)
(require 'ardentia-core)
(require 'ardentia-actions)
(require 'ardentia-session)
(require 'ardentia-answer)

(declare-function ardentia-dashboard-refresh-all "ardentia-dashboard" ())
(declare-function ardentia-dashboard--live-buffers "ardentia-dashboard" ())

;;;; Pure parts: LOGBOOK extraction

(defun ardentia--logbook-content (entry-text)
  "Return the content of the first LOGBOOK drawer in ENTRY-TEXT, or nil."
  (let ((case-fold-search t))
    (when (string-match "^[ \t]*:LOGBOOK:[ \t]*\n" entry-text)
      (let* ((start (match-end 0))
             (end (if (string-match "^[ \t]*:END:[ \t]*$" entry-text start)
                      (match-beginning 0)
                    (length entry-text))))
        (substring entry-text start end)))))

(defun ardentia--indentation (line)
  "Return the number of leading spaces of LINE."
  (if (string-match "\\`[ ]*" line) (match-end 0) 0))

(defun ardentia--logbook-items (content)
  "Split the LOGBOOK CONTENT into top-level items.
Return a list of items; each item is a list of lines.  The top-level
indentation is that of the first non-blank line."
  (let* ((lines (split-string content "\n"))
         (first (seq-find (lambda (l) (not (string-blank-p l))) lines))
         (ind (if first (ardentia--indentation first) 0))
         (prefix (concat (make-string ind ?\s) "- "))
         items current)
    (dolist (line lines)
      (cond ((string-prefix-p prefix line)
             (when current (push (nreverse current) items))
             (setq current (list line)))
            (current (push line current))))
    (when current (push (nreverse current) items))
    (nreverse items)))

(defun ardentia--dedent-item (item)
  "Return the lines of ITEM without the indentation of its first line.
Trailing blank lines are dropped."
  (let* ((ind (ardentia--indentation (car item)))
         (lines (mapcar (lambda (l)
                          (if (>= (ardentia--indentation l) ind)
                              (substring l ind)
                            (string-trim-left l)))
                        item)))
    (while (and lines (string-blank-p (car (last lines))))
      (setq lines (butlast lines)))
    lines))

(defun ardentia--extract-run-report (content)
  "Extract the newest agent run report and later notes from LOGBOOK CONTENT.
Return a plist with :report, a list of dedented lines or nil, and
:notes, a list of note items, each a list of dedented lines, newest first.
Notes are the \"Note taken on\" items above the newest report."
  (let (report notes (items (ardentia--logbook-items (or content ""))))
    (while (and items (not report))
      (let ((item (pop items)))
        (cond ((string-match-p "\\`[ ]*- AI-Lauf \\[" (car item))
               (setq report (ardentia--dedent-item item)))
              ((string-match-p "\\`[ ]*- Note taken on " (car item))
               (push (ardentia--dedent-item item) notes)))))
    (list :report report :notes (nreverse notes))))

;;;; Pure parts: the "For you" line

(defun ardentia--for-you (report-lines)
  "Return the \"For you\" line of the run report REPORT-LINES, or nil.
REPORT-LINES are the dedented lines of the newest report (the :report of
`ardentia--extract-run-report').  Only the first sub-item directly after
the report's first line counts: older reports predate the line and are
never consulted.  Return a plist (:class :text :lines).  :class is
`nothing' (text starts with \"nothing to do\"), else `required' (text
contains \"REQUIRED\"), else `optional' (text contains \"optional\").
:text is the text after \"For you:\"; :lines are the texts of the
deeper-indented sub-items below it.  Return nil without a line or for
an unrecognised one."
  (let ((line (cadr report-lines)))
    (when (and line (string-match "\\`\\( *\\)- For you: *\\(.*?\\) *\\'" line))
      (let* ((base (length (match-string 1 line)))
             (text (match-string 2 line))
             (class (cond ((string-prefix-p "nothing to do" text) 'nothing)
                          ((string-match-p "REQUIRED" text) 'required)
                          ((string-match-p "optional" text) 'optional)))
             subs)
        (when class
          (catch 'done
            (dolist (l (cddr report-lines))
              (if (and (> (ardentia--indentation l) base)
                       (string-match "\\` *- +\\(.*?\\) *\\'" l))
                  (push (match-string 1 l) subs)
                (throw 'done nil))))
          (list :class class :text text :lines (nreverse subs)))))))

;;;; Pure parts: the decision card of a Decide child

(defun ardentia--reduce-links (text)
  "Return TEXT with org links reduced to their description (or target)."
  (replace-regexp-in-string
   "\\[\\[\\([^]]*\\)\\]\\]" "\\1"
   (replace-regexp-in-string "\\[\\[[^]]*\\]\\[\\([^]]*\\)\\]\\]" "\\1" text)))

(defun ardentia--card-body-lines (text)
  "Return the lines of the body TEXT without drawers and planning lines.
A trailing \" \\\\\" (the line break of LOGBOOK items) is stripped."
  (let (out in-drawer (head t))
    (dolist (line (split-string (or text "") "\n"))
      (cond
       (in-drawer (when (string-match-p "\\`[ \t]*:END:[ \t]*\\'" line)
                    (setq in-drawer nil)))
       ((string-match-p "\\`[ \t]*:\\(PROPERTIES\\|LOGBOOK\\):[ \t]*\\'" line)
        (setq in-drawer t))
       ((and head (string-match-p
                   "\\`[ \t]*\\(SCHEDULED\\|DEADLINE\\|CLOSED\\):" line)))
       (t (unless (string-blank-p line) (setq head nil))
          (push (replace-regexp-in-string " *\\\\\\\\ *\\'" "" line) out))))
    (nreverse out)))

(defun ardentia--card-parse (text)
  "Parse the body TEXT of a Decide child into a plist.
The body has the top-level items `- Question: TEXT', `- Options' with the
sub-items `- A: TEXT -- TRADE-OFF', `- Recommendation: L -- REASON' and
`- Until you answer: TEXT'; other items are background and ignored.  The
plist has :question, :options (a list of (:letter :text :tradeoff), the
trade-off \"\" without a ` -- '), :recommendation (a plist :letter
:reason, or nil without the item; a text that does not start with a
single capital letter has letter \"\"), :until and :complete.  :complete
is non-nil when the question and the until text are not empty, there are
at least two options and the recommended letter is one of them.  Org
links are reduced to their description."
  (let* ((lines (ardentia--card-body-lines text))
         (first (seq-find (lambda (l) (string-match-p "\\` *- " l)) lines))
         (ind (if first (ardentia--indentation first) 0))
         (until "") question options rec section)
    (dolist (line lines)
      (let ((li (ardentia--indentation line)))
        (cond
         ((and (= li ind) (string-match "\\` *- +\\(.*?\\) *\\'" line))
          (let ((item (match-string 1 line)))
            (setq section nil)
            (cond
             ((string-match "\\`Question: *\\(.*\\)\\'" item)
              (unless question
                (setq question (ardentia--reduce-links (match-string 1 item)))))
             ((string-match-p "\\`Options\\(?::.*\\)?\\'" item)
              (setq section 'options))
             ((string-match "\\`Recommendation: *\\(.*\\)\\'" item)
              (unless rec
                (let ((r (ardentia--reduce-links (match-string 1 item))))
                  (setq rec (if (string-match
                                 "\\`\\([A-Z]\\)\\(?: -- \\(.*\\)\\)?\\'" r)
                                (list :letter (match-string 1 r)
                                      :reason (or (match-string 2 r) ""))
                              (list :letter "" :reason r))))))
             ((string-match "\\`Until you answer: *\\(.*\\)\\'" item)
              (when (string-empty-p until)
                (setq until (ardentia--reduce-links (match-string 1 item))))))))
         ((and (eq section 'options) (> li ind)
               (string-match "\\` *- \\([A-Z]\\): \\(.*?\\) *\\'" line))
          (let* ((letter (match-string 1 line))
                 (rest (ardentia--reduce-links (match-string 2 line)))
                 (pos (string-search " -- " rest)))
            (push (list :letter letter
                        :text (string-trim (if pos (substring rest 0 pos) rest))
                        :tradeoff (if pos (string-trim (substring rest (+ pos 4))) ""))
                  options))))))
    (setq options (nreverse options)
          question (or question ""))
    (list :question question
          :options options
          :recommendation rec
          :until until
          :complete (and (not (string-empty-p question))
                         (>= (length options) 2)
                         rec
                         (seq-some (lambda (o)
                                     (equal (plist-get o :letter)
                                            (plist-get rec :letter)))
                                   options)
                         (not (string-empty-p until))
                         t))))

(defun ardentia--decide-child-p (child)
  "Return non-nil when CHILD, a child plist, is an open Decide: TODO."
  (and (plist-get child :todo)
       (not (plist-get child :done))
       (string-prefix-p "Decide:" (or (plist-get child :title) ""))))

(defun ardentia--open-decides (children)
  "Return the open Decide children of CHILDREN that have a :card, in order."
  (seq-filter (lambda (c) (and (ardentia--decide-child-p c) (plist-get c :card)))
              children))

(defun ardentia--card-lines (card)
  "Return the lines (list items) that show the CARD plist."
  (let ((q (plist-get card :question))
        (rec (plist-get card :recommendation))
        (until (plist-get card :until))
        out)
    (unless (string-empty-p q) (push (concat "- Question: " q) out))
    (dolist (o (plist-get card :options))
      (push (format "- %s: %s%s" (plist-get o :letter) (plist-get o :text)
                    (if (string-empty-p (plist-get o :tradeoff)) ""
                      (concat " -- " (plist-get o :tradeoff))))
            out))
    (when rec
      (push (concat "- Recommendation: "
                    (cond ((string-empty-p (plist-get rec :letter))
                           (plist-get rec :reason))
                          ((string-empty-p (plist-get rec :reason))
                           (plist-get rec :letter))
                          (t (format "%s -- %s" (plist-get rec :letter)
                                     (plist-get rec :reason)))))
            out))
    (unless (string-empty-p until) (push (concat "- Until you answer: " until) out))
    (unless (plist-get card :complete)
      (push "- (card incomplete -- see the Decide child)" out))
    (nreverse out)))

(defun ardentia--decided-in-name (report-lines)
  "Return the \"Decided in your name\" texts of REPORT-LINES, newest report.
Every sub-item `- Decided in your name: TEXT' counts; links are reduced."
  (let (out)
    (dolist (l report-lines)
      (when (string-match "\\` *- +Decided in your name: *\\(.*?\\) *\\'" l)
        (push (ardentia--reduce-links (match-string 1 l)) out)))
    (nreverse out)))

;;;; Pure parts: children

(defun ardentia--classify-children (children)
  "Classify CHILDREN, a list of plists (:title :todo :done :tags :id).
Return a plist: :results, the children without TODO keyword, and
:for-you, the children with one, open ones first."
  (let ((results (seq-remove (lambda (c) (plist-get c :todo)) children))
        (todos (seq-filter (lambda (c) (plist-get c :todo)) children)))
    (list :results results
          :for-you (append (seq-remove (lambda (c) (plist-get c :done)) todos)
                           (seq-filter (lambda (c) (plist-get c :done)) todos)))))

;;;; Result section content

(defun ardentia--plain-heading-text (text keywords)
  "Return the heading TEXT (after the stars) without TODO keyword and tags.
KEYWORDS are the TODO keywords to drop; nil means TODO and DONE."
  (let ((text (replace-regexp-in-string
               "[ \t]+:[[:alnum:]_@#%:]+:[ \t]*\\'" "" text)))
    (if (string-match (concat "\\` \\(?:"
                              (regexp-opt (or keywords '("TODO" "DONE")))
                              "\\)\\(?: \\|\\'\\)")
                      text)
        (concat " " (substring text (match-end 0)))
      text)))

(defun ardentia--section-content (content child-level target-level
                                          &optional keywords)
  "Return the lines of CONTENT, a result section's subtree text, to render.
CHILD-LEVEL is the org level of the section heading, TARGET-LEVEL the
level it gets in the review buffer.  Property and LOGBOOK drawers and
planning lines directly under a heading are dropped, a heading of
level L becomes level TARGET-LEVEL + (L - CHILD-LEVEL) without its TODO
keyword (one of KEYWORDS, default TODO and DONE) and tags, and leading
and trailing blank lines are trimmed.  Return nil when nothing is left."
  (let (out in-drawer (after-heading t))
    (dolist (line (and content (split-string content "\n")))
      (cond
       (in-drawer (when (string-match-p "\\`[ \t]*:END:[ \t]*\\'" line)
                    (setq in-drawer nil)))
       ((string-match-p "\\`[ \t]*:\\(PROPERTIES\\|LOGBOOK\\):[ \t]*\\'" line)
        (setq in-drawer t))
       ((and after-heading
             (string-match-p
              "\\`[ \t]*\\(SCHEDULED\\|DEADLINE\\|CLOSED\\):" line)))
       ((string-match "\\`\\(\\*+\\)\\( .*\\)?\\'" line)
        (push (concat (make-string
                       (max 1 (+ target-level
                                 (- (length (match-string 1 line)) child-level)))
                       ?*)
                      (ardentia--plain-heading-text (or (match-string 2 line) "")
                                                    keywords))
              out)
        (setq after-heading t))
       (t (push line out)
          (setq after-heading nil))))
    (setq out (nreverse out))
    (while (and out (string-blank-p (car out))) (pop out))
    (setq out (nreverse out))
    (while (and out (string-blank-p (car out))) (pop out))
    (nreverse out)))

;;;; Collecting from the visiting buffer

(defun ardentia-review--subtree-content ()
  "Return the text of the subtree at point after its heading line."
  (save-excursion
    (let ((end (save-excursion (org-end-of-subtree t t) (point))))
      (forward-line 1)
      (if (< (point) end)
          (buffer-substring-no-properties (point) end)
        ""))))

(defun ardentia-review--own-body ()
  "Return the text of the heading at point up to the next heading."
  (save-excursion
    (beginning-of-line)
    (let ((end (save-excursion (outline-next-heading) (point))))
      (forward-line 1)
      (if (< (point) end) (buffer-substring-no-properties (point) end) ""))))

(defun ardentia-review--children ()
  "Return the direct children of the heading at point as plists."
  (let ((level (org-current-level)) result)
    (save-excursion
      (when (org-goto-first-child)
        (let ((more t))
          (while more
            (when (= (org-current-level) (1+ level))
              (let ((todo (org-get-todo-state)))
                (push (append
                       (list :title (org-get-heading t t t t)
                             :todo todo
                             :done (and todo (member todo org-done-keywords) t)
                             :tags (org-get-tags nil t)
                             :id (org-entry-get nil "ID"))
                       (when (and todo (not (member todo org-done-keywords))
                                  (string-prefix-p
                                   "Decide:" (org-get-heading t t t t)))
                         (list :card (ardentia--card-parse
                                      (ardentia-review--own-body))))
                       (unless todo
                         (list :level (org-current-level)
                               :content (ardentia-review--subtree-content))))
                      result)))
            (setq more (org-get-next-sibling))))))
    (nreverse result)))

(defun ardentia-review--collect (file id)
  "Collect the review data of task ID in FILE from its visiting buffer.
Return a plist; signal a `user-error' when the heading is missing."
  (let ((buf (ardentia--task-buffer file)))
    (with-current-buffer buf
      (org-with-wide-buffer
       (unless (ardentia--goto-id id)
         (user-error "Heading with ID %s not found in %s" id file))
       (let* ((title (org-get-heading t t t t))
              (tags (org-get-tags nil t))
              (props (seq-filter (lambda (c) (string-prefix-p "AI_" (car c)))
                                 (org-entry-properties nil 'standard)))
              (entry-end (save-excursion (outline-next-heading) (point)))
              (entry-text (buffer-substring-no-properties (point) entry-end))
              (log (ardentia--extract-run-report
                    (ardentia--logbook-content entry-text)))
              (body (save-excursion
                      (org-end-of-meta-data t)
                      (string-trim
                       (buffer-substring-no-properties
                        (min (point) entry-end) entry-end)))))
         (append (list :file file :id id :title title :tags tags :props props
                       :keywords org-todo-keywords-1
                       :todo (org-get-todo-state)
                       ;; Explicit cookie only; no default priority.
                       :priority (nth 3 (org-heading-components))
                       :body body)
                 log
                 (let ((kids (ardentia--classify-children
                              (ardentia-review--children))))
                   (append kids
                           (list :decides
                                 (ardentia--open-decides
                                  (plist-get kids :for-you)))))))))))

;;;; The "For you" class of review rows

(defvar ardentia-review--for-you-cache (make-hash-table :test #'equal)
  "Cache of `ardentia-review-for-you-class', keyed by (FILE ID AI_SESSION).")

(defun ardentia-review-for-you-class (task)
  "Return the class of the \"For you\" line of TASK's newest run report.
The result is `required', `optional', `nothing' or nil (no line).  The
report is read from the task's buffer once per run (the key is file, ID
and AI_SESSION).  Any error gives nil and is not cached."
  (let* ((file (plist-get task :file))
         (id (plist-get task :id))
         (key (list file id (ardentia--task-prop task "AI_SESSION")))
         (hit (gethash key ardentia-review--for-you-cache 'miss)))
    (if (not (eq hit 'miss))
        (car hit)
      (condition-case nil
          (when (and file id (file-exists-p file))
            (let* ((info (ardentia-review--collect file id))
                   (class (plist-get (ardentia--for-you (plist-get info :report))
                                     :class)))
              (puthash key (list class) ardentia-review--for-you-cache)
              class))
        (error nil)))))

;;;; Rendering

(defun ardentia-review--link (child)
  "Return an org link (or plain title) for the CHILD plist."
  (let ((title (replace-regexp-in-string
                "\\[" "(" (replace-regexp-in-string "\\]" ")" (plist-get child :title)))))
    (if (plist-get child :id)
        (format "[[id:%s][%s]]" (plist-get child :id) title)
      title)))

(defun ardentia-review--status-text (info)
  "Return the text of the Status line for the review data INFO."
  (let ((todo (plist-get info :todo))
        (prio (plist-get info :priority))
        (tags (plist-get info :tags))
        (agents (cdr (assoc "AI_STATUS" (plist-get info :props)))))
    (concat (or todo "no TODO keyword")
            (if prio (format ", priority %c" prio) "")
            (if tags (format ", tags: %s" (string-join tags " ")) "")
            (if agents (format ", agents: %s" agents) ""))))

(defun ardentia-review--card-section (info)
  "Return the lines of the card of the review data INFO.
That is one block per open Decide child, else the \"For you\" text of
the newest run report, else a hint."
  (let ((decides (plist-get info :decides)) (i 0) out)
    (if decides
        (dolist (c decides)
          (setq i (1+ i))
          (when (or (cdr decides) (not (plist-get (plist-get c :card) :complete)))
            (push (if (cdr decides)
                      (format "- Decision %d of %d: %s" i (length decides)
                              (ardentia-review--link c))
                    (format "- Decision: %s" (ardentia-review--link c)))
                  out))
          (dolist (l (ardentia--card-lines (plist-get c :card)))
            (push (if (cdr decides) (concat "  " l) l) out)))
      (let ((fy (ardentia--for-you (plist-get info :report))))
        (cond
         ((null fy)
          (push "- (no \"For you\" line in the run report -- see Latest run report)"
                out))
         ((eq (plist-get fy :class) 'nothing)
          (push "- nothing to do -- accept?" out))
         (t (push (format "- For you: %s" (plist-get fy :text)) out)
            (dolist (l (plist-get fy :lines))
              (push (concat "  - " l) out))))))
    (nreverse out)))

(defun ardentia-review--render-string (info)
  "Return the org text for the review data INFO.
The heading is the plain title (no TODO keyword, no tags: the buffer is
a view and must never look like a task); the card comes first, the
other sections after it."
  (with-temp-buffer
    (insert "* " (plist-get info :title) "\n")
    (dolist (l (ardentia-review--card-section info))
      (insert l "\n"))
    (insert "** File and status\n")
    (insert (format "- File :: [[file:%s][%s]]\n"
                    (abbreviate-file-name (plist-get info :file))
                    (file-name-nondirectory (plist-get info :file))))
    (insert (format "- Status :: %s\n" (ardentia-review--status-text info)))
    (when-let* ((fy (ardentia--for-you (plist-get info :report))))
      (insert (format "- For you :: %s\n" (plist-get fy :text)))
      (dolist (l (plist-get fy :lines))
        (insert "  - " l "\n")))
    (insert "** Agent properties\n")
    (when-let* ((usage (ardentia--usage-line (plist-get info :props))))
      (insert "- Usage :: " usage "\n"))
    (if (plist-get info :props)
        (dolist (p (plist-get info :props))
          (insert (format "- %s :: %s\n" (car p) (cdr p))))
      (insert "- none\n"))
    (insert "** Latest run report\n")
    (if (plist-get info :report)
        (insert (string-join (plist-get info :report) "\n") "\n")
      (insert "- no AI-Lauf report found\n"))
    (when-let* ((decided (ardentia--decided-in-name (plist-get info :report))))
      (insert "** Decided in your name\n")
      (dolist (d decided)
        (insert "- " d "\n")))
    (when (plist-get info :notes)
      (insert "** Your notes after the report\n")
      (dolist (note (plist-get info :notes))
        (insert (string-join note "\n") "\n")))
    (insert "** Result sections\n")
    (if (plist-get info :results)
        (dolist (c (plist-get info :results))
          (insert "*** " (ardentia-review--link c) "\n")
          (dolist (l (ardentia--section-content
                      (plist-get c :content) (or (plist-get c :level) 2) 3
                      (plist-get info :keywords)))
            (insert l "\n")))
      (insert "- none\n"))
    (insert "** For you\n")
    (if (plist-get info :for-you)
        (dolist (c (plist-get info :for-you))
          (insert (format "- %s %s%s\n"
                          (plist-get c :todo) (ardentia-review--link c)
                          (if (plist-get c :tags)
                              (format " :%s:" (string-join (plist-get c :tags) ":"))
                            ""))))
      (insert "- none\n"))
    (insert "** Task text\n")
    (let ((body (plist-get info :body)))
      (insert (if (string-blank-p body) "- empty" body) "\n"))
    (buffer-string)))

;;;; Mode

(defcustom ardentia-review-indent t
  "Whether the review buffer turns on `org-indent-mode'."
  :type 'boolean
  :group 'ardentia)

(defcustom ardentia-review-truncate-lines nil
  "Whether review buffers truncate long lines instead of wrapping them.
When nil (the default), long lines wrap at the window edge, at word
boundaries, regardless of `org-startup-truncated'.  When t, they are
truncated.  Applied each time `ardentia-review-task' shows a buffer.
Toggle in a buffer with \\[toggle-truncate-lines]."
  :type 'boolean
  :group 'ardentia)

(defvar-local ardentia-review--task nil
  "Plist (:file :id :title) of the task shown in this review buffer.")

(defvar-local ardentia-review--info nil
  "Review data last rendered in this buffer.")

(ardentia-define-keys-and-menu
    ardentia-review-mode-map ardentia-review-menu
  :keymap-doc "Keymap for `ardentia-review-mode'."
  :menu-doc "Keys of the ardentia review buffer.
Also bound: C-c C-q (tags), S-<up> and S-<down> (priority up and down)."
  :groups
  (("Review"
    ("a" "Accept" ardentia-review-accept)
    ("d" "Decide (pick an option of the card)" ardentia-review-decide)
    ("s" "Send back" ardentia-review-send-back)
    ("y" "Let agents do child TODO" ardentia-review-yourself))
   ("Task"
    ("o" "Visit heading" ardentia-review-visit)
    ("t" "Transcript" ardentia-review-transcript)
    ("C-c A" "Agent shell" ardentia-review-agent-shell)
    ("z" "Add note to task" ardentia-review-add-note))
   ("Edit"
    (":" "Tags" ardentia-review-set-tags)
    ("C-c C-s" "Schedule" ardentia-review-schedule)
    ("C-c C-d" "Deadline" ardentia-review-deadline)
    ("C-c C-t" "TODO state" ardentia-review-todo)
    ("," "Priority" ardentia-review-priority)
    ("+" "Priority up" ardentia-review-priority-up)
    ("-" "Priority down" ardentia-review-priority-down))
   ("View"
    ("g" "Re-render" ardentia-review-render)
    ("q" "Quit" quit-window)))
  :alternates
  (("C-c C-z" . ardentia-review-add-note)
   ("C-c C-q" . ardentia-review-set-tags)
   ("S-<up>" . ardentia-review-priority-up)
   ("S-<down>" . ardentia-review-priority-down)))

(defun ardentia-review-refuse-copy ()
  "Refuse to refile, copy, cut or archive in a review buffer.
The buffer is a view of a task, not the task."
  (interactive)
  (user-error "Review buffer is a view; use o to visit the real heading"))

(defconst ardentia-review--refused-commands
  '(org-refile org-refile-copy org-copy-subtree org-cut-subtree
    org-archive-subtree org-archive-subtree-default
    org-archive-subtree-default-with-confirmation
    org-archive-to-archive-sibling org-toggle-archive-tag)
  "Org commands that `ardentia-review-mode' remaps to a refusal.")

(defun ardentia-review--refuse-advice (&rest _)
  "Refuse the advised org command in a review buffer."
  (when (derived-mode-p 'ardentia-review-mode)
    (ardentia-review-refuse-copy)))

(dolist (cmd ardentia-review--refused-commands)
  ;; The remap covers the keys, the advice covers M-x and calls from code.
  (keymap-set ardentia-review-mode-map (format "<remap> <%s>" cmd)
              #'ardentia-review-refuse-copy)
  (advice-add cmd :before #'ardentia-review--refuse-advice))

(define-derived-mode ardentia-review-mode org-mode "Ardentia-Review"
  "Read-only org view of one task waiting for review.

\\{ardentia-review-mode-map}"
  (setq buffer-read-only t)
  (when ardentia-review-indent (org-indent-mode 1))
  (setq-local truncate-lines ardentia-review-truncate-lines)
  (setq-local word-wrap (not ardentia-review-truncate-lines))
  (setq ardentia--current-task-function (lambda () ardentia-review--task)))

(declare-function ardentia "ardentia" ())

(defun ardentia-review--fold ()
  "Fold every section below the card, so the first screen is the card."
  (let ((inhibit-read-only t))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^\\*\\* " nil t)
        (org-fold-subtree t)
        (goto-char (line-end-position))))))

(defun ardentia-review-render ()
  "Render the task of the current review buffer again."
  (interactive)
  (unless ardentia-review--task (user-error "Not an ardentia review buffer"))
  (let* ((info (ardentia-review--collect (plist-get ardentia-review--task :file)
                                         (plist-get ardentia-review--task :id)))
         (text (ardentia-review--render-string info))
         (inhibit-read-only t)
         (line (line-number-at-pos)))
    (setq ardentia-review--info info)
    (erase-buffer)
    (insert text)
    (ardentia-review--fold)
    (goto-char (point-min))
    (forward-line (1- line))
    (when (invisible-p (point))
      (outline-back-to-heading t))
    (set-buffer-modified-p nil)))

;;;###autoload
(defun ardentia-review-task (task)
  "Open the review buffer for TASK, a task plist, or the task at point."
  (interactive (list (ardentia--current-task)))
  (let* ((title (plist-get task :title))
         (buf (get-buffer-create (format "*ardentia: %s*" title))))
    (with-current-buffer buf
      (unless (derived-mode-p 'ardentia-review-mode) (ardentia-review-mode))
      (setq-local truncate-lines ardentia-review-truncate-lines)
      (setq-local word-wrap (not ardentia-review-truncate-lines))
      (when (plist-get task :file)
        (setq default-directory
              (file-name-directory (expand-file-name (plist-get task :file)))))
      (setq ardentia-review--task
            (list :file (plist-get task :file) :id (plist-get task :id)
                  :title title))
      (ardentia-review-render)
      (goto-char (point-min)))
    (pop-to-buffer buf)))

(defun ardentia-review-accept ()
  "Accept the task of this review buffer."
  (interactive)
  (when (ardentia-accept ardentia-review--task)
    (quit-window t)))

(defun ardentia-review-send-back ()
  "Send the task of this review buffer back to the agents."
  (interactive)
  (ardentia-send-back ardentia-review--task)
  (quit-window t))

(defun ardentia-review-visit ()
  "Visit the heading of this review buffer's task in its org file."
  (interactive)
  (ardentia-visit-task ardentia-review--task))

(defun ardentia-review-transcript ()
  "Open the session transcript of this review buffer's task."
  (interactive)
  (ardentia-session-open ardentia-review--task))

(defun ardentia-review-yourself ()
  "Let the agents do an open child TODO of this task themselves.
Pick among the open children tagged humanreview; replace the tag by aiready."
  (interactive)
  (let* ((open (seq-filter
                (lambda (c) (and (not (plist-get c :done))
                                 (member "humanreview" (plist-get c :tags))))
                (plist-get ardentia-review--info :for-you)))
         (titles (mapcar (lambda (c) (plist-get c :title)) open)))
    (unless titles (user-error "No open child TODO tagged humanreview"))
    (let* ((title (completing-read "You can do that yourself: " titles nil t))
           (note (read-string "Note (optional): ")))
      (ardentia-yourself-1 (plist-get ardentia-review--task :file)
                           (plist-get ardentia-review--task :id)
                           title note)
      (ardentia-review-render))))

(defun ardentia-review-agent-shell ()
  "Open the latest session of this review buffer's task in agent-shell."
  (interactive)
  (ardentia-session-open-agent-shell ardentia-review--task))

(defun ardentia-review--option-label (option rec)
  "Return the completion label of OPTION; REC is the recommended letter."
  (format "%s: %s%s%s" (plist-get option :letter) (plist-get option :text)
          (if (string-empty-p (plist-get option :tradeoff)) ""
            (concat " -- " (plist-get option :tradeoff)))
          (if (equal rec (plist-get option :letter)) "  (recommended)" "")))

(defun ardentia-review-decide ()
  "Pick an option of the card and hand the task back to the agents.
With several open Decide children ask which one first.  Ask for an
optional note, show what will happen and ask once.  The note \"Decision
on TITLE: L -- OPTION[; NOTE]\" goes to the task, the Decide child is set
DONE and humanreview is replaced by aiready (not while the task is
running)."
  (interactive)
  (let ((decides (plist-get ardentia-review--info :decides)))
    (unless decides (user-error "No open Decide child with a card"))
    (let* ((sub (if (cdr decides)
                    (let* ((titles (mapcar (lambda (c) (plist-get c :title))
                                           decides))
                           (pick (completing-read "Decide: " titles nil t nil nil
                                                  (car titles))))
                      (1+ (seq-position titles pick)))
                  1))
           (card (plist-get (nth (1- sub) decides) :card))
           (rec (plist-get (plist-get card :recommendation) :letter)))
      (unless (plist-get card :options)
        (user-error "Card incomplete -- see the Decide child"))
      (let* ((labels (mapcar (lambda (o) (ardentia-review--option-label o rec))
                             (plist-get card :options)))
             (pick (completing-read "Option: " labels nil t nil nil
                                    (seq-find (lambda (l) (string-prefix-p
                                                           (concat rec ":") l))
                                              labels)))
             (letter (substring pick 0 1))
             (note (string-trim (read-string "Note (optional): ")))
             (task (list :file (plist-get ardentia-review--task :file)
                         :id (plist-get ardentia-review--task :id)
                         :title (plist-get ardentia-review--task :title)))
             (steps (ardentia-answer-resolve
                     (list (list :n 1 :sub sub :kind 'decide :letter letter
                                 :text note))
                     (list (list 1 :task task :info ardentia-review--info))))
             (result (ardentia-answer-execute steps)))
        (when (and result (> (plist-get result :done) 0))
          (quit-window t))))))

;;;; Adding a note to the task

(defvar ardentia-review--pending-note nil
  "Plist of the note being taken for a review buffer's task, or nil.
Keys: :buffer (the task's org buffer), :id, :was-modified (whether that
buffer had unsaved changes before) and :review-buffer.")

(defun ardentia-review--clear-pending-note ()
  "Forget the pending note."
  (setq ardentia-review--pending-note nil))

(defun ardentia-review--finish-pending (pending)
  "Finish the edit PENDING describes, a plist like `ardentia-review--pending-note'.
Save the task buffer when it was unmodified before, render the review
buffer again and refresh the dashboards."
  (ardentia--finish-buffer (plist-get pending :buffer)
                           (plist-get pending :was-modified))
  (let ((review (plist-get pending :review-buffer)))
    (when (buffer-live-p review)
      (with-current-buffer review (ardentia-review-render))))
  (when (and (fboundp 'ardentia-dashboard--live-buffers)
             (ardentia-dashboard--live-buffers))
    (ardentia-dashboard-refresh-all)))

(defun ardentia-review--note-stored ()
  "Finish the note of `ardentia-review--pending-note' once org stored it.
Runs on `org-after-note-stored-hook' with point at the new note.  Save the
task buffer when it was unmodified before, render the review buffer again
and refresh the dashboards.  Do nothing for any other note.  The pending
record is also made by the edits of `ardentia-actions' that org logs
(TODO state, schedule, deadline)."
  (let ((pending ardentia-review--pending-note))
    (when (and pending (eq (plist-get pending :buffer) (current-buffer)))
      (condition-case err
          (when (equal (save-excursion
                         (org-back-to-heading t)
                         (org-entry-get nil "ID"))
                       (plist-get pending :id))
            (ardentia-review--clear-pending-note)
            (ardentia-review--finish-pending pending))
        (error (message "ardentia: finishing the note failed: %s"
                        (error-message-string err)))))))

(defun ardentia-review--abandon-pending-note ()
  "Forget the pending note; finish the edit it belonged to, if any.
A cancelled note of a TODO state, schedule or deadline change leaves the
change itself in the buffer; it is saved like a stored one."
  (let ((pending ardentia-review--pending-note))
    (ardentia-review--clear-pending-note)
    (when (and pending (plist-get pending :changed)
               (buffer-live-p (plist-get pending :buffer)))
      (condition-case err
          (ardentia-review--finish-pending pending)
        (error (message "ardentia: finishing the change failed: %s"
                        (error-message-string err)))))))

(defun ardentia-review--watch-note-buffer ()
  "Clear the pending note when the `*Org Note*' buffer goes away.
Runs on `org-log-buffer-setup-hook'.  Org kills the buffer before it
stores the note and runs `org-after-note-stored-hook', so the clearing is
deferred to a timer; a cancelled note then leaves no record behind."
  (when ardentia-review--pending-note
    (add-hook 'kill-buffer-hook
              (lambda ()
                (run-at-time 0 nil #'ardentia-review--abandon-pending-note))
              nil t)))

(add-hook 'org-after-note-stored-hook #'ardentia-review--note-stored)
(add-hook 'org-log-buffer-setup-hook #'ardentia-review--watch-note-buffer)

(defun ardentia-review-add-note ()
  "Add a note to the LOGBOOK of the task of this review buffer.
Open org's note window for the real heading: `C-c C-c' stores the note,
`C-c C-k' cancels.  The org file is saved afterwards when it was
unmodified before."
  (interactive)
  (unless (and ardentia-review--task (derived-mode-p 'ardentia-review-mode))
    (user-error "Not an ardentia review buffer"))
  (let* ((id (plist-get ardentia-review--task :id))
         (buf (ardentia--task-buffer (plist-get ardentia-review--task :file)))
         (pending (list :buffer buf :id id
                        :was-modified (buffer-modified-p buf)
                        :review-buffer (current-buffer))))
    (with-current-buffer buf
      (save-restriction
        (widen)
        (save-excursion
          (unless (ardentia--goto-id id)
            (user-error "%s" ardentia--changed-message))
          (org-add-log-setup 'note))))
    (setq ardentia-review--pending-note pending)))

;;;; Editing the task like org-agenda

(defun ardentia-review--edit (fn)
  "Call FN with the task of this review buffer, then render the buffer again.
FN returns `deferred' when org still has to log the change: the buffer is
rendered once that is done."
  (unless (and ardentia-review--task (derived-mode-p 'ardentia-review-mode))
    (user-error "Not an ardentia review buffer"))
  (unless (eq (funcall fn (append ardentia-review--task
                                  (list :tags (plist-get ardentia-review--info
                                                         :tags))))
              'deferred)
    (ardentia-review-render)))

(defun ardentia-review-set-tags ()
  "Set the tags of the task of this review buffer, like `org-agenda-set-tags'."
  (interactive)
  (ardentia-review--edit #'ardentia-set-tags-1))

(defun ardentia-review-schedule (&optional arg)
  "Schedule the task of this review buffer, like `org-agenda-schedule'.
With a prefix ARG remove the schedule."
  (interactive "P")
  (let ((time (ardentia--read-planning-time 'scheduled arg))
        (buf (current-buffer)))
    (ardentia-review--edit
     (lambda (task) (ardentia-schedule-1 task arg time buf)))))

(defun ardentia-review-deadline (&optional arg)
  "Set the deadline of the task of this review buffer, like `org-agenda-deadline'.
With a prefix ARG remove the deadline."
  (interactive "P")
  (let ((time (ardentia--read-planning-time 'deadline arg))
        (buf (current-buffer)))
    (ardentia-review--edit
     (lambda (task) (ardentia-deadline-1 task arg time buf)))))

(defun ardentia-review-todo (&optional arg)
  "Change the TODO state of the task of this review buffer, like `org-agenda-todo'.
ARG is the prefix argument of `org-todo'."
  (interactive "P")
  (let ((buf (current-buffer)))
    (ardentia-review--edit
     (lambda (task) (ardentia-set-todo-1 task nil arg buf)))))

(defun ardentia-review--set-priority (action)
  "Apply priority ACTION to the task of this review buffer."
  (ardentia-review--edit
   (lambda (task)
     (ardentia-set-priority-1 (plist-get task :file) (plist-get task :id)
                              action (plist-get task :line)
                              (plist-get task :title)
                              (ardentia--task-lifecycle-tags task)))))

(defun ardentia-review-priority ()
  "Set the priority of the task of this review buffer; SPC removes it."
  (interactive)
  (ardentia-review--set-priority (ardentia--read-priority)))

(defun ardentia-review-priority-up ()
  "Raise the priority of the task of this review buffer."
  (interactive)
  (ardentia-review--set-priority 'up))

(defun ardentia-review-priority-down ()
  "Lower the priority of the task of this review buffer."
  (interactive)
  (ardentia-review--set-priority 'down))

(provide 'ardentia-review)
;;; ardentia-review.el ends here

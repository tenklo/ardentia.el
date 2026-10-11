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
;; buffer: the TODO state and priority, properties, the newest run
;; report from the LOGBOOK, the notes the human wrote after it, the child
;; headings (result sections and open TODOs "for you") and the original
;; task text.  Everything is read
;; from the visiting buffer, not from disk.

;;; Code:

(require 'org)
(require 'org-indent)
(require 'seq)
(require 'subr-x)
(require 'ardentia-core)
(require 'ardentia-actions)
(require 'ardentia-session)

(declare-function ardentia-dashboard-refresh-all "ardentia-dashboard" ())

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

;;;; Collecting from the visiting buffer

(defun ardentia-review--children ()
  "Return the direct children of the heading at point as plists."
  (let ((level (org-current-level)) result)
    (save-excursion
      (when (org-goto-first-child)
        (let ((more t))
          (while more
            (when (= (org-current-level) (1+ level))
              (let ((todo (org-get-todo-state)))
                (push (list :title (org-get-heading t t t t)
                            :todo todo
                            :done (and todo (member todo org-done-keywords) t)
                            :tags (org-get-tags nil t)
                            :id (org-entry-get nil "ID"))
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
                       :todo (org-get-todo-state)
                       ;; Explicit cookie only; no default priority.
                       :priority (nth 3 (org-heading-components))
                       :body body)
                 log
                 (ardentia--classify-children (ardentia-review--children))))))))

;;;; Rendering

(defun ardentia-review--link (child)
  "Return an org link (or plain title) for the CHILD plist."
  (let ((title (replace-regexp-in-string
                "\\[" "(" (replace-regexp-in-string "\\]" ")" (plist-get child :title)))))
    (if (plist-get child :id)
        (format "[[id:%s][%s]]" (plist-get child :id) title)
      title)))

(defun ardentia-review--render-string (info)
  "Return the org text for the review data INFO."
  (with-temp-buffer
    (let ((tags (plist-get info :tags))
          (todo (plist-get info :todo))
          (prio (plist-get info :priority))
          (agents (cdr (assoc "AI_STATUS" (plist-get info :props)))))
      (insert "* "
              (if todo (concat todo " ") "")
              (if prio (format "[#%c] " prio) "")
              (plist-get info :title)
              (if tags (format " :%s:" (string-join tags ":")) "") "\n")
      (insert (format "- File :: [[file:%s][%s]]\n"
                      (abbreviate-file-name (plist-get info :file))
                      (file-name-nondirectory (plist-get info :file))))
      (insert (format "- Status :: %s%s\n"
                      (or todo "no TODO keyword")
                      (if agents (format ", agents: %s" agents) ""))))
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
    (when (plist-get info :notes)
      (insert "** Your notes after the report\n")
      (dolist (note (plist-get info :notes))
        (insert (string-join note "\n") "\n")))
    (insert "** Result sections\n")
    (if (plist-get info :results)
        (dolist (c (plist-get info :results))
          (insert "- " (ardentia-review--link c) "\n"))
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
  :menu-doc "Keys of the ardentia review buffer."
  :groups
  (("Review"
    ("a" "Accept" ardentia-review-accept)
    ("s" "Send back" ardentia-review-send-back)
    ("y" "Let agents do child TODO" ardentia-review-yourself))
   ("Task"
    ("o" "Visit heading" ardentia-review-visit)
    ("t" "Transcript" ardentia-review-transcript))
   ("View"
    ("g" "Re-render" ardentia-review-render)
    ("q" "Quit" quit-window))))

(define-derived-mode ardentia-review-mode org-mode "Ardentia-Review"
  "Read-only org view of one task waiting for review.

\\{ardentia-review-mode-map}"
  (setq buffer-read-only t)
  (when ardentia-review-indent (org-indent-mode 1))
  (setq-local truncate-lines ardentia-review-truncate-lines)
  (setq-local word-wrap (not ardentia-review-truncate-lines))
  (setq ardentia--current-task-function (lambda () ardentia-review--task)))

(declare-function ardentia "ardentia" ())

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
    (goto-char (point-min))
    (forward-line (1- line))
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

(provide 'ardentia-review)
;;; ardentia-review.el ends here

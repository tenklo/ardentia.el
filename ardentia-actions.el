;;; ardentia-actions.el --- Mutations of agent tasks for ardentia -*- lexical-binding: t -*-

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

;; All mutations happen in the buffer that visits the org file, through
;; org functions, like the human would do it by hand.  Files are never
;; edited behind Emacs' back.  After a mutation the buffer is saved only
;; when it was unmodified before; otherwise it stays modified and the
;; human is told, because the timer only sees saved state.

;;; Code:

(require 'org)
(require 'subr-x)
(require 'ardentia-core)

(declare-function ardentia-dashboard-refresh-all "ardentia-dashboard" ())
(declare-function ardentia-dashboard--live-buffers "ardentia-dashboard" ())

(defvar ardentia-review--pending-note)
(defvar org-priority-highest)
(defvar org-priority-lowest)

(defconst ardentia--changed-message "Task state changed -- refresh (g)"
  "Error message when a task is not in the expected state any more.")

(defun ardentia--task-buffer (file)
  "Return the buffer visiting FILE, in sync with the disk.
Revert it when the file changed and the buffer is unmodified; refuse
with a `user-error' when the buffer has unsaved changes."
  ;; `find-file-noselect' on a stale visited buffer would prompt, so use
  ;; the existing buffer as is and check it ourselves.
  (let ((buf (or (find-buffer-visiting file) (find-file-noselect file))))
    (unless (verify-visited-file-modtime buf)
      (if (buffer-modified-p buf)
          (user-error "File changed on disk and buffer has unsaved changes")
        (with-current-buffer buf (revert-buffer t t t))))
    buf))

(defun ardentia--direct-tags ()
  "Return the tags set directly on the heading at point."
  (org-get-tags nil t))

(defun ardentia--require-tag (tag)
  "Signal a `user-error' unless the heading at point carries TAG.
TAG is a tag, or a list of tags that must all be present; nil requires
nothing."
  (dolist (tg (if (listp tag) tag (list tag)))
    (unless (member tg (ardentia--direct-tags))
      (user-error "%s" ardentia--changed-message))))

(defun ardentia--find-child (title tag)
  "Move to the direct child of the heading at point named TITLE with TAG.
Return non-nil on success; otherwise point stays on the parent.  The
child must have a TODO keyword that is not a done state."
  (let ((parent (point)) (more (org-goto-first-child)) found)
    (while (and more (not found))
      (if (and (equal (org-get-heading t t t t) title)
               (member tag (ardentia--direct-tags))
               (let ((todo (org-get-todo-state)))
                 (and todo (not (member todo org-done-keywords)))))
          (setq found t)
        (setq more (org-get-next-sibling))))
    (unless found (goto-char parent))
    found))

(defun ardentia--goto-heading (id line title)
  "Move to the heading of a task in the current, wide buffer.
With ID, go to the heading with that ID or signal a `user-error'.
Without an ID (a freshly captured task has none yet) use LINE, accepted
only when the heading there has exactly TITLE; otherwise use the unique
heading with exactly TITLE.  Never settle on a heading with another
title: zero or several matches signal a `user-error' asking to refresh."
  (let ((id (ardentia--val id)))
    (if id
        (or (ardentia--goto-id id)
            (user-error "Heading with ID %s not found in %s" id
                        (or buffer-file-name (buffer-name))))
      (unless (and (stringp title) (not (string-empty-p title)))
        (user-error "Task has neither an ID nor a title -- refresh the dashboard (g)"))
      (let (found)
        (when (and (integerp line) (> line 0))
          (goto-char (point-min))
          (forward-line (1- line))
          (when (and (org-at-heading-p)
                     (equal (org-get-heading t t t t) title))
            (setq found (point))))
        (unless found
          (let (matches)
            (goto-char (point-min))
            (while (re-search-forward org-heading-regexp nil t)
              (when (equal (org-get-heading t t t t) title)
                (push (line-beginning-position) matches)))
            (unless (= (length matches) 1)
              (user-error "%s heading \"%s\" in %s -- refresh the dashboard (g)"
                          (if matches "Several headings titled" "No heading titled")
                          title (file-name-nondirectory
                                 (or buffer-file-name (buffer-name)))))
            (setq found (car matches))))
        (goto-char found)))))

(defvar ardentia--await-log nil
  "Non-nil while an edit has to wait for a log entry that org schedules.
Org writes state, reschedule and note log entries from
`post-command-hook' (and asks for the text of a note), so saving right
after the edit would miss them.  With this set, `ardentia--call-on-heading'
leaves the saving and the refresh to `ardentia-review--note-stored'
whenever org has set up a log entry.  The value is the review buffer to
render afterwards, or t.")

(defun ardentia--log-pending-p ()
  "Return non-nil when org has set up a log entry it has not written yet."
  (and org-log-setup (memq 'org-add-log-note post-command-hook) t))

(defun ardentia--cancel-pending-log ()
  "Drop a log entry org has set up but not written yet."
  (when (ardentia--log-pending-p)
    (remove-hook 'post-command-hook 'org-add-log-note)
    (setq org-log-setup nil)))

(defmacro ardentia--without-logging (&rest body)
  "Run BODY so that org writes no log entry and asks for no note.
A closing time stamp is still set when `org-log-done' is `time' or
`note'."
  (declare (indent 0))
  `(let ((org-log-done (if (eq org-log-done 'note) 'time org-log-done))
         (org-log-repeat nil)
         (org-log-reschedule nil)
         (org-log-redeadline nil)
         (org-todo-log-states nil))
     ,@body))

(defun ardentia--at-heading (id child-title tag fn &optional line title)
  "Call FN at the heading with ID in the current buffer, widened.
CHILD-TITLE, TAG, LINE and TITLE are as for `ardentia--call-on-heading'.
Return the value of FN."
  (org-with-wide-buffer
   (ardentia--goto-heading id line title)
   (when child-title
     (unless (ardentia--find-child child-title tag)
       (user-error "%s" ardentia--changed-message)))
   (ardentia--require-tag tag)
   (funcall fn)))

(defun ardentia--finish-buffer (buf was-modified)
  "Save BUF after an edit unless WAS-MODIFIED, in which case say so."
  (with-current-buffer buf
    (if was-modified
        (message "ardentia: %s has unsaved changes; the timer only sees saved state"
                 (file-name-nondirectory (or buffer-file-name (buffer-name))))
      (save-buffer))))

(defun ardentia--refresh-dashboards ()
  "Refresh the dashboards, if there are any; never signal an error."
  (when (and (fboundp 'ardentia-dashboard--live-buffers)
             (ardentia-dashboard--live-buffers))
    (condition-case err
        (ardentia-dashboard-refresh-all)
      (error (message "ardentia: refresh failed: %s"
                      (error-message-string err))))))

(defun ardentia--call-on-heading (file id child-title tag fn &optional line title)
  "Call FN at the heading with ID in FILE, in its visiting buffer.
Without an ID, LINE and TITLE locate the heading, see
`ardentia--goto-heading'.
With CHILD-TITLE, FN runs at the open TODO child of that name instead.
The heading must carry TAG, a tag or a list of tags (see
`ardentia--require-tag').  Save the buffer afterwards when it was
unmodified before; refresh the dashboards.  Return t, or `deferred'
when `ardentia--await-log' is set and org has a log entry pending: the
saving and the refresh then wait for it, see `ardentia-review--note-stored'."
  (let* ((buf (ardentia--task-buffer file))
         (was-modified (buffer-modified-p buf))
         pending)
    (with-current-buffer buf
      (ardentia--at-heading
       id child-title tag
       (lambda ()
         (funcall fn)
         (when (and ardentia--await-log (ardentia--log-pending-p))
           (setq pending
                 (list :buffer buf
                       :id (save-excursion
                             (org-back-to-heading t)
                             (org-entry-get nil "ID"))
                       :was-modified was-modified
                       :review-buffer (and (bufferp ardentia--await-log)
                                           ardentia--await-log)
                       :changed t))))
       line title))
    (cond (pending (setq ardentia-review--pending-note pending)
                   'deferred)
          (t (ardentia--finish-buffer buf was-modified)
             (ardentia--refresh-dashboards)
             t))))

(defun ardentia--note-lines (note)
  "Return the non-blank, trimmed lines of NOTE."
  (seq-remove #'string-empty-p
              (mapcar #'string-trim (split-string note "\n"))))

(defun ardentia--insert-log-note (note)
  "Insert NOTE as a logbook note under the heading at point.
The format matches the notes org writes itself."
  (let ((lines (ardentia--note-lines note)))
    (when lines
      (goto-char (org-log-beginning t))
      (let* ((ind (make-string (current-indentation) ?\s))
             (heading (org-replace-escapes
                       (cdr (assq 'note org-log-note-headings))
                       (list (cons "%t" (format-time-string
                                         (org-time-stamp-format t t))))))
             (text (concat ind "- " heading " \\\\\n"
                           (mapconcat (lambda (l) (concat ind "  " l "\n"))
                                      lines ""))))
        (beginning-of-line)
        (insert text)))))

(defun ardentia--swap-tag (from to)
  "Replace the tag FROM by TO on the heading at point."
  (org-toggle-tag from 'off)
  (org-toggle-tag to 'on))

;;;; Public actions (non-interactive cores)

(defun ardentia-accept-1 (file id)
  "Set the task ID in FILE to DONE.  The humanreview tag stays."
  (ardentia--call-on-heading
   file id nil "humanreview"
   (lambda () (org-todo "DONE"))))

(defun ardentia-send-back-1 (file id note)
  "Send the task ID in FILE back to the agents with NOTE.
Replace humanreview by aiready and add NOTE to the logbook."
  (when (string-blank-p note)
    (user-error "A note for the agents is required"))
  (ardentia--call-on-heading
   file id nil "humanreview"
   (lambda ()
     (ardentia--swap-tag "humanreview" "aiready")
     (ardentia--insert-log-note note))))

(defun ardentia-yourself-1 (file id child-title note)
  "Hand the child CHILD-TITLE of task ID in FILE back to the agents.
Replace humanreview by aiready on the child; add NOTE (may be empty)."
  (ardentia--call-on-heading
   file id child-title "humanreview"
   (lambda ()
     (ardentia--swap-tag "humanreview" "aiready")
     (ardentia--insert-log-note note))))

(defun ardentia-set-priority-1 (file id action &optional line title tags)
  "Change the priority of the task ID in FILE according to ACTION.
ACTION is what `org-priority' accepts: a character, `up', `down',
`remove' or `set' (prompt).  A task without ID is found by LINE and
TITLE, see `ardentia--goto-heading'.  The heading must carry TAGS (a
list; nil requires nothing)."
  (ardentia--call-on-heading
   file id nil tags
   (lambda () (org-priority action))
   line title))

;;;; Edits of a task row, like org-agenda

(defun ardentia--task-lifecycle-tags (task)
  "Return the lifecycle and trouble tags TASK was listed under.
A row whose heading lost one of them is out of date."
  (seq-filter (lambda (tag)
                (or (assoc tag ardentia--queue-tags)
                    (assoc tag ardentia--trouble-tags)))
              (plist-get task :tags)))

(defun ardentia--call-on-task (task fn &optional await-log)
  "Call FN at the heading of TASK, a task plist, see `ardentia--call-on-heading'.
The heading must still carry the lifecycle tags of TASK.  A non-nil
AWAIT-LOG (t or a review buffer) lets a pending log entry of org
finish the edit, see `ardentia--await-log'."
  (let ((ardentia--await-log await-log))
    (ardentia--call-on-heading
     (plist-get task :file) (plist-get task :id) nil
     (ardentia--task-lifecycle-tags task) fn
     (plist-get task :line) (plist-get task :title))))

(defun ardentia--read-priority ()
  "Read a priority like `org-agenda-priority'.
Return a character, or `remove' for SPC."
  (let ((c (read-char-exclusive
            (format "Priority %c-%c, SPC to remove: "
                    org-priority-highest org-priority-lowest))))
    (cond ((eq c ?\s) 'remove)
          ((and (>= (upcase c) org-priority-highest)
                (<= (upcase c) org-priority-lowest))
           (upcase c))
          (t (user-error "Priority must be between `%c' and `%c'"
                         org-priority-highest org-priority-lowest)))))

(defun ardentia--read-planning-time (kind arg)
  "Read the date for a KIND (`scheduled' or `deadline') edit once.
Return the date string of `org-read-date', or nil when ARG, the prefix
argument, is non-nil: it removes (or, doubled, adjusts) the date."
  (unless arg
    (org-read-date nil nil nil
                   (if (eq kind 'deadline) "Deadline: " "Schedule: "))))

(defun ardentia-set-tags-1 (task)
  "Set the tags of TASK at its heading, with `org-set-tags-command'.
That is what `org-agenda-set-tags' does, so a fast tag selection or
completion setup applies as it does in org."
  (ardentia--call-on-task
   task (lambda () (call-interactively #'org-set-tags-command))))

(defun ardentia-schedule-1 (task arg time &optional review-buffer)
  "Schedule TASK like `org-schedule' with ARG and TIME at its heading.
ARG (a prefix argument) \='(4) removes the schedule.  REVIEW-BUFFER is
rendered again when a log entry finishes the edit later."
  (ardentia--call-on-task task (lambda () (org-schedule arg time))
                          (or review-buffer t)))

(defun ardentia-deadline-1 (task arg time &optional review-buffer)
  "Set the deadline of TASK like `org-deadline' with ARG and TIME.
ARG (a prefix argument) \='(4) removes the deadline.  REVIEW-BUFFER is
as for `ardentia-schedule-1'."
  (ardentia--call-on-task task (lambda () (org-deadline arg time))
                          (or review-buffer t)))

(defun ardentia-set-todo-1 (task state &optional arg review-buffer)
  "Change the TODO state of TASK at its heading.
With the string STATE set that state.  Without it run `org-todo'
interactively, with the prefix argument ARG, like `org-agenda-todo'.
REVIEW-BUFFER is as for `ardentia-schedule-1'."
  (ardentia--call-on-task
   task
   (lambda ()
     (if (stringp state)
         (org-todo state)
       (let ((current-prefix-arg arg))
         (call-interactively #'org-todo))))
   (or review-buffer t)))

;;;; Bulk edits

(defun ardentia--group-by-file (tasks)
  "Return TASKS as an alist (FILE . TASKS), files in order of first use."
  (let (files groups)
    (dolist (task tasks)
      (let ((file (plist-get task :file)))
        (unless (member file files) (push file files))
        (push task (alist-get file groups nil nil #'equal))))
    (mapcar (lambda (file)
              (cons file (reverse (cdr (assoc file groups)))))
            (nreverse files))))

(defun ardentia-bulk-edit (tasks fn)
  "Call FN with each of TASKS, a list of task plists, at its heading.
Work file by file: every file buffer is saved once, when it was
unmodified before, and the dashboards are refreshed once at the end.
A task that fails (its state changed, its heading is gone, FN signals an
error) is skipped.  FN must not leave a log entry to org's command loop;
use `ardentia--without-logging'.  Return a plist with :done, :skipped
and :reason, the error message of the first skipped task or nil."
  (let ((done 0) (skipped 0) reason)
    (dolist (group (ardentia--group-by-file tasks))
      (let ((file-done 0) buf was-modified)
        (condition-case err
            (setq buf (ardentia--task-buffer (car group))
                  was-modified (buffer-modified-p buf))
          (error (setq skipped (+ skipped (length (cdr group)))
                       reason (or reason (error-message-string err))
                       buf nil)))
        (when buf
          (dolist (task (cdr group))
            (condition-case err
                (with-current-buffer buf
                  (ardentia--at-heading
                   (plist-get task :id) nil
                   (ardentia--task-lifecycle-tags task)
                   (lambda ()
                     (unwind-protect (funcall fn task)
                       (ardentia--cancel-pending-log)))
                   (plist-get task :line) (plist-get task :title))
                  (setq file-done (1+ file-done)))
              (error (setq skipped (1+ skipped)
                           reason (or reason (error-message-string err))))))
          (when (> file-done 0)
            (ardentia--finish-buffer buf was-modified))
          (setq done (+ done file-done)))))
    (when (> done 0) (ardentia--refresh-dashboards))
    (list :done done :skipped skipped :reason reason)))

;;;; Interactive commands

(defun ardentia-accept (task)
  "Accept the review TASK (a task plist) after asking for confirmation.
Return non-nil when the task was accepted."
  (interactive (list (ardentia--current-task)))
  (when (y-or-n-p (format "Accept \"%s\" (set DONE)? " (plist-get task :title)))
    (ardentia-accept-1 (plist-get task :file) (plist-get task :id))
    t))

(defun ardentia-send-back (task)
  "Send the review TASK (a task plist) back to the agents with a note."
  (interactive (list (ardentia--current-task)))
  (let ((note (read-string "Note for the agents: ")))
    (ardentia-send-back-1 (plist-get task :file) (plist-get task :id) note)))

(defvar-local ardentia--current-task-function nil
  "Function returning the task at point in the current ardentia buffer.")

(defun ardentia--current-task ()
  "Return the task at point of the current ardentia buffer or signal an error."
  (let ((task (and ardentia--current-task-function
                   (funcall ardentia--current-task-function))))
    (cond ((null task) (user-error "No task here"))
          ((plist-get task :merge) (user-error "Not a task: a running merge"))
          (t task))))

(provide 'ardentia-actions)
;;; ardentia-actions.el ends here

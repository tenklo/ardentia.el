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
  "Signal a `user-error' unless the heading at point carries TAG."
  (unless (member tag (ardentia--direct-tags))
    (user-error "%s" ardentia--changed-message)))

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

(defun ardentia--call-on-heading (file id child-title tag fn)
  "Call FN at the heading with ID in FILE, in its visiting buffer.
With CHILD-TITLE, FN runs at the open TODO child of that name instead.
The heading must carry TAG.  Save the buffer afterwards when it was
unmodified before; refresh the dashboards."
  (let* ((buf (ardentia--task-buffer file))
         (was-modified (buffer-modified-p buf)))
    (with-current-buffer buf
      (org-with-wide-buffer
       (unless (ardentia--goto-id id)
         (user-error "Heading with ID %s not found in %s" id file))
       (when child-title
         (unless (ardentia--find-child child-title tag)
           (user-error "%s" ardentia--changed-message)))
       (ardentia--require-tag tag)
       (funcall fn))
      (if was-modified
          (message "ardentia: %s has unsaved changes; the timer only sees saved state"
                   (file-name-nondirectory file))
        (save-buffer)))
    (when (and (fboundp 'ardentia-dashboard--live-buffers)
               (ardentia-dashboard--live-buffers))
      (condition-case err
          (ardentia-dashboard-refresh-all)
        (error (message "ardentia: refresh failed: %s"
                        (error-message-string err)))))))

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

(defun ardentia-set-priority-1 (file id action)
  "Change the priority of the ready task ID in FILE according to ACTION.
ACTION is what `org-priority' accepts: a character, `up', `down',
`remove' or `set' (prompt)."
  (ardentia--call-on-heading
   file id nil "aiready"
   (lambda () (org-priority action))))

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
  (or (and ardentia--current-task-function
           (funcall ardentia--current-task-function))
      (user-error "No task here")))

(provide 'ardentia-actions)
;;; ardentia-actions.el ends here

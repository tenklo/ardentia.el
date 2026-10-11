;;; ardentia-alert.el --- Alert for new review tasks in ardentia -*- lexical-binding: t -*-

;; Copyright (C) 2026 Karsten Klöss

;; Author: Karsten Klöss
;; Assisted-by: Claude Code:Claude Opus 5.5, Claude Sonnet 5.5
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

;; Opt-in alert inside Emacs when a task newly reaches the review queue
;; (tag humanreview).  Set `ardentia-review-alert' to enable it.
;;
;; The alert has no timer of its own.  It piggybacks on the refreshes of
;; the mode-line counter and of the dashboard, through
;; `ardentia-review-tasks-functions', so alerts only arrive while one of
;; them runs.  The first observation after loading is only a baseline:
;; tasks already waiting are not announced.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'ardentia-core)

(defcustom ardentia-review-alert nil
  "How to alert in Emacs when a task newly reaches the review queue.
nil means no alert.  The symbol `message' shows the titles in the
echo area.  A function is called with the list of the new task
plists, so it can hand them to `alert', `notifications-notify' or
the like.  Alerts only arrive while the mode-line counter or a
dashboard refreshes."
  :type '(choice (const :tag "Off" nil)
                 (const :tag "Echo area message" message)
                 (function :tag "Function"))
  :group 'ardentia)

(defcustom ardentia-review-alert-ding nil
  "Non-nil means ring the bell when `ardentia-review-alert' fires."
  :type 'boolean
  :group 'ardentia)

(defcustom ardentia-review-alert-skip-command
  (expand-file-name "ai/tools/notify-skip" ardentia-org-directory)
  "Program that decides whether a task should not be announced.
It is called as PROGRAM FILE ID.  Exit status 0 means skip the
task; any other status (or a program that cannot run) means alert.
nil disables the check.  Tasks without an ID are never checked."
  :type '(choice (const :tag "No check" nil) file)
  :group 'ardentia)

(defvar ardentia--review-alert-seen nil
  "Keys of the review tasks of the last observation.")

(defvar ardentia--review-alert-baseline nil
  "Non-nil once a first observation has been recorded.")

(defun ardentia--review-alert-text (tasks)
  "Return the message text announcing TASKS."
  (let ((titles (mapcar (lambda (tk) (or (plist-get tk :title) "?")) tasks)))
    (if (cdr titles)
        (format "ardentia: %d new tasks for review: %s"
                (length titles) (string-join titles "; "))
      (format "ardentia: for review: %s" (car titles)))))

(defun ardentia--review-alert-fire (tasks)
  "Alert about TASKS according to `ardentia-review-alert'."
  (when (and tasks ardentia-review-alert)
    (condition-case err
        (progn
          (if (eq ardentia-review-alert 'message)
              (message "%s" (ardentia--review-alert-text tasks))
            (funcall ardentia-review-alert tasks))
          (when ardentia-review-alert-ding (ding)))
      (error (message "ardentia: review alert failed: %s"
                      (error-message-string err))))))

(defun ardentia--review-alert-dispatch (tasks)
  "Run the skip check for TASKS, then alert about those not skipped."
  (let* ((cmd ardentia-review-alert-skip-command)
         (check (and cmd (file-executable-p cmd)))
         (tasks (copy-sequence tasks))
         (join (ardentia--make-join
                (length tasks)
                (lambda (skips)
                  (ardentia--review-alert-fire
                   (delq nil (cl-mapcar (lambda (tk skip) (and (not skip) tk))
                                        tasks skips))))))
         (i 0))
    (dolist (task tasks)
      (let ((index i) (id (plist-get task :id)))
        (if (and check id)
            (ardentia--run-async
             cmd (list (plist-get task :file) id)
             (lambda (status _out _err) (funcall join index (eql status 0))))
          (funcall join index nil)))
      (setq i (1+ i)))))

(defun ardentia--review-alert-check (review)
  "Alert about the tasks in REVIEW that were not seen before.
REVIEW is the list of current review-queue tasks.  Never signals."
  (condition-case err
      (let* ((seen ardentia--review-alert-seen)
             (new (and ardentia--review-alert-baseline
                       (seq-remove (lambda (tk)
                                     (member (ardentia--task-key tk) seen))
                                   review))))
        (setq ardentia--review-alert-seen (mapcar #'ardentia--task-key review)
              ardentia--review-alert-baseline t)
        (when (and new ardentia-review-alert)
          (ardentia--review-alert-dispatch new)))
    (error (message "ardentia: review alert check failed: %s"
                    (error-message-string err)))))

(add-hook 'ardentia-review-tasks-functions #'ardentia--review-alert-check)

(provide 'ardentia-alert)
;;; ardentia-alert.el ends here

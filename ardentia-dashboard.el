;;; ardentia-dashboard.el --- Dashboard and review inbox for ardentia -*- lexical-binding: t -*-

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

;; `ardentia-dashboard' lists the agent queues (review, running, ready, later)
;; in a `tabulated-list-mode' buffer.  `ardentia-review-inbox' is the same
;; view filtered to the review queue.  Refreshing is asynchronous.

;;; Code:

(require 'tabulated-list)
(require 'seq)
(require 'subr-x)
(require 'ardentia-core)
(require 'ardentia-actions)
(require 'ardentia-review)
(require 'ardentia-session)

(declare-function ardentia "ardentia" ())

(defvar-local ardentia-dashboard--filter nil
  "Queue symbol this dashboard buffer is restricted to, or nil for all.")

(defun ardentia-dashboard--info-line (&optional state)
  "Return the header line text from STATE (default `ardentia--state')."
  (let* ((st (or state ardentia--state))
         (timer (plist-get st :timer))
         (time (plist-get st :time)))
    (concat
     (format " Host: %s" (or (plist-get st :host) "unknown"))
     (format " | Timer last: %s next: %s"
             (if st (or (plist-get timer :last) "unknown") "unknown")
             (cond ((plist-get timer :running) "after the current run")
                   (st (or (plist-get timer :next) "unknown"))
                   (t "unknown")))
     (format " | Refreshed: %s"
             (if time (format-time-string "%H:%M:%S" time) "never"))
     (if (plist-get st :error) (format " | ERROR: %s" (plist-get st :error)) "")
     (if (plist-get st :refreshing) " | refreshing..." ""))))

(defcustom ardentia-dashboard-show-later t
  "Non-nil means the dashboard lists ready tasks that are not yet due.
Those are :aiready: tasks whose SCHEDULED time still lies ahead; the
runner leaves them alone until then.  They show up as queue \"later\".
The key `l' toggles this per dashboard buffer."
  :type 'boolean
  :group 'ardentia)

(defvar-local ardentia-dashboard--show-later 'default
  "Buffer-local override of `ardentia-dashboard-show-later'.
The symbol `default' means: use the option.")

(defun ardentia-dashboard--show-later-p ()
  "Return non-nil when this buffer shows later tasks."
  (if (eq ardentia-dashboard--show-later 'default)
      ardentia-dashboard-show-later
    ardentia-dashboard--show-later))

(defun ardentia-dashboard--entries (tasks filter now &optional hide-later)
  "Return `tabulated-list-entries' for TASKS restricted to queue FILTER at NOW.
With HIDE-LATER, ready tasks that are not yet due are left out."
  (mapcar (lambda (task) (list task (ardentia--task-row task now)))
          (ardentia--sort-tasks
           (seq-filter (lambda (tk)
                         (and (or (null filter)
                                  (eq (plist-get tk :queue) filter))
                              (not (and hide-later
                                        (ardentia--task-later-p tk now)))))
                       tasks)
           now)))

(defvar-keymap ardentia-dashboard-mode-map
  :doc "Keymap for `ardentia-dashboard-mode'."
  "g" #'ardentia-dashboard-refresh
  "RET" #'ardentia-dashboard-enter
  "t" #'ardentia-dashboard-transcript
  "A" #'ardentia-dashboard-agent-shell
  "o" #'ardentia-dashboard-visit
  "a" #'ardentia-dashboard-accept
  "s" #'ardentia-dashboard-send-back
  "l" #'ardentia-dashboard-toggle-later
  "q" #'quit-window
  "?" #'ardentia)

(define-derived-mode ardentia-dashboard-mode tabulated-list-mode "Ardentia"
  "Major mode listing the agent team's queues.

\\{ardentia-dashboard-mode-map}"
  (setq tabulated-list-format
        [("Queue" 8 nil) ("Status" 10 nil) ("Host" 10 nil) ("Att" 4 nil)
         ("Age" 5 nil) ("Flags" 12 nil) ("File" 18 nil) ("Title" 0 nil)])
  (setq tabulated-list-padding 1)
  (setq tabulated-list-sort-key nil)
  (setq ardentia--current-task-function #'ardentia-dashboard--task-at-point)
  ;; The info line owns the header line; the column titles go into the buffer.
  (setq tabulated-list-use-header-line nil)
  (tabulated-list-init-header)
  (setq header-line-format
        '(:eval (ardentia-dashboard--info-line))))

(defun ardentia-dashboard--task-at-point ()
  "Return the task plist of the row at point, or nil."
  (tabulated-list-get-id))

(defun ardentia-dashboard--render ()
  "Fill the current dashboard buffer from `ardentia--state'."
  (setq tabulated-list-entries
        (ardentia-dashboard--entries (plist-get ardentia--state :tasks)
                                     ardentia-dashboard--filter
                                     (current-time)
                                     (not (ardentia-dashboard--show-later-p))))
  (tabulated-list-print t)
  (force-mode-line-update))

(defun ardentia-dashboard--live-buffers ()
  "Return the live dashboard buffers."
  (seq-filter (lambda (b) (with-current-buffer b
                            (derived-mode-p 'ardentia-dashboard-mode)))
              (buffer-list)))

(defun ardentia-dashboard--render-all ()
  "Render all live dashboard buffers."
  (dolist (buf (ardentia-dashboard--live-buffers))
    (with-current-buffer buf (ardentia-dashboard--render))))

(defun ardentia-dashboard-refresh-all ()
  "Refresh all dashboard buffers asynchronously."
  (ardentia-refresh #'ardentia-dashboard--render-all)
  (ardentia-dashboard--render-all))

(defun ardentia-dashboard-refresh ()
  "Refresh the dashboard asynchronously; Emacs stays responsive."
  (interactive)
  (ardentia-dashboard-refresh-all))

(defun ardentia-dashboard--open (name filter)
  "Show the dashboard buffer NAME restricted to queue FILTER and refresh it."
  (let ((buf (get-buffer-create name)))
    (with-current-buffer buf
      (unless (derived-mode-p 'ardentia-dashboard-mode)
        (ardentia-dashboard-mode))
      (setq ardentia-dashboard--filter filter)
      (ardentia-dashboard--render))
    (pop-to-buffer-same-window buf)
    (ardentia-dashboard-refresh-all)))

;;;###autoload
(defun ardentia-dashboard ()
  "Show the dashboard of all agent queues."
  (interactive)
  (ardentia-dashboard--open "*ardentia*" nil))

;;;###autoload
(defun ardentia-review-inbox ()
  "Show the dashboard restricted to tasks waiting for review."
  (interactive)
  (ardentia-dashboard--open "*ardentia-review*" 'review))

(defun ardentia-dashboard--require-task (&optional review)
  "Return the task at point; with REVIEW require the review queue."
  (let ((task (or (ardentia-dashboard--task-at-point) (user-error "No task here"))))
    (when (and review (not (eq (plist-get task :queue) 'review)))
      (user-error "Task is not waiting for review"))
    task))

(defun ardentia-dashboard-review ()
  "Open the review buffer of the task at point."
  (interactive)
  (ardentia-review-task (ardentia-dashboard--require-task t)))

;;;###autoload
(defun ardentia-dashboard-enter ()
  "Open the task at point: review buffer, or session viewer when running.
Other queues signal a `user-error'; `t' shows their last transcript."
  (interactive)
  (let ((task (ardentia-dashboard--require-task)))
    (pcase (plist-get task :queue)
      ('review (ardentia-review-task task))
      ('running (ardentia-session-open-viewer task))
      (_ (user-error
          "Task is neither waiting for review nor running (t: last transcript)")))))

(defun ardentia-dashboard-transcript ()
  "Open the latest session transcript of the task at point, in any queue."
  (interactive)
  (ardentia-session-open (ardentia-dashboard--require-task)))

(defun ardentia-dashboard-agent-shell ()
  "Open the latest session of the task at point in agent-shell."
  (interactive)
  (ardentia-session-open-agent-shell (ardentia-dashboard--require-task)))

(defun ardentia-dashboard-toggle-later ()
  "Hide or show ready tasks that are not yet due in this dashboard buffer."
  (interactive)
  (setq ardentia-dashboard--show-later
        (not (ardentia-dashboard--show-later-p)))
  (ardentia-dashboard--render)
  (message "Later tasks: %s"
           (if (ardentia-dashboard--show-later-p) "shown" "hidden")))

(defun ardentia-dashboard-visit ()
  "Visit the heading of the task at point in its org file."
  (interactive)
  (ardentia-visit-task (ardentia-dashboard--require-task)))

(defun ardentia-dashboard-accept ()
  "Accept the review task at point."
  (interactive)
  (ardentia-accept (ardentia-dashboard--require-task t)))

(defun ardentia-dashboard-send-back ()
  "Send the review task at point back to the agents."
  (interactive)
  (ardentia-send-back (ardentia-dashboard--require-task t)))

(provide 'ardentia-dashboard)
;;; ardentia-dashboard.el ends here

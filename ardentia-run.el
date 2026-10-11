;;; ardentia-run.el --- Start the runner on one task for ardentia -*- lexical-binding: t -*-

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

;; Start the runner on exactly one `:aiready:' task right now instead of
;; waiting for the timer.  The command line is `ardentia-run-task-command'.
;; `ardentia-run-task-at-point' works in an org buffer,
;; `ardentia-dashboard-run' (in ardentia-dashboard.el) on a dashboard row.
;;
;; `ardentia-stop-task' does the opposite for a running task: it runs
;; `ardentia-stop-task-command' asynchronously, which signals the agent
;; process on this computer; the runner then closes the task.  Bound to
;; `k' on a dashboard row and in a session transcript.

;;; Code:

(require 'org)
(require 'org-id)
(require 'seq)
(require 'subr-x)
(require 'ardentia-core)
(require 'ardentia-actions)

(declare-function ardentia-dashboard-refresh-all "ardentia-dashboard" ())
(declare-function ardentia-dashboard--live-buffers "ardentia-dashboard" ())

(defcustom ardentia-run-task-command
  (list "systemd-run" "--user" "--collect" "--quiet"
        "--setenv=PATH" "--setenv=ORG_AIREADY_ONLY_ID=%i"
        "--description=ardentia: run task %i" "--property=Nice=10"
        (expand-file-name "ai/tools/process-aiready" ardentia-org-directory)
        "%f")
  "Command that starts the runner on one task right now.
A list of strings: the program and its arguments, started without a
shell.  In every element \"%i\" is replaced by the org ID of the task
and \"%f\" by the absolute name of its file.

The contract: the command must start the runner on exactly that task
and return; the runner does the work.  The default uses a transient
systemd user unit, so the run is independent of Emacs and its output
goes to the journal.  A bare `--setenv=PATH' copies PATH from Emacs's
environment.  Nil switches the feature off."
  :type '(choice (const :tag "Off" nil) (repeat string))
  :group 'ardentia)

(defcustom ardentia-stop-task-command
  (list (expand-file-name "ai/tools/aiready-stop" ardentia-org-directory)
        "%i")
  "Command that stops the run of one task on this computer.
A list of strings: the program and its arguments, started without a
shell.  In every element \"%i\" is replaced by the org ID of the task
and \"%f\" by the absolute name of its file.

The contract: the command signals the agent process working on that
task; the runner then closes the task (blocked, back to review).  It
prints one line of text and exits with
  0  stopped, and the task is closed,
  3  the task is not running on this computer,
  4  the signal was sent, but the runner has not finished closing the
     task yet,
  1  usage error,
  2  any other failure.
Only the computer that claimed the task can stop it.  Nil switches the
feature off."
  :type '(choice (const :tag "Off" nil) (repeat string))
  :group 'ardentia)

(defconst ardentia-run--buffer-name " *ardentia-run*"
  "Name of the hidden buffer that collects the output of the start command.")

(defun ardentia-run--substitute (command id file)
  "Return COMMAND with \"%i\" replaced by ID and \"%f\" by FILE."
  (mapcar (lambda (arg)
            (replace-regexp-in-string
             "%[if]"
             (lambda (m) (if (equal m "%i") id file))
             arg t t))
          command))

(defun ardentia-run--refresh ()
  "Refresh the dashboards when there are any; never signal an error."
  (when (and (fboundp 'ardentia-dashboard--live-buffers)
             (ardentia-dashboard--live-buffers))
    (condition-case err
        (ardentia-dashboard-refresh-all)
      (error (message "ardentia: refresh failed: %s"
                      (error-message-string err))))))

(defun ardentia-run--require-command ()
  "Signal a `user-error' when `ardentia-run-task-command' is nil."
  (unless ardentia-run-task-command
    (user-error "Set `ardentia-run-task-command' to enable running a task")))

(defun ardentia-run-task-1 (file id title)
  "Start the runner on the task ID in FILE, titled TITLE.
Run `ardentia-run-task-command' asynchronously and return the process."
  (ardentia-run--require-command)
  (let* ((command (ardentia-run--substitute ardentia-run-task-command
                                            id (expand-file-name file)))
         (buf (get-buffer-create ardentia-run--buffer-name)))
    (with-current-buffer buf
      (goto-char (point-max))
      (insert (format "\n$ %s\n" (mapconcat #'identity command " "))))
    (make-process
     :name "ardentia-run"
     :buffer buf
     :command command
     :noquery t
     :connection-type 'pipe
     :sentinel
     (lambda (proc _event)
       (when (memq (process-status proc) '(exit signal))
         (let ((code (process-exit-status proc)))
           (if (eql code 0)
               (progn (message "ardentia: runner started for \"%s\"" title)
                      (ardentia-run--refresh))
             (message "ardentia: starting the runner for \"%s\" failed (exit %s), see buffer %s"
                      title code ardentia-run--buffer-name))))))))

(defun ardentia-run--local-host ()
  "Return the local host name if known, else nil.  Never blocks."
  (or ardentia--local-host (plist-get ardentia--state :host)))

(defun ardentia-run--check-host (tags)
  "Signal a `user-error' when TAGS bind the task to another host.
Only an explicit NAME_only tag counts; an unknown local host does not
block."
  (let ((only (ardentia--only-host tags))
        (local (ardentia-run--local-host)))
    (when (and only local (not (equal only local)))
      (user-error "Task belongs to host %s" only))))

(defun ardentia-run-task (task)
  "Run the ready TASK (a task plist) now after asking for confirmation.
A task shown as `later' counts as ready.  Return the process, or nil
when declined."
  (ardentia-run--require-command)
  (unless (eq (plist-get task :queue) 'ready)
    (user-error "Task is not waiting for the agents (:aiready:)"))
  (ardentia-run--check-host (plist-get task :tags))
  (when (y-or-n-p (format "Run \"%s\" now? " (plist-get task :title)))
    (let ((id (ardentia--val (plist-get task :id))))
      (if id
          (ardentia-run-task-1 (plist-get task :file) id
                               (plist-get task :title))
        ;; Freshly captured: no ID yet.  Create it in the buffer.
        (let ((buf (ardentia--task-buffer (plist-get task :file))))
          (with-current-buffer buf
            (org-with-wide-buffer
             (ardentia--goto-heading nil (plist-get task :line)
                                     (plist-get task :title))
             (ardentia--require-tag "aiready")
             (ardentia-run--start-here (plist-get task :title)))))))))

(defun ardentia-run--start-here (title)
  "Start the runner on the heading at point, titled TITLE.
Create a missing ID.  The runner reads the file from disk, so a buffer
with unsaved changes is saved first after asking, and again after the
ID was created.  Return the process."
  (when (buffer-modified-p)
    (unless (y-or-n-p (format "Save %s first (the runner reads the file from disk)? "
                              (file-name-nondirectory buffer-file-name)))
      (user-error "Cancelled"))
    (save-buffer))
  (let ((id (org-id-get-create)))
    (when (buffer-modified-p) (save-buffer))
    (ardentia-run-task-1 buffer-file-name id title)))

;;;###autoload
(defun ardentia-run-task-at-point ()
  "Run the `:aiready:' task at point in this org buffer now.
Needs the tag set directly on the heading and a task that is not done.
The runner reads the file from disk, so unsaved changes are saved first
after asking.  A missing ID is created."
  (interactive)
  (unless (derived-mode-p 'org-mode) (user-error "Not an org buffer"))
  (unless buffer-file-name (user-error "Buffer does not visit a file"))
  (ardentia-run--require-command)
  (save-excursion
    (org-back-to-heading t)
    (unless (member "aiready" (org-get-tags nil t))
      (user-error "Heading is not tagged :aiready:"))
    (when (org-entry-is-done-p)
      (user-error "Task is already done"))
    (ardentia-run--check-host (org-get-tags nil t))
    (let ((title (org-get-heading t t t t)))
      (unless (y-or-n-p (format "Run \"%s\" now? " title))
        (user-error "Cancelled"))
      (ardentia-run--start-here title))))

;;;###autoload
(defun ardentia-stop-task (task)
  "Stop the run of the running TASK (a task plist) after asking.
Run `ardentia-stop-task-command' asynchronously and return the process,
or nil when declined.  Only the computer that claimed the task can stop
it."
  (unless ardentia-stop-task-command
    (user-error "Set `ardentia-stop-task-command' to enable stopping a task"))
  (unless (eq (plist-get task :queue) 'running)
    (user-error "Task is not running"))
  (let ((claimed (ardentia--task-prop task "AI_CLAIMED_BY"))
        (local (ardentia-run--local-host))
        (id (ardentia--val (plist-get task :id)))
        (title (plist-get task :title)))
    (when (and claimed local (not (equal claimed local)))
      (user-error "Claimed by %s; stop it on that computer" claimed))
    (unless id (user-error "Task has no ID"))
    (when (y-or-n-p (format "Stop the run of \"%s\"? " title))
      (let* ((file (plist-get task :file))
             (command (ardentia-run--substitute
                       ardentia-stop-task-command id
                       (if file (expand-file-name file) "")))
             (buf (get-buffer-create ardentia-run--buffer-name)))
        (with-current-buffer buf
          (goto-char (point-max))
          (insert (format "\n$ %s\n" (mapconcat #'identity command " "))))
        (make-process
         :name "ardentia-stop"
         :buffer buf
         :command command
         :noquery t
         :connection-type 'pipe
         :sentinel
         (lambda (proc _event)
           (when (memq (process-status proc) '(exit signal))
             (let ((code (process-exit-status proc)))
               (pcase code
                 (0 (message "ardentia: stopped \"%s\"" title)
                    (ardentia-run--refresh))
                 (3 (message "ardentia: \"%s\" is not running on this computer"
                             title))
                 (4 (message "ardentia: stop signal sent for \"%s\", the runner is still closing it"
                             title)
                    (ardentia-run--refresh))
                 (_ (message "ardentia: stopping \"%s\" failed (exit %s), see buffer %s"
                             title code ardentia-run--buffer-name)))))))))))

(provide 'ardentia-run)
;;; ardentia-run.el ends here

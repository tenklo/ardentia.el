;;; ardentia-session.el --- Follow the agent session of a task -*- lexical-binding: t -*-

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

;; The runner starts one Claude Code session per task, with the org
;; directory as working directory.  Claude Code writes the transcript as
;; JSONL under `~/.claude/projects/SLUG/SESSION.jsonl' while it runs.
;; This file finds the transcript of a task (the prompt contains
;; "ID=<org id>"), renders it as readable text in `ardentia-session-mode'
;; and follows it like `tail -f'.  Transcripts of subagents
;; (SESSION/subagents/agent-*.jsonl) are merged in by time stamp with a
;; "[type]" prefix.
;;
;; Optionally the session can be resumed in `agent-shell' (a separate
;; package, not required): see `ardentia-session-open-agent-shell'.

;;; Code:

(require 'json)
(require 'time-date)
(require 'seq)
(require 'subr-x)
(require 'ardentia-core)
(require 'ardentia-actions)

(declare-function ardentia "ardentia" ())
(declare-function agent-shell-resume-session "agent-shell" (session-id))
(defvar agent-shell-session-restore-verbosity)

(defcustom ardentia-session-directory nil
  "Directory holding the session transcripts (*.jsonl), or nil.
Nil derives it from `ardentia-org-directory': the truename of that
directory with every character other than letters and digits replaced
by \"-\", below ~/.claude/projects/."
  :type '(choice (const :tag "Derived from the org directory" nil) directory)
  :group 'ardentia)

(defcustom ardentia-session-viewer 'ardentia
  "Viewer that RET opens on a running task in the dashboard.
`ardentia' is the built-in read-only buffer, `agent-shell' resumes the
session in the optional agent-shell package."
  :type '(choice (const :tag "Built-in transcript buffer" ardentia)
                 (const :tag "agent-shell (optional package)" agent-shell))
  :group 'ardentia)

(defcustom ardentia-session-agent-shell-verbosity 'full
  "Value of `agent-shell-session-restore-verbosity' when resuming a session.
`full' replays the whole conversation."
  :type '(choice (const minimal) (const last) (const first-last) (const full))
  :group 'ardentia)

(defcustom ardentia-session-follow-interval 2
  "Seconds between two reads of the transcript while following."
  :type 'number
  :group 'ardentia)

(defcustom ardentia-session-result-lines 4
  "Number of lines of a tool result (or prompt) shown before cutting it."
  :type 'integer
  :group 'ardentia)

(defcustom ardentia-session-line-width 160
  "Width at which single-line summaries (tool calls, result lines) are cut."
  :type 'integer
  :group 'ardentia)

(defcustom ardentia-session-idle-seconds 900
  "Seconds without a write after which a transcript counts as finished.
The newest write of the main transcript and of its subagent transcripts
counts, so a long subagent run keeps the session live.  A transcript is
also finished when the last refresh of the dashboard state does not list
its task as running."
  :type 'integer
  :group 'ardentia)

(defcustom ardentia-session-search-limit 200
  "Maximum number of newest transcripts examined when looking for a task."
  :type 'integer
  :group 'ardentia)

(defface ardentia-session-time '((t :inherit shadow))
  "Face of the time stamps in the transcript buffer."
  :group 'ardentia)

(defface ardentia-session-prompt '((t :inherit font-lock-doc-face))
  "Face of prompts in the transcript buffer."
  :group 'ardentia)

(defface ardentia-session-tool '((t :inherit font-lock-function-name-face))
  "Face of tool calls in the transcript buffer."
  :group 'ardentia)

(defface ardentia-session-result '((t :inherit shadow))
  "Face of tool results in the transcript buffer."
  :group 'ardentia)

(defface ardentia-session-error '((t :inherit error))
  "Face of failed tool results in the transcript buffer."
  :group 'ardentia)

;;;; Finding the transcript

(defun ardentia-session--slug (directory)
  "Return the Claude Code project slug of DIRECTORY.
The truename is taken first; every character other than ASCII letters
and digits becomes \"-\", and a trailing slash is dropped."
  (replace-regexp-in-string
   "[^A-Za-z0-9]" "-"
   (directory-file-name (file-truename (expand-file-name directory)))))

(defun ardentia-session--dir ()
  "Return the directory with the transcripts."
  (or ardentia-session-directory
      (expand-file-name (ardentia-session--slug ardentia-org-directory)
                        "~/.claude/projects/")))

(defun ardentia-session--mtime (file)
  "Return the modification time of FILE as a float, or 0.0."
  (let ((attrs (file-attributes file)))
    (if attrs (float-time (file-attribute-modification-time attrs)) 0.0)))

(defun ardentia-session--head-matches-p (file id)
  "Return non-nil when the first 16 KB of FILE contain \"ID=ID\"."
  (condition-case nil
      (with-temp-buffer
        (set-buffer-multibyte nil)
        (insert-file-contents-literally file nil 0 16384)
        (goto-char (point-min))
        (re-search-forward
         (concat "ID=" (regexp-quote id) "\\(?:[^A-Za-z0-9_-]\\|\\'\\)")
         nil t))
    (error nil)))

(defun ardentia-session-find (task)
  "Return the transcript file of TASK, or nil.
That is the newest *.jsonl in `ardentia-session--dir' whose beginning
mentions the task id.  For a running task only files modified shortly
before its AI_CLAIMED time stamp or later are considered."
  (let* ((id (plist-get task :id))
         (dir (ardentia-session--dir))
         (claimed (and (eq (plist-get task :queue) 'running)
                       (ardentia--parse-stamp
                        (ardentia--task-prop task "AI_CLAIMED"))))
         (cutoff (and claimed (- (float-time claimed) 300)))
         (files (and id (file-directory-p dir)
                     (directory-files dir t "\\.jsonl\\'" t)))
         (timed (sort (mapcar (lambda (f) (cons (ardentia-session--mtime f) f))
                              files)
                      (lambda (a b) (> (car a) (car b))))))
    (when (> (length timed) ardentia-session-search-limit)
      (setq timed (seq-take timed ardentia-session-search-limit)))
    (cdr (seq-find (lambda (cell)
                     (and (or (null cutoff) (>= (car cell) cutoff))
                          (ardentia-session--head-matches-p (cdr cell) id)))
                   timed))))

;;;; Reading and rendering

(defun ardentia-session--read-new (file offset)
  "Read the complete lines of FILE after byte OFFSET.
Return (RECORDS . NEW-OFFSET).  A partial last line is left for the
next call; a line that is not valid JSON is skipped."
  (let ((size (or (file-attribute-size (file-attributes file)) 0)))
    (if (<= size offset)
        (cons nil offset)
      (let ((chunk (with-temp-buffer
                     (set-buffer-multibyte nil)
                     (insert-file-contents-literally file nil offset size)
                     (buffer-string))))
        (let ((last (string-search "\n" (reverse chunk))))
          (if (null last)
              (cons nil offset)
            (let* ((len (- (length chunk) last))
                   (text (decode-coding-string (substring chunk 0 len) 'utf-8))
                   records)
              (dolist (line (split-string text "\n" t))
                (let ((rec (condition-case nil
                               (json-parse-string line :object-type 'alist
                                                  :array-type 'list
                                                  :null-object nil
                                                  :false-object nil)
                             (error nil))))
                  (when (and (consp rec) (consp (car rec)))
                    (push rec records))))
              (cons (nreverse records) (+ offset len)))))))))

(defun ardentia-session--time (ts)
  "Return the local time \"HH:MM:SS\" of the ISO time stamp TS."
  (or (ignore-errors
        (format-time-string "%H:%M:%S" (date-to-time ts)))
      "--:--:--"))

(defun ardentia-session--limit-lines (text)
  "Cut TEXT to `ardentia-session-result-lines' lines and the line width."
  (let* ((lines (split-string (string-trim-right text) "\n"))
         (n (max 1 ardentia-session-result-lines))
         (shown (mapcar (lambda (l) (truncate-string-to-width
                                     l ardentia-session-line-width nil nil "..."))
                        (seq-take lines n)))
         (more (- (length lines) n)))
    (string-join (if (> more 0)
                     (append shown (list (format "... (%d more lines)" more)))
                   shown)
                 "\n")))

(defun ardentia-session--entry (time prefix marker text face)
  "Return the transcript lines for TEXT at TIME.
PREFIX is the subagent prefix, MARKER the text before the first line,
FACE the face of the text."
  (let* ((lines (split-string text "\n"))
         (head (concat (propertize time 'face 'ardentia-session-time) " "
                       (if (string-empty-p prefix) "" (concat prefix " "))))
         (indent (make-string (+ 9 (if (string-empty-p prefix) 0
                                     (1+ (length prefix))))
                              ?\s))
         (pad (make-string (length marker) ?\s)))
    (concat head
            (mapconcat (lambda (l) (propertize l 'face face))
                       (cons (concat marker (car lines))
                             (mapcar (lambda (l) (concat pad l)) (cdr lines)))
                       (concat "\n" indent))
            "\n")))

(defun ardentia-session--tool-summary (name input)
  "Return the one-line summary of the tool call NAME with INPUT."
  (let* ((type (alist-get 'subagent_type input))
         (what (seq-some (lambda (key)
                           (let ((v (alist-get key input)))
                             (and (stringp v) (not (string-blank-p v)) v)))
                         '(description command file_path pattern path url
                                       query prompt)))
         (one (replace-regexp-in-string "[ \t\n]+" " " (or what ""))))
    (format "> %s%s: %s"
            name (if (stringp type) (format " (%s)" type) "")
            (truncate-string-to-width (string-trim one)
                                      ardentia-session-line-width nil nil "..."))))

(defun ardentia-session--result-text (content)
  "Return the text of a tool result CONTENT (string or list of blocks)."
  (when (vectorp content) (setq content (append content nil)))
  (cond ((stringp content) content)
        ((listp content)
         (mapconcat (lambda (b) (let ((tx (and (consp b) (alist-get 'text b))))
                                  (if (stringp tx) tx "")))
                    content "\n"))
        (t "")))

(defun ardentia-session--render-record (rec prefix)
  "Return the text for the record REC, or nil when it shows nothing.
PREFIX is the subagent prefix (a string, possibly empty)."
  (condition-case nil
      (let* ((type (alist-get 'type rec))
             (time (ardentia-session--time (alist-get 'timestamp rec)))
             (content (let ((c (alist-get 'content (alist-get 'message rec))))
                        (if (vectorp c) (append c nil) c)))
             (out nil)
             (emit (lambda (marker text face)
                     (push (ardentia-session--entry time prefix marker text face)
                           out))))
        (cond
         ((equal type "user")
          (if (stringp content)
              (funcall emit "Prompt: " (ardentia-session--limit-lines content)
                       'ardentia-session-prompt)
            (dolist (b (and (listp content) content))
              (let ((bt (and (consp b) (alist-get 'type b))))
                (cond
                 ((equal bt "tool_result")
                  (let ((err (eq (alist-get 'is_error b) t))
                        (text (ardentia-session--result-text
                               (alist-get 'content b))))
                    (funcall emit (if err "! " "< ")
                             (ardentia-session--limit-lines text)
                             (if err 'ardentia-session-error
                               'ardentia-session-result))))
                 ((and (equal bt "text") (stringp (alist-get 'text b)))
                  (funcall emit "Prompt: "
                           (ardentia-session--limit-lines (alist-get 'text b))
                           'ardentia-session-prompt)))))))
         ((equal type "assistant")
          (dolist (b (if (stringp content)
                         (list `((type . "text") (text . ,content)))
                       (and (listp content) content)))
            (let ((bt (and (consp b) (alist-get 'type b))))
              (cond
               ((and (equal bt "text") (stringp (alist-get 'text b))
                     (not (string-blank-p (alist-get 'text b))))
                (funcall emit "" (string-trim (alist-get 'text b)) 'default))
               ((equal bt "tool_use")
                (funcall emit ""
                         (ardentia-session--tool-summary
                          (or (alist-get 'name b) "?") (alist-get 'input b))
                         'ardentia-session-tool)))))))
        (and out (apply #'concat (nreverse out))))
    (error nil)))

;;;; Sources: main transcript and subagents

(defun ardentia-session--subagent-files (file)
  "Return the subagent transcripts that belong to the transcript FILE."
  (let ((dir (expand-file-name "subagents" (file-name-sans-extension file))))
    (and (file-directory-p dir)
         (directory-files dir t "\\`agent-.*\\.jsonl\\'" t))))

(defun ardentia-session--subagent-prefix (file)
  "Return the prefix \"[TYPE]\" for the subagent transcript FILE."
  (let* ((meta (concat (file-name-sans-extension file) ".meta.json"))
         (type (ignore-errors
                 (with-temp-buffer
                   (insert-file-contents meta)
                   (alist-get 'agentType
                              (json-parse-buffer :object-type 'alist))))))
    (format "[%s]" (if (and (stringp type) (not (string-empty-p type)))
                       type "agent"))))

;;;; The buffer

(defvar-local ardentia-session--task nil
  "Task plist shown in this transcript buffer.")

(defvar-local ardentia-session--file nil
  "Main transcript file of this buffer.")

(defvar-local ardentia-session--sources nil
  "List of (FILE PREFIX . OFFSET) for each transcript merged into the buffer.")

(defvar-local ardentia-session--follow nil
  "Non-nil means this buffer is polled for new content.")

(defvar-local ardentia-session--finished nil
  "Non-nil when the session counts as finished.")

(defvar ardentia-session--timer nil
  "The timer polling all following transcript buffers, or nil.")

(defvar-keymap ardentia-session-mode-map
  :doc "Keymap for `ardentia-session-mode'."
  "g" #'ardentia-session-reread
  "f" #'ardentia-session-toggle-follow
  "o" #'ardentia-session-visit
  "a" #'ardentia-session-agent-shell-here
  "q" #'quit-window
  "?" #'ardentia)

(define-derived-mode ardentia-session-mode special-mode "Ardentia-Session"
  "Read-only view of the agent session transcript of one task.

\\{ardentia-session-mode-map}"
  (setq ardentia--current-task-function (lambda () ardentia-session--task))
  (setq header-line-format '(:eval (ardentia-session--header)))
  (add-hook 'kill-buffer-hook #'ardentia-session--cancel-if-unused nil t))

(defun ardentia-session--header ()
  "Return the header line of the current transcript buffer."
  (format " %s | session %s | %s"
          (or (plist-get ardentia-session--task :title) "task")
          (if ardentia-session--file
              (file-name-base ardentia-session--file) "none")
          (cond (ardentia-session--finished "finished")
                (ardentia-session--follow "following")
                (t "not following"))))

(defun ardentia-session--live-buffers (&optional except)
  "Return the live transcript buffers, without EXCEPT."
  (seq-filter (lambda (b) (and (not (eq b except))
                               (buffer-local-value 'ardentia-session--file b)
                               (eq (buffer-local-value 'major-mode b)
                                   'ardentia-session-mode)))
              (buffer-list)))

(defun ardentia-session--wanted-p (buf)
  "Return non-nil when the buffer BUF needs polling."
  (and (buffer-local-value 'ardentia-session--follow buf)
       (not (buffer-local-value 'ardentia-session--finished buf))))

(defun ardentia-session--cancel-if-unused ()
  "Cancel the timer when no other transcript buffer needs it."
  (unless (seq-some #'ardentia-session--wanted-p
                    (ardentia-session--live-buffers (current-buffer)))
    (ardentia-session--cancel-timer)))

(defun ardentia-session--cancel-timer ()
  "Cancel the polling timer."
  (when (timerp ardentia-session--timer)
    (cancel-timer ardentia-session--timer))
  (setq ardentia-session--timer nil))

(defun ardentia-session--ensure-timer ()
  "Start the polling timer when a buffer needs it and none runs."
  (when (and (null ardentia-session--timer)
             (seq-some #'ardentia-session--wanted-p
                       (ardentia-session--live-buffers)))
    (setq ardentia-session--timer
          (run-with-timer ardentia-session-follow-interval
                          ardentia-session-follow-interval
                          #'ardentia-session--tick))))

(defun ardentia-session--tick ()
  "Poll every transcript buffer that follows; cancel the timer when none."
  (dolist (buf (ardentia-session--live-buffers))
    (when (ardentia-session--wanted-p buf)
      (with-current-buffer buf
        (condition-case err (ardentia-session--poll)
          (error (message "ardentia session: %s" (error-message-string err)))))))
  (unless (seq-some #'ardentia-session--wanted-p
                    (ardentia-session--live-buffers))
    (ardentia-session--cancel-timer)))

(defun ardentia-session--task-running-p (task)
  "Return non-nil when TASK is running.
After a refresh the dashboard state decides; before that the :queue of
TASK does."
  (if (and ardentia--state (plist-get ardentia--state :time))
      (let ((key (ardentia--task-key task)))
        (seq-some (lambda (tk) (and (equal (ardentia--task-key tk) key)
                                    (eq (plist-get tk :queue) 'running)))
                  (plist-get ardentia--state :tasks)))
    (eq (plist-get task :queue) 'running)))

(defun ardentia-session--finished-p ()
  "Return non-nil when the session of the current buffer is finished.
That is: the last refresh does not list the task as running, or neither
the transcript nor one of its subagent transcripts was written for
`ardentia-session-idle-seconds'.  While a subagent works only its own
file grows, so the main file alone would read as idle."
  (or (not (ardentia-session--task-running-p ardentia-session--task))
      (> (- (float-time)
            (apply #'max
                   (mapcar #'ardentia-session--mtime
                           (cons ardentia-session--file
                                 (ardentia-session--subagent-files
                                  ardentia-session--file)))))
         ardentia-session-idle-seconds)))

(defun ardentia-session--discover ()
  "Add newly appeared subagent transcripts to `ardentia-session--sources'."
  (dolist (f (ardentia-session--subagent-files ardentia-session--file))
    (unless (assoc f ardentia-session--sources)
      (setq ardentia-session--sources
            (append ardentia-session--sources
                    (list (cons f (cons (ardentia-session--subagent-prefix f)
                                        0))))))))

(defun ardentia-session--poll ()
  "Read new content of all transcripts into the current buffer.
Only bytes after the stored offsets are read.  New records of all
sources are merged by time stamp and appended; point stays at the end
when it was there."
  (ardentia-session--discover)
  (let (items)
    (dolist (src ardentia-session--sources)
      (let ((res (ardentia-session--read-new (car src) (cddr src))))
        (setcdr (cdr src) (cdr res))
        (dolist (rec (car res))
          (push (cons (or (alist-get 'timestamp rec) "") (cons (cadr src) rec))
                items))))
    (setq items (sort (nreverse items) :key #'car :lessp #'string<))
    (let ((text (apply #'concat
                       (mapcar (lambda (it)
                                 (ardentia-session--render-record
                                  (cddr it) (cadr it)))
                               items))))
      (unless (string-empty-p text)
        (let* ((inhibit-read-only t)
               (at-end (= (point) (point-max)))
               (wins (seq-filter (lambda (w) (= (window-point w) (point-max)))
                                 (get-buffer-window-list nil nil t))))
          (save-excursion
            (goto-char (point-max))
            (insert text))
          (when at-end (goto-char (point-max)))
          (dolist (w wins) (set-window-point w (point-max)))
          (set-buffer-modified-p nil)))))
  (setq ardentia-session--finished (ardentia-session--finished-p))
  (force-mode-line-update))

(defun ardentia-session--reset ()
  "Erase the current buffer and forget all offsets."
  (let ((inhibit-read-only t))
    (erase-buffer))
  (setq ardentia-session--sources
        (list (cons ardentia-session--file (cons "" 0)))))

(defun ardentia-session-reread ()
  "Read the transcript again from the start."
  (interactive)
  (unless ardentia-session--task (user-error "Not an ardentia transcript buffer"))
  (let ((file (or (ardentia-session-find ardentia-session--task)
                  ardentia-session--file)))
    (setq ardentia-session--file file)
    (ardentia-session--reset)
    (ardentia-session--poll)
    (goto-char (point-max))
    (ardentia-session--ensure-timer)))

(defun ardentia-session-toggle-follow ()
  "Toggle following the transcript."
  (interactive)
  (setq ardentia-session--follow (not ardentia-session--follow))
  (when ardentia-session--follow
    (ardentia-session--poll)
    (ardentia-session--ensure-timer))
  (force-mode-line-update)
  (message "Following: %s" (if ardentia-session--follow "on" "off")))

(defun ardentia-session-visit ()
  "Visit the heading of the task of this transcript."
  (interactive)
  (ardentia-visit-task ardentia-session--task))

(defun ardentia-session--require-file (task)
  "Return the transcript of TASK or signal a `user-error'."
  (or (ardentia-session-find task)
      (user-error "No session transcript found for \"%s\""
                  (or (plist-get task :title) "this task"))))

;;;###autoload
(defun ardentia-session-open (task)
  "Open the transcript of TASK (a task plist, default the task at point).
While the task runs the buffer follows the transcript like `tail -f'."
  (interactive (list (ardentia--current-task)))
  (let* ((file (ardentia-session--require-file task))
         (buf (get-buffer-create
               (format "*ardentia-session: %s*" (plist-get task :title)))))
    (with-current-buffer buf
      (unless (derived-mode-p 'ardentia-session-mode) (ardentia-session-mode))
      (setq ardentia-session--task task
            ardentia-session--file file)
      (ardentia-session--reset)
      (ardentia-session--poll)
      (setq ardentia-session--follow (not ardentia-session--finished))
      (goto-char (point-max))
      (ardentia-session--ensure-timer))
    (pop-to-buffer buf)
    (with-current-buffer buf (goto-char (point-max)))
    buf))

;;;; agent-shell (optional)

;;;###autoload
(defun ardentia-session-open-agent-shell (task)
  "Resume the session of TASK in `agent-shell', which must be installed.
Starts a new agent process that loads the session and shows the
conversation according to `ardentia-session-agent-shell-verbosity'.
Asks first when the task still runs: its session is then written by
another process."
  (interactive (list (ardentia--current-task)))
  (unless (require 'agent-shell nil t)
    (user-error "The package agent-shell is not installed (it is optional)"))
  (let* ((file (ardentia-session--require-file task))
         (id (file-name-base file)))
    (when (and (ardentia-session--task-running-p task)
               (not (yes-or-no-p
                     "Task is still running; resuming starts a second process on its session.  Continue? ")))
      (user-error "Cancelled"))
    (let ((default-directory
           (file-name-as-directory
            (file-truename (expand-file-name ardentia-org-directory))))
          (agent-shell-session-restore-verbosity
           ardentia-session-agent-shell-verbosity))
      (agent-shell-resume-session id))))

(defun ardentia-session-agent-shell-here ()
  "Open the task of this transcript buffer in `agent-shell'."
  (interactive)
  (ardentia-session-open-agent-shell ardentia-session--task))

;;;###autoload
(defun ardentia-session-open-viewer (task)
  "Open TASK in the viewer chosen by `ardentia-session-viewer'."
  (interactive (list (ardentia--current-task)))
  (if (eq ardentia-session-viewer 'agent-shell)
      (ardentia-session-open-agent-shell task)
    (ardentia-session-open task)))

(provide 'ardentia-session)
;;; ardentia-session.el ends here

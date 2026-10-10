;;; ardentia-capture.el --- Create and delegate agent tasks for ardentia -*- lexical-binding: t -*-

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

;; `ardentia-new-task' and `ardentia-delegate' create an :aiready: task
;; with `org-capture'.  `ardentia-delegate' quotes the active region in a
;; quote block: quoted material is data for the agents, not instructions.

;;; Code:

(require 'org)
(require 'org-capture)
(require 'subr-x)
(require 'ardentia-core)

(defcustom ardentia-capture-file
  (expand-file-name "refile.org" ardentia-org-directory)
  "Org file that receives new agent tasks."
  :type 'file
  :group 'ardentia)

(defconst ardentia-capture--any-host "any (default host)"
  "Completion candidate for a task without a host tag.")

(defun ardentia-capture--host-tag (host)
  "Return the tag suffix for HOST, e.g. \"desktop_only:\", or \"\" for nil."
  (if (null host)
      ""
    (let ((tag (concat host "_only")))
      (unless (ardentia--valid-tag-p tag)
        (user-error "Host name %S does not make a valid org tag" host))
      (concat tag ":"))))

(defun ardentia-capture--template (host &optional with-quote)
  "Return the capture template string for HOST (nil for any host).
With WITH-QUOTE, the initial text (`%i', the region) is appended in a
quote block."
  (concat "* TODO %^{Task} :refile:aiready:" (ardentia-capture--host-tag host) "\n"
          ":LOGBOOK:\n- Added: %U\n- Added From: %a\n:END:\n"
          (if with-quote "#+begin_quote\n%i\n#+end_quote\n" "")
          "%?"))

(defun ardentia-capture--read-host ()
  "Ask for the host that should run the task; return its name or nil."
  (let* ((hosts (plist-get (ardentia-hosts) :hosts))
         (choice (completing-read "Run on: "
                                  (cons ardentia-capture--any-host hosts)
                                  nil t nil nil ardentia-capture--any-host)))
    (unless (equal choice ardentia-capture--any-host) choice)))

(defun ardentia-capture--run (host region)
  "Capture an agent task for HOST; REGION is quoted text or nil."
  (let ((org-capture-templates
         `(("a" "Agent task" entry (file ,ardentia-capture-file)
            ,(ardentia-capture--template host (and region t)))))
        (org-capture-initial region))
    (org-capture nil "a")))

;;;###autoload
(defun ardentia-new-task ()
  "Capture a new task for the agent team."
  (interactive)
  (ardentia-capture--run (ardentia-capture--read-host) nil))

;;;###autoload
(defun ardentia-delegate ()
  "Capture an agent task that refers to the current buffer.
With an active region the region is quoted as data in the task."
  (interactive)
  (let ((host (ardentia-capture--read-host))
        (region (when (use-region-p)
                  (buffer-substring-no-properties (region-beginning) (region-end)))))
    (ardentia-capture--run host region)))

(provide 'ardentia-capture)
;;; ardentia-capture.el ends here

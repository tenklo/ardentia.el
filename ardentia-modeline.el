;;; ardentia-modeline.el --- Mode-line counter for ardentia -*- lexical-binding: t -*-

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

;; `ardentia-mode-line-mode' shows the number of tasks waiting for review
;; in the mode line.  The timer exists only while the mode is on.

;;; Code:

(require 'seq)
(require 'ardentia-core)

(defvar ardentia--mode-line-timer nil
  "Timer that refreshes the review counter.")

(defvar ardentia-mode-line-mode)
(defvar ardentia-mode-line-interval)

(defun ardentia--mode-line-timer-restart (&optional delay)
  "Cancel the refresh timer; start it again when the mode is on.
The first run happens after DELAY seconds, default the interval."
  (when (timerp ardentia--mode-line-timer)
    (cancel-timer ardentia--mode-line-timer))
  (setq ardentia--mode-line-timer nil)
  (when ardentia-mode-line-mode
    (setq ardentia--mode-line-timer
          (run-at-time (or delay ardentia-mode-line-interval)
                       ardentia-mode-line-interval
                       #'ardentia--mode-line-tick))))

(defcustom ardentia-mode-line-interval 300
  "Seconds between two refreshes of the mode-line counter.
Changing it via Customize or `setopt' takes effect immediately; a plain
`setq' only applies after toggling `ardentia-mode-line-mode'."
  :type 'integer
  :set (lambda (sym val)
         (set-default sym val)
         (when (timerp ardentia--mode-line-timer)
           (ardentia--mode-line-timer-restart)))
  :group 'ardentia)

(defface ardentia-mode-line '((t :inherit warning :weight bold))
  "Face of the review counter in the mode line."
  :group 'ardentia)

(defconst ardentia--mode-line-construct '(:eval (ardentia--mode-line-string))
  "The element added to `global-mode-string'.")

(defvar ardentia--mode-line-busy nil
  "Non-nil while a counter refresh is running.")

(defun ardentia--mode-line-string ()
  "Return the mode-line text: \" AR:N\" for N > 0 reviews, else \"\"."
  (if (and (integerp ardentia--review-count) (> ardentia--review-count 0))
      (propertize (format " AR:%d" ardentia--review-count)
                  'face 'ardentia-mode-line
                  'help-echo "ardentia: tasks waiting for your review")
    ""))

(defun ardentia--mode-line-refresh ()
  "Refresh the review counter asynchronously."
  (unless ardentia--mode-line-busy
    (setq ardentia--mode-line-busy t)
    (ardentia--fetch-tags
     '("humanreview")
     (lambda (tasks err)
       (setq ardentia--mode-line-busy nil)
       (if err
           (message "ardentia: counter refresh failed: %s" err)
         (let ((review (ardentia--review-tasks tasks)))
           (setq ardentia--review-count (length review))
           (force-mode-line-update t)
           (ardentia--run-review-hook review)))))))

(defun ardentia--mode-line-tick ()
  "Timer function: refresh the counter and never signal an error."
  (condition-case err
      (ardentia--mode-line-refresh)
    (error (setq ardentia--mode-line-busy nil)
           (message "ardentia: mode-line timer: %s" (error-message-string err)))))

;;;###autoload
(define-minor-mode ardentia-mode-line-mode
  "Show the number of tasks waiting for review in the mode line."
  :global t
  :group 'ardentia
  (setq global-mode-string
        (delete ardentia--mode-line-construct
                (if (listp global-mode-string) global-mode-string
                  (list global-mode-string))))
  (when ardentia-mode-line-mode
    (setq global-mode-string
          (append (or global-mode-string (list ""))
                  (list ardentia--mode-line-construct))))
  (ardentia--mode-line-timer-restart 0))

(provide 'ardentia-modeline)
;;; ardentia-modeline.el ends here

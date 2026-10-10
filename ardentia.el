;;; ardentia.el --- View and command layer for the org-mode agent team -*- lexical-binding: t -*-

;; Copyright (C) 2026 Karsten Klöss

;; Author: Karsten Klöss
;; Assisted-by: Claude Code:Claude Opus 5.5, Claude Sonnet 5.5
;; URL: https://github.com/tenklo/ardentia.el
;; Keywords: outlines, tools
;; Version: 0.1
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

;; ardentia shows the agent queues (:aiready: -> :ai_in_progress: ->
;; :humanreview:) in Emacs, lets you review, accept or send back results,
;; and creates new agent tasks.  Reads go through the `org-tool' CLI,
;; writes happen in the visiting org buffer.  See README.md.
;;
;; Entry point: `M-x ardentia'.

;;; Code:

(require 'transient)
(require 'ardentia-core)
(require 'ardentia-actions)
(require 'ardentia-review)
(require 'ardentia-session)
(require 'ardentia-dashboard)
(require 'ardentia-capture)
(require 'ardentia-modeline)

(defun ardentia-refresh-dashboards ()
  "Refresh all ardentia dashboards asynchronously."
  (interactive)
  (ardentia-dashboard-refresh-all))

;;;###autoload (autoload 'ardentia "ardentia" nil t)
(transient-define-prefix ardentia ()
  "Dispatch menu of the agent team."
  [["View"
    ("d" "Dashboard" ardentia-dashboard)
    ("r" "Review inbox" ardentia-review-inbox)
    ("g" "Refresh" ardentia-refresh-dashboards)
    ("m" "Toggle mode-line counter" ardentia-mode-line-mode)]
   ["Create"
    ("n" "New task" ardentia-new-task)
    ("D" "Delegate region/buffer" ardentia-delegate)]])

(provide 'ardentia)
;;; ardentia.el ends here

;;; ardentia-model.el --- Pick the model of an agent task for ardentia -*- lexical-binding: t -*-

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

;; The property `ardentia-model-property' (AI_MODEL_ORCHESTRATOR) on a task sets the
;; model the runner starts the task's orchestrator with.  This file offers
;; the models that are available right now: the aliases in
;; `ardentia-model-aliases' plus the model IDs the models endpoint
;; returns for the OAuth login of `ardentia-usage-credentials-file'.  The
;; token is handed to curl on its standard input, as in ardentia-usage.
;; Without network or login only the aliases are offered.
;;
;; `ardentia-set-model' sets or removes the property on the heading at
;; point; `ardentia-dashboard-set-model' on the task at point of the
;; dashboard.  `org-set-property' completes the same values through
;; `org-property-allowed-value-functions'.

;;; Code:

(require 'org)
(require 'seq)
(require 'subr-x)
(require 'ardentia-core)
(require 'ardentia-actions)
(require 'ardentia-usage)

(defcustom ardentia-model-property "AI_MODEL_ORCHESTRATOR"
  "Property that names the model of an agent task."
  :type 'string
  :group 'ardentia)

(defcustom ardentia-model-aliases '("haiku" "sonnet" "opus" "fable")
  "Model aliases offered before the fetched model IDs.
An alias always means the latest model of that family."
  :type '(repeat string)
  :group 'ardentia)

(defcustom ardentia-model-url "https://api.anthropic.com/v1/models?limit=100"
  "Endpoint that lists the available models."
  :type 'string
  :group 'ardentia)

(defcustom ardentia-model-cache-seconds 3600
  "Seconds a fetched model list is reused before it is fetched again."
  :type 'integer
  :group 'ardentia)

(defvar ardentia-model--cache nil
  "Cons (TIME . MODELS) of the last successful fetch.
MODELS is a list of (ID . DISPLAY-NAME).")

(defun ardentia-model--parse (json)
  "Return the (ID . DISPLAY-NAME) list of the models endpoint JSON, or nil."
  (ignore-errors
    (let ((data (ardentia-usage--get
                 (json-parse-string json :object-type 'alist :array-type 'list
                                    :null-object nil :false-object nil)
                 'data)))
      (delq nil
            (mapcar (lambda (m)
                      (let ((id (ardentia-usage--get m 'id)))
                        (when (stringp id)
                          (cons id (let ((n (ardentia-usage--get m 'display_name)))
                                     (and (stringp n) n))))))
                    data)))))

(defun ardentia-model--fetch ()
  "Fetch the model list synchronously; return it, or nil on any failure."
  (let ((token (ardentia-usage--token))
        (curl (executable-find "curl")))
    (when (and token curl)
      (with-temp-buffer
        (let ((status (ignore-errors
                        (call-process-region
                         (format "Authorization: Bearer %s\nanthropic-version: 2023-06-01\nanthropic-beta: oauth-2025-04-20\n"
                                 token)
                         nil curl nil t nil
                         "-sS" "-m" "10" "-f" "-H" "@-" ardentia-model-url))))
          (and (eql status 0) (ardentia-model--parse (buffer-string))))))))

(defun ardentia-model-list (&optional force)
  "Return the available models as a list of (ID . DISPLAY-NAME).
Use the cache unless it is older than `ardentia-model-cache-seconds' or
FORCE is non-nil.  A failed fetch returns the stale cache, if any."
  (if (and (not force) ardentia-model--cache
           (< (float-time (time-subtract nil (car ardentia-model--cache)))
              ardentia-model-cache-seconds))
      (cdr ardentia-model--cache)
    (let ((models (ardentia-model--fetch)))
      (if models
          (cdr (setq ardentia-model--cache (cons (current-time) models)))
        (cdr ardentia-model--cache)))))

(defun ardentia-model-candidates (&optional force)
  "Return the values offered for the model property, aliases first.
FORCE is passed to `ardentia-model-list'."
  (delete-dups (append ardentia-model-aliases
                       (mapcar #'car (ardentia-model-list force)))))

(defun ardentia-model-read (&optional current force)
  "Read a model with completion; empty input means none.
CURRENT is the value now set, shown in the prompt.  FORCE refetches the
list.  Return the chosen string, or nil for none."
  (let* ((models (ardentia-model-list force))
         (cands (ardentia-model-candidates))
         (annotate (lambda (c)
                     (let ((name (cdr (assoc c models))))
                       (cond (name (concat "  " name))
                             ((member c ardentia-model-aliases)
                              "  latest of the family")))))
         (choice (completing-read
                  (format "Model%s (empty = frontmatter default): "
                          (if current (format " [now %s]" current) ""))
                  (lambda (str pred action)
                    (if (eq action 'metadata)
                        `(metadata (annotation-function . ,annotate)
                                   (display-sort-function . identity))
                      (complete-with-action action cands str pred)))
                  nil nil)))
    (unless (string-empty-p (string-trim choice)) (string-trim choice))))

(defun ardentia-model--apply (model)
  "Set the model property of the heading at point to MODEL; nil removes it."
  (if model
      (org-entry-put nil ardentia-model-property model)
    (org-entry-delete nil ardentia-model-property)))

;;;###autoload
(defun ardentia-set-model (&optional force)
  "Set or remove the model property of the org heading at point.
With a prefix argument FORCE refetch the model list."
  (interactive "P")
  (unless (derived-mode-p 'org-mode) (user-error "Not in an org buffer"))
  (org-back-to-heading t)
  (let ((model (ardentia-model-read
                (org-entry-get nil ardentia-model-property) force)))
    (ardentia-model--apply model)
    (message (if model "%s: %s" "%s removed")
             ardentia-model-property model)))

(defun ardentia-set-model-1 (task model)
  "Set the model property of TASK (a task plist) to MODEL; nil removes it."
  (ardentia--call-on-task task (lambda () (ardentia-model--apply model))))

(defun ardentia-model-allowed-values (property)
  "Offer the models when PROPERTY is the model property.
For `org-property-allowed-value-functions'."
  (when (equal property ardentia-model-property)
    ;; ":ETC" lets org accept a model ID that is not in the list.
    (append (ardentia-model-candidates) '(":ETC"))))

(add-hook 'org-property-allowed-value-functions #'ardentia-model-allowed-values)

(provide 'ardentia-model)
;;; ardentia-model.el ends here

;;; gpt.el --- Support for the GPT API  -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Dominik Martinez


;; Author: Dominik Martinez <dominikmartinez@pm.me>
;; Maintainer: Dominik Martinez <dominikmartinez@pm.me>
;; Created: 27 May 2023

;; Version: 0.0.1
;; Keywords: ai
;; URL:


;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <https://www.gnu.org/licenses/>.

;;; Code:

(defgroup gpt nil
  "Interact with the GPT API")

(defcustom gpt-system-message
  "You are an assistant built into Emacs that only responds in Emacs Lisp, never respond with plain text.
When you need additional information from the user, you can continue the conversation with the function (gpt--talk \"message from assistant\"). 
Otherwise, you should default to using the function (message \"message from assistant\") for other communications.
This code will be running in the Emacs Lisp evaluator.
Try to avoid defining new functions.
If the user responds with the debugger output, do not respond in plain text, just output the correct code.
Some messages may include the text of the current buffer. When that is the case, there will be a \"BUFFER TEXT\" heading and a \"USER MESSAGE\" heading."
  "The system message sent to GPT"
  :type 'string
  :group 'gpt)

(defcustom gpt-model "gpt-4"
  "ID of the chat model to use"
  :type 'string
  :options '("gpt-4" "gpt-4-32k" "gpt-3.5-turbo")
  :group 'gpt)

(defcustom gpt-temperature 0.2
  "The sampling temperature to use"
  :type 'number
  :group 'gpt)

(defcustom gpt-api-key nil "The OpenAI api key"
  :type 'string
  :group 'gpt)

(defconst gpt-chat-default `(((role . system) (content . ,gpt-system-message))))

(defvar gpt-chat gpt-chat-default)

(defvar gpt--waiting nil)

(defun gpt--add-user-chat (message)
  (setq gpt-chat (cons `((role . user) (content . ,(format "%s\nRespond only with Emacs Lisp." message))) gpt-chat)))

(defun gpt--add-gpt-chat (message)
  (setq gpt-chat (cons `((role . assistant) (content . ,message)) gpt-chat)))

(defun gpt--get-request ()
  `(("model" . ,gpt-model) ("temperature" . ,gpt-temperature) ("messages" . ,(reverse gpt-chat))))

(defun gpt--eval-next-form (content next-pos)
  (if (< next-pos (length content))
      (let* ((form-pos (read-from-string content next-pos))
	     (form (car form-pos))
	     (pos (cdr form-pos)))
	(eval form)
	(gpt--eval-next-form content pos))))

(defun gpt--eval-last-gpt-chat ()
  (let* ((last-message (car gpt-chat))
	 (role (cdr (assoc 'role last-message)))
	 (content (cdr (assoc 'content last-message))))
    (if (eq role 'assistant)
	(gpt--eval-next-form content 0))))

(defun gpt--mode-line-format ()
  (if gpt--waiting
      "Waiting for GPT response..."
    "Received messasge from GPT!"))

(defun gpt--add-mode-line ()
  (or (memq 'gpt--mode-line-format mode-line-misc-info)
      (add-to-list 'mode-line-misc-info '(:eval (gpt--mode-line-format))))
  (force-mode-line-update))

(defun gpt--remove-mode-line ()
  (setq mode-line-misc-info (delete '(:eval (gpt--mode-line-format)) mode-line-misc-info))
  (force-mode-line-update))
 
(defun gpt--message-with-buffer (message)
  (format "BUFFER TEXT\n%s\n\nUSER MESSAGE\n%s" (buffer-substring-no-properties (point-min) (point-max)) message))

(defun gpt--send-request ()
  (request
    "https://api.openai.com/v1/chat/completions"
    :type "POST"
    :headers `(("Authorization" . ,(format "Bearer %s" gpt-api-key)) ("Content-Type" . "application/json"))
    :data (json-encode (gpt--get-request))
    :parser 'json-read
    :error
    (cl-function
     (lambda (&key response &allow-other-keys)
       (message (pp response))))
    :success
    (cl-function
     (lambda (&key data &allow-other-keys)
       (setq gpt--waiting nil)
       (force-mode-line-update)
       (run-at-time 2 nil (lambda ()
			    (gpt--remove-mode-line)))
       (let* ((choice (aref (cdr (assoc 'choices data)) 0))
	      (message (cdr (assoc 'message choice)))
	      (content (cdr (assoc 'content message))))
	 (gpt--add-gpt-chat content)
	 (condition-case nil
	     (gpt--eval-last-gpt-chat)
	   (error nil))))))
  (setq gpt--waiting t)
  (gpt--add-mode-line))

(defun gpt--talk (gpt-prompt)
  (let ((message (read-string gpt-prompt)))
    (gpt--add-user-chat message)
    (gpt--send-request)))

(defun gpt-message ()
  (interactive)
  (unless gpt-api-key
    (setq gpt-api-key (read-string "Enter OpenAI API key: ")))
  (let ((message (read-string "Enter message: ")))
    (gpt--add-user-chat message)
    (gpt--send-request)))

(defun gpt-message-buffer ()
  (interactive)
  (unless gpt-api-key
    (setq gpt-api-key (read-string "Enter OpenAI API key: ")))
  (let ((message (read-string "Enter message: ")))
    (gpt--add-user-chat (gpt--message-with-buffer message))
    (gpt--send-request)))

(defun gpt-reset ()
  (interactive)
  (setq gpt-chat gpt-chat-default))

(provide 'gpt)

;;; my-gpt.el --- Minimal example to interact with OpenAI GPT-4 -*- lexical-binding: t; -*-

;; Author: Your Name
;; Version: 0.1
;; Package-Requires: ((emacs "27.1") (request "0.3.3"))
;; Keywords: convenience, gpt, api

;;; Commentary:

;; This package provides a minimal example to send a request to OpenAI's GPT-4 API
;; and display the response in a temporary buffer.

;;; Code:
(require 'json)
(require 'url)
(require 'request)
(require 'auth-source)

(defvar my/gpt-api-url "https://api.openai.com/v1/chat/completions"
  "The OpenAI API endpoint for completions.")

(defun my/gpt-get-api-key ()
  "Retrieve the OpenAI API key from the authinfo.gpg file."
  (let ((auth-info (auth-source-search
                    :host "api.openai.com"
                    :user "org-ai"
                    :require '(:secret))))
    (if auth-info
        (let ((api-key (funcall (plist-get (car auth-info) :secret))))
          api-key)
      (error "No API key found for api.openai.com"))))

;; test 
;; (my/gpt-get-api-key)
;; OK

(defun my/gpt-send-region-with-prompt (begin end)
  "Send the selected region and a user prompt to GPT-4 and display the response."
  (interactive "r")
  (let* ((context (buffer-substring-no-properties begin end))
         (prompt (read-string "Enter your prompt: "))
         (api-key (my/gpt-get-api-key))
         (data (json-encode 
                `(("model" . "gpt-4")
                  ("messages" . [((role . "system")
                                  (content . "You are a helpful assistant. Use the provided context to answer the user's question."))
                                 ((role . "user")
                                  (content . ,(format "Context:\n%s\n\nQuestion: %s" context prompt)))])
                  ("max_tokens" . 1000))))
         (url-request-method "POST")
         (url-request-extra-headers
          `(("Content-Type" . "application/json")
            ("Authorization" . ,(format "Bearer %s" api-key))))
         (url-request-data data))
    
    (url-retrieve 
     my/gpt-api-url
     (lambda (status)
       (if-let ((error-status (plist-get status :error)))
           (message "Error: %S" error-status)
         (goto-char (point-min))
         (re-search-forward "^$")
         (let* ((json-object-type 'hash-table)
                (json-array-type 'vector)
                (json-key-type 'string)
                (response (json-read))
                (response-text (gethash "content" 
                                        (gethash "message"
                                                 (aref (gethash "choices" response) 0)))))
           (with-current-buffer (get-buffer-create "*GPT-4 Response*")
             (erase-buffer)
             (insert (format "Context:\n%s\n\n" context))
             (insert (format "Prompt: %s\n\n" prompt))
             (insert (format "Response:\n%s\n" response-text))
             (pop-to-buffer (current-buffer)))))))))


(provide 'my-gpt)
;;; my-gpt.el ends here


;;; my-gpt-openai.el --- OpenAI GPT specific functions -*- lexical-binding: t; -*-

;;; Commentary:

;; This file contains functions specific to interacting with OpenAI's GPT API.

;;; Code:

(require 'my-gpt-utils)

(defvar my-gpt-openai-api-url "https://api.openai.com/v1/chat/completions"
  "The OpenAI API endpoint for completions.")

(defun my-gpt-openai-get-api-key ()
  "Retrieve the OpenAI API key from the authinfo.gpg file."
  (my-gpt-get-api-key "api.openai.com" "org-ai"))

(defun my-gpt-openai-send-region-with-prompt (begin end)
  "Send the selected region and a user prompt to GPT-4 and display the response."
  (interactive "r")
  (let* ((context (buffer-substring-no-properties begin end))
         (prompt (read-string "Enter your prompt for GPT-4: "))
         (api-key (my-gpt-openai-get-api-key))
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
    
    (my-gpt-send-request my-gpt-openai-api-url "*GPT-4 Response*" context prompt)))

(provide 'my-gpt-openai)

;;; my-gpt-openai.el ends here

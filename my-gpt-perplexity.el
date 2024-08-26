;;; my-gpt-perplexity.el --- Perplexity AI specific functions -*- lexical-binding: t; -*-

;;; Commentary:

;; This file contains functions specific to interacting with Perplexity AI API.

;;; Code:

(require 'my-gpt-utils)

(defvar my-gpt-perplexity-api-url "https://api.perplexity.ai/chat/completions"
  "The Perplexity AI API endpoint for completions.")

(defun my-gpt-perplexity-get-api-key ()
  "Retrieve the Perplexity AI API key from the authinfo.gpg file."
  (my-gpt-get-api-key "api.perplexity.ai" "org-ai"))

(defun my-gpt-perplexity-send-region-with-prompt (begin end)
  "Send the selected region and a user prompt to Perplexity AI and display the response."
  (interactive "r")
  (let* ((context (buffer-substring-no-properties begin end))
         (prompt (read-string "Enter your prompt for Perplexity AI: "))
         (api-key (my-gpt-perplexity-get-api-key))
         (data (json-encode 
                `(("model" . "llama-3.1-sonar-small-128k-online")
                  ("messages" . [((role . "system")
                                  (content . "Be precise and concise."))
                                 ((role . "user")
                                  (content . ,(format "Context:\n%s\n\nQuestion: %s" context prompt)))]))))
         (url-request-method "POST")
         (url-request-extra-headers
          `(("Content-Type" . "application/json")
            ("accept" . "application/json")
            ("Authorization" . ,(format "Bearer %s" api-key))))
         (url-request-data data))
    
    (my-gpt-send-request my-gpt-perplexity-api-url "*Perplexity AI Response*" context prompt)))

(provide 'my-gpt-perplexity)

;;; my-gpt-perplexity.el ends here

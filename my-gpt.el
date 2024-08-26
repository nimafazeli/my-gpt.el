;;; my-gpt.el --- Interact with various AI APIs -*- lexical-binding: t; -*-

;; Author: Your Name
;; Version: 0.4
;; Package-Requires: ((emacs "27.1") (request "0.3.3"))
;; Keywords: convenience, ai, api
;; URL: https://github.com/yourusername/my-gpt

;;; Commentary:

;; This package provides functions to interact with various AI APIs
;; such as OpenAI's GPT and Perplexity AI.

;;; Code:

(require 'my-gpt-utils)
(require 'my-gpt-openai)
(require 'my-gpt-perplexity)

(defgroup my-gpt nil
  "Customization group for my-gpt package."
  :group 'convenience
  :prefix "my-gpt-")

(defcustom my-gpt-default-service 'openai
  "The default AI service to use."
  :type '(choice (const :tag "OpenAI GPT" openai)
                 (const :tag "Perplexity AI" perplexity))
  :group 'my-gpt)

(defun my-gpt-send-region-with-prompt (begin end)
  "Send the selected region and a user prompt to the default AI service."
  (interactive "r")
  (pcase my-gpt-default-service
    ('openai (my-gpt-openai-send-region-with-prompt begin end))
    ('perplexity (my-gpt-perplexity-send-region-with-prompt begin end))
    (_ (error "Unknown AI service"))))

(provide 'my-gpt)

;;; my-gpt.el ends here

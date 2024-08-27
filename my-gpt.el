;;; my-gpt.el --- Interact with various AI APIs -*- lexical-binding: t; -*-

;;; Commentary:
;; This package provides integration between Org-mode and AI language models
;; (currently OpenAI's GPT and Perplexity AI).

;;; Code:

(require 'json)
(require 'url)
(require 'auth-source)
(require 'ob)

(defgroup my-gpt nil
  "Customization group for my-gpt package."
  :group 'applications)

(defcustom my-gpt-openai-api-url "https://api.openai.com/v1/chat/completions"
  "The OpenAI API endpoint for completions."
  :type 'string
  :group 'my-gpt)

(defcustom my-gpt-perplexity-api-url "https://api.perplexity.ai/chat/completions"
  "The Perplexity AI API endpoint for completions."
  :type 'string
  :group 'my-gpt)

(defcustom my-gpt-openai-model "gpt-4"
  "The default model to use for OpenAI API requests."
  :type 'string
  :group 'my-gpt)

(defcustom my-gpt-perplexity-model "llama-3.1-sonar-small-128k-online"
  "The default model to use for Perplexity API requests."
  :type 'string
  :group 'my-gpt)

(defvar my-gpt-sessions (make-hash-table :test 'equal)
  "Hash table to store session data.")

(defun my-gpt-get-api-key (host user)
  "Retrieve the API key for HOST and USER from the authinfo.gpg file."
  (let ((auth-info (auth-source-search :host host :user user :require '(:secret))))
    (if auth-info
        (funcall (plist-get (car auth-info) :secret))
      (error "No API key found for %s" host))))

(defun my-gpt-send-request (url headers data)
  "Send request to URL with HEADERS and DATA, and return the response."
  (let ((url-request-method "POST")
        (url-request-extra-headers headers)
        (url-request-data data))
    (with-current-buffer (url-retrieve-synchronously url)
      (set-buffer-multibyte t)
      (goto-char (point-min))
      (re-search-forward "^$")
      (let* ((json-object-type 'hash-table)
             (json-array-type 'vector)
             (json-key-type 'string)
             (json-string (decode-coding-string 
                           (buffer-substring-no-properties (point) (point-max))
                           'utf-8)))
        (json-read-from-string json-string)))))

(defun my-gpt-extract-content (response)
  "Extract content from RESPONSE."
  (let* ((choices (gethash "choices" response))
         (first-choice (aref choices 0))
         (message (gethash "message" first-choice)))
    (gethash "content" message)))

(defun my-gpt-format-messages (body session)
  "Format messages using BODY and SESSION."
  (let ((messages (gethash session my-gpt-sessions)))
    (vconcat
     messages
     (vector `((role . "user") (content . ,body))))))

(defun my-gpt-send (service body session)
  "Send BODY to SERVICE using SESSION, and return the response."
  (let* ((api-key (my-gpt-get-api-key (if (eq service 'openai) "api.openai.com" "api.perplexity.ai") "org-ai"))
         (url (if (eq service 'openai) my-gpt-openai-api-url my-gpt-perplexity-api-url))
         (model (if (eq service 'openai) my-gpt-openai-model my-gpt-perplexity-model))
         (headers `(("Content-Type" . "application/json")
                    ("Authorization" . ,(format "Bearer %s" api-key))))
         (messages (my-gpt-format-messages body session))
         (data (json-encode `(("model" . ,model)
                              ("messages" . ,messages)))))
    (let* ((response (my-gpt-send-request url headers data))
           (content (my-gpt-extract-content response)))
      (puthash session
               (vconcat messages `[((role . "assistant") (content . ,content))])
               my-gpt-sessions)
      content)))

(defun org-babel-execute:my-gpt (body params)
  "Execute a block of my-gpt code with org-babel."
  (let* ((service (intern (or (cdr (assq :service params)) "openai")))
         (session (or (cdr (assq :session params)) "default"))
         (result (my-gpt-send service body session)))
    result))

(provide 'my-gpt)

;;; my-gpt.el ends here

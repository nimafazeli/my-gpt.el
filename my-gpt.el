;;; my-gpt.el --- Interact with AI APIs through Org-mode -*- lexical-binding: t; -*-

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

(defun my-gpt-get-buffer-messages (session)
  "Retrieve conversation history for SESSION from the current buffer."
  (let ((messages [])
        (case-fold-search t))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^#\\+begin_src\\s-+my-gpt\\(.*\\)$" nil t)
        (let ((src-block-start (point))
              (params (org-babel-parse-header-arguments (match-string 1))))
          (when (string= (cdr (assq :session params)) session)
            (let ((body (progn
                          (forward-line)
                          (buffer-substring-no-properties (point) (progn (search-forward "#+end_src" nil t) (line-beginning-position))))))
              (push `((role . "user") (content . ,body)) messages)
              (forward-line)
              (when (looking-at "^#\\+RESULTS:")
                (forward-line)
                (let ((result (buffer-substring-no-properties (point) (progn (search-forward "#+end_src" nil t) (line-beginning-position)))))
                  (push `((role . "assistant") (content . ,result)) messages))))))))
    (vconcat (nreverse messages))))

(defun my-gpt-format-messages (body session system-content)
  "Format messages using BODY, SESSION, and SYSTEM-CONTENT."
  (let ((messages (my-gpt-get-buffer-messages session)))
    (vconcat
     (when system-content
       (vector `((role . "system") (content . ,system-content))))
     messages
     (vector `((role . "user") (content . ,body))))))

(defun my-gpt-send (service body session system-content)
  "Send BODY to SERVICE using SESSION and SYSTEM-CONTENT, and return the response."
  (let* ((api-key (my-gpt-get-api-key (if (eq service 'openai) "api.openai.com" "api.perplexity.ai") "org-ai"))
         (url (if (eq service 'openai) my-gpt-openai-api-url my-gpt-perplexity-api-url))
         (model (if (eq service 'openai) my-gpt-openai-model my-gpt-perplexity-model))
         (headers `(("Content-Type" . "application/json")
                    ("Authorization" . ,(format "Bearer %s" api-key))))
         (messages (my-gpt-format-messages body session system-content))
         (data (json-encode `(("model" . ,model)
                              ("messages" . ,messages)))))
    (let* ((response (my-gpt-send-request url headers data))
           (content (my-gpt-extract-content response)))
      content)))

(defun org-babel-execute:my-gpt (body params)
  "Execute a block of my-gpt code with org-babel."
  (let* ((service (intern (or (cdr (assq :service params)) "openai")))
         (session (or (cdr (assq :session params)) "default"))
         (system-content
          (or
           ;; First, check for a :system header argument
           (cdr (assq :system params))
           ;; If not found, look for a SYSTEM property, including inherited ones
           (org-entry-get nil "SYSTEM" t)
           ;; If still not found, use a default directive
           "You are a helpful assistant."))
         (expanded-body (org-babel-expand-body:my-gpt body params))
         (result (my-gpt-send service expanded-body session system-content)))
    result))

(defun org-babel-expand-body:my-gpt (body params)
  "Expand BODY according to PARAMS, return the expanded body."
  (org-babel-expand-noweb-references
   (list "my-gpt" body params)))

(add-to-list 'org-babel-load-languages '(my-gpt . t))

(provide 'my-gpt)

;;; my-gpt.el ends here

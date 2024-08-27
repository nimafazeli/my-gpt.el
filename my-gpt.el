;;; my-gpt.el --- Interact with various AI APIs -*- lexical-binding: t; -*-

(require 'json)
(require 'url)
(require 'auth-source)
(require 'ob)

(defvar my-gpt-openai-api-url "https://api.openai.com/v1/chat/completions"
  "The OpenAI API endpoint for completions.")

(defvar my-gpt-perplexity-api-url "https://api.perplexity.ai/chat/completions"
  "The Perplexity AI API endpoint for completions.")

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
      (goto-char (point-min))
      (re-search-forward "^$")
      (let ((json-object-type 'hash-table)
            (json-array-type 'vector)
            (json-key-type 'string)
            (json-string (buffer-substring-no-properties (point) (point-max))))
        (json-read-from-string json-string)))))

(defun my-gpt-extract-content (response service)
  "Extract content from RESPONSE based on SERVICE."
  (let* ((choices (gethash "choices" response))
         (first-choice (aref choices 0))
         (message (gethash "message" first-choice))
         (content (gethash "content" message)))
    content))

(defun my-gpt-format-messages (service body session)
  "Format messages for SERVICE using BODY and SESSION."
  (if (eq service 'perplexity)
      (vector
       `((role . "user")
         (content . [((type . "text") (text . ,body))])))
    (vector
     `((role . "user")
       (content . ,body)))))

(defun my-gpt-send (service model body &optional session)
  "Send BODY to SERVICE using MODEL and SESSION, and return the response."
  (let* ((api-key (my-gpt-get-api-key (if (eq service 'openai) "api.openai.com" "api.perplexity.ai") "org-ai"))
         (url (if (eq service 'openai) my-gpt-openai-api-url my-gpt-perplexity-api-url))
         (headers `(("Content-Type" . "application/json")
                    ("Authorization" . ,(format "Bearer %s" api-key))))
         (messages (my-gpt-format-messages service body session))
         (data (json-encode `(("model" . ,model)
                              ("messages" . ,messages))))
         (retries 3))
    (message "Request data: %s" data)  ; Debug output
    (while (> retries 0)
      (condition-case err
          (let ((response (my-gpt-send-request url headers data)))
            (message "API Response: %S" response)
            (return-from my-gpt-send (my-gpt-extract-content response service)))
        (error
         (setq retries (1- retries))
         (message "Error occurred: %S. Retries left: %d" err retries)
         (when (= retries 0)
           (error "Failed to get response after multiple attempts: %S" err)))))))

(defun org-babel-execute:my-gpt (body params)
  "Execute a block of my-gpt code with org-babel."
  (let* ((service (intern (or (cdr (assq :service params)) "openai")))
         (model (or (cdr (assq :model params)) 
                    (if (eq service 'openai) "gpt-4" "llama-3.1-sonar-small-128k-online")))
         (session (cdr (assq :session params)))
         (result (my-gpt-send service model body session)))
    result))

(add-to-list 'org-babel-load-languages '(my-gpt . t))

(provide 'my-gpt)
;;; my-gpt.el ends here

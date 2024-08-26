;;; my-gpt-utils.el --- Utility functions for my-gpt package -*- lexical-binding: t; -*-

;;; Commentary:

;; This file contains utility functions used across the my-gpt package.

;;; Code:

(require 'json)
(require 'url)
(require 'auth-source)

(defun my-gpt-get-api-key (host user)
  "Retrieve the API key for HOST and USER from the authinfo.gpg file."
  (let ((auth-info (auth-source-search
                    :host host
                    :user user
                    :require '(:secret))))
    (if auth-info
        (let ((api-key (funcall (plist-get (car auth-info) :secret))))
          api-key)
      (error "No API key found for %s" host))))

(defun my-gpt-send-request (url buffer-name context prompt)
  "Send request to URL and display response in BUFFER-NAME with CONTEXT and PROMPT."
  (url-retrieve 
   url
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
         (with-current-buffer (get-buffer-create buffer-name)
           (erase-buffer)
           (insert (format "Context:\n%s\n\n" context))
           (insert (format "Prompt: %s\n\n" prompt))
           (insert (format "Response:\n%s\n" response-text))
           (pop-to-buffer (current-buffer))))))))

(provide 'my-gpt-utils)

;;; my-gpt-utils.el ends here

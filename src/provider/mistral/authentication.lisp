(in-package #:autolith)


;;;; -- Mistral API Key Authentication --

(define-static-api-key-provider mistral
  :display-name "Mistral"
  :environment-variable "MISTRAL_API_KEY")


;;;; -- Mistral API Key Validation --

(-> mistral-validate-api-key (string) null)
(defun mistral-validate-api-key (key)
  "Probe the configured Mistral models endpoint with KEY."
  (api-key-validate-probe
   "Mistral"
   (lambda ()
     (provider-call-with-response-deadline
      60
      (lambda ()
        (dexador:get
         (mistral-models-endpoint)
         :headers (list (cons "Authorization" (concatenate 'string "Bearer " key)))
         :force-string t
         :keep-alive nil
         :connect-timeout 30
         :read-timeout 60))))))

(-> mistral-api-key-login
    (mistral-credential-manager &key (:stream stream))
    string)
(defun mistral-api-key-login (manager &key (stream *standard-output*))
  "Prompt for, validate, and store the Mistral API key."
  (api-key-login manager
                 :stream stream
                 :validate #'mistral-validate-api-key))

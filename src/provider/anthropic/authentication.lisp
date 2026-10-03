(in-package #:autolith)


;;;; -- Anthropic API Key Authentication --

(define-static-api-key-provider anthropic
  :display-name "Anthropic"
  :environment-variable "ANTHROPIC_API_KEY")


;;;; -- Anthropic API Key Validation --

(-> anthropic-validate-api-key (string) null)
(defun anthropic-validate-api-key (key)
  "Probe the Anthropic models endpoint with KEY, signaling on rejection."
  (api-key-validate-probe
   "Anthropic"
   (lambda ()
     (provider-call-with-response-deadline
      60
      (lambda ()
        (dexador:get
         *anthropic-models-endpoint*
         :headers (list (cons "x-api-key" key)
                        (cons "anthropic-version" *anthropic-api-version*)
                        (cons "User-Agent" (provider-user-agent)))
         :force-string t
         :keep-alive nil
         :connect-timeout 30
         :read-timeout 60))))))

(-> anthropic-api-key-login
    (anthropic-credential-manager &key (:stream t))
    string)
(defun anthropic-api-key-login (manager &key (stream *standard-output*))
  "Prompt for, validate, and store the Anthropic API key."
  (api-key-login manager
                 :stream stream
                 :validate #'anthropic-validate-api-key))

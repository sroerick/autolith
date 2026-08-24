(in-package #:autolith)

;;;; -- OpenRouter Provider Tests --

(-> openrouter-provider-test--configuration
    (&key
     (:model non-empty-string)
     (:reasoning-effort non-empty-string)
     (:provider-endpoint non-empty-string))
    configuration)
(defun openrouter-provider-test--configuration
    (&key
       (model (openrouter--model-name "openai/gpt-5-mini"))
       (reasoning-effort *default-reasoning-effort*)
       (provider-endpoint *openrouter-chat-completions-endpoint*))
  "Return an isolated OpenRouter test configuration."
  (let ((base (test-configuration)))
    (make-instance 'configuration
                   :source-root (configuration-source-root base)
                   :working-directory (configuration-working-directory base)
                   :config-root (configuration-config-root base)
                   :data-root (configuration-data-root base)
                   :state-root (configuration-state-root base)
                   :cache-root (configuration-cache-root base)
                   :codex-auth-path (configuration-codex-auth-path base)
                   :grok-bootstrap-auth-path
                   (configuration-grok-bootstrap-auth-path base)
                   :model model
                   :reasoning-effort reasoning-effort
                   :provider-endpoint provider-endpoint)))

(-> openrouter-provider-test--restore-environment
    (string (option string))
    null)
(defun openrouter-provider-test--restore-environment (name value)
  "Restore environment variable NAME to VALUE."
  (if value
      (sb-posix:setenv name value 1)
      (sb-posix:unsetenv name))
  nil)

(-> openrouter-provider-test--credentials () null)
(defun openrouter-provider-test--credentials ()
  "Test OpenRouter environment precedence and private-store fallback."
  (let* ((configuration (openrouter-provider-test--configuration))
         (root (test-configuration-root configuration))
         (saved (uiop:getenv *openrouter-environment-variable*)))
    (unwind-protect
         (progn
           (setf (uiop:getenv *openrouter-environment-variable*)
                 "openrouter-environment-key")
           (let ((manager (openrouter-credential-manager-create configuration)))
             (api-key-credential-manager-save-key manager "openrouter-private-key"))
           (let* ((manager (openrouter-credential-manager-create configuration))
                  (loaded (credential-manager-load manager)))
             (test-assert
              (string= (oauth-credentials-access-token loaded)
                       "openrouter-environment-key")
              "OpenRouter environment credentials take precedence"))
           (sb-posix:unsetenv *openrouter-environment-variable*)
           (let* ((manager (openrouter-credential-manager-create configuration))
                  (loaded (credential-manager-load manager)))
             (test-assert
              (string= (oauth-credentials-access-token loaded)
                       "openrouter-private-key")
              "OpenRouter uses its private key when the environment is absent")))
      (openrouter-provider-test--restore-environment
       *openrouter-environment-variable* saved)
      (uiop:delete-directory-tree root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> openrouter-provider-test--model-decoding () null)
(defun openrouter-provider-test--model-decoding ()
  "Test OpenRouter model filtering and user-visible namespacing."
  (let ((body
          (json-encode
           (json-object
            "data"
            (json-array
             (json-object
              "id" "openai/gpt-5-mini"
              "architecture"
              (json-object "output_modalities" (json-array "text"))
              "supported_parameters"
              (json-array "tools" "tool_choice" "reasoning"))
             (json-object
              "id" "vendor/no-tools"
              "architecture"
              (json-object "output_modalities" (json-array "text"))
              "supported_parameters" (json-array "temperature"))
             (json-object
              "id" "vendor/image-only"
              "architecture"
              (json-object "output_modalities" (json-array "image"))
              "supported_parameters" (json-array "tools" "tool_choice")))))))
    (test-assert
     (equal
      (mapcar #'openrouter--model-name
              (openai-compatible--decode-model-list
               body
               :entry-predicate #'openrouter--chat-tool-model-p))
      '("openrouter/openai/gpt-5-mini"))
      "OpenRouter discovery requires text, tools, and normalized reasoning"))
  (test-assert
   (string= (openrouter--wire-model-name
             "openrouter/anthropic/claude-haiku-4.5")
            "anthropic/claude-haiku-4.5")
   "OpenRouter preserves the upstream vendor and model slug on the wire")
  (test-assert
   (handler-case
       (progn
         (openrouter--wire-model-name "openai/gpt-5-mini")
         nil)
     (configuration-error () t))
   "OpenRouter rejects an unnamespaced local model identifier")
  nil)

(-> openrouter-provider-test--provider () null)
(defun openrouter-provider-test--provider ()
  "Test OpenRouter construction, copying, headers, and request translation."
  (let* ((configuration (openrouter-provider-test--configuration
                         :reasoning-effort "medium"))
         (root (test-configuration-root configuration))
         (provider (openrouter-provider-create configuration))
         (copy (provider-with-configuration provider configuration))
         (conversation
           (conversation-create configuration :identifier "openrouter-request"))
         (credentials
           (make-instance 'oauth-credentials
                          :access-token "synthetic-openrouter-key"
                          :account-id "openrouter"
                          :source-path #P"/tmp/openrouter-test-key")))
    (unwind-protect
         (progn
           (test-assert
            (and (typep provider 'openrouter-chat-completions-provider)
                 (eq (provider-family provider) ':openrouter)
                 (string= (provider-account-label provider) "OpenRouter"))
            "OpenRouter construction preserves its provider identity")
           (test-assert
            (and (typep copy 'openrouter-chat-completions-provider)
                 (eq (provider-credential-manager copy)
                     (provider-credential-manager provider))
                 (string= (provider-session-id copy)
                          (provider-session-id provider)))
            "OpenRouter copies retain credentials and session identity")
           (let ((headers
                   (openai-compatible--request-headers
                    provider credentials conversation)))
             (test-assert
              (and (string= (rest (assoc "HTTP-Referer" headers :test #'string=))
                            "https://github.com/lambda-symbolics/autolith")
                   (string= (rest (assoc "X-OpenRouter-Title" headers
                                         :test #'string=))
                            "Autolith"))
              "OpenRouter requests include non-secret application attribution"))
           (conversation-append-user-message conversation "Hello OpenRouter.")
           (let ((*provider-maximum-output-tokens* 321))
             (multiple-value-bind (request delivery)
                 (provider-request-object provider conversation (json-array)
                                          :goal-context nil
                                          :compaction-p nil)
               (declare (ignore delivery))
               (test-assert
                (string= (json-get request "model") "openai/gpt-5-mini")
                "OpenRouter strips its local namespace from the wire model")
               (test-assert (= (json-get request "max_tokens") 321)
                            "OpenRouter requests use max_tokens")
               (test-assert (null (json-get request "max_completion_tokens"))
                            "OpenRouter requests omit max_completion_tokens")
               (test-assert
                (string= (json-get (json-get request "reasoning") "effort")
                         "medium")
                "OpenRouter requests use its normalized reasoning object")))
           (test-assert (string= (openrouter--reasoning-effort "ultra") "max")
                        "OpenRouter maps Autolith ultra reasoning to max")
           (test-assert (null (openrouter--reasoning-effort "none"))
                        "OpenRouter omits reasoning controls when disabled"))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> openrouter-provider-test--endpoints-and-discovery () null)
(defun openrouter-provider-test--endpoints-and-discovery ()
  "Test endpoint overrides, authenticated discovery, filtering, and validation."
  (let* ((configuration (openrouter-provider-test--configuration))
         (root (test-configuration-root configuration))
         (saved-chat (uiop:getenv "AUTOLITH_OPENROUTER_PROVIDER_ENDPOINT"))
         (saved-models (uiop:getenv "AUTOLITH_OPENROUTER_MODELS_ENDPOINT"))
         (observed nil))
    (unwind-protect
         (progn
           (setf (uiop:getenv "AUTOLITH_OPENROUTER_PROVIDER_ENDPOINT")
                 "https://chat.openrouter.invalid/v1/chat/completions"
                 (uiop:getenv "AUTOLITH_OPENROUTER_MODELS_ENDPOINT")
                 "https://models.openrouter.invalid/v1/models")
           (test-assert
            (string= (configuration--provider-endpoint-for
                      "openrouter/openai/gpt-5-mini")
                     "https://chat.openrouter.invalid/v1/chat/completions")
            "OpenRouter chat endpoint overrides registered metadata")
           (test-assert
            (string= (openrouter-models-endpoint)
                     "https://models.openrouter.invalid/v1/models")
            "OpenRouter model discovery has an independent override")
           (api-key-credential-manager-save-key
            (openrouter-credential-manager-create configuration)
            "openrouter-discovery-key")
           (test-call-with-function-replacements
            (list
             (list 'dexador:get
                   (lambda (url &key headers &allow-other-keys)
                     (push (list url headers) observed)
                     (values
                      (json-encode
                       (json-object
                        "data"
                        (json-array
                         (json-object
                          "id" "google/gemini-3.5-flash"
                          "architecture"
                          (json-object "output_modalities" (json-array "text"))
                          "supported_parameters"
                          (json-array "tools" "tool_choice" "reasoning"))
                         (json-object
                          "id" "vendor/text-only"
                          "architecture"
                          (json-object "output_modalities" (json-array "text"))
                          "supported_parameters" (json-array "temperature")))))
                      200
                      nil))))
            (lambda ()
              (test-assert
               (equal (openrouter--fetch-models configuration)
                      '("openrouter/google/gemini-3.5-flash"))
               "OpenRouter fetches and namespaces compatible models")
              (openrouter-validate-api-key "openrouter-validation-key")))
           (test-assert
            (and (= (length observed) 2)
                 (every
                  (lambda (request)
                    (string= (first request)
                             "https://models.openrouter.invalid/v1/models"))
                  observed)
                 (some
                  (lambda (request)
                    (equal (rest (assoc "Authorization" (second request)
                                        :test #'string=))
                           "Bearer openrouter-discovery-key"))
                  observed)
                 (some
                  (lambda (request)
                    (equal (rest (assoc "Authorization" (second request)
                                        :test #'string=))
                           "Bearer openrouter-validation-key"))
                  observed)
                 (some
                  (lambda (request)
                    (string= (rest (assoc "X-OpenRouter-Title" (second request)
                                          :test #'string=))
                             "Autolith"))
                  observed))
            "OpenRouter discovery and validation use the configured endpoint and keys"))
      (openrouter-provider-test--restore-environment
       "AUTOLITH_OPENROUTER_PROVIDER_ENDPOINT" saved-chat)
      (openrouter-provider-test--restore-environment
       "AUTOLITH_OPENROUTER_MODELS_ENDPOINT" saved-models)
      (uiop:delete-directory-tree root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> openrouter-provider-test--builtin-registration () null)
(defun openrouter-provider-test--builtin-registration ()
  "Test the built-in OpenRouter provider registration and family dispatch."
  (let ((registration (provider-registration-find "openrouter"))
        (configuration (openrouter-provider-test--configuration)))
    (unwind-protect
         (progn
           (test-assert registration "the OpenRouter builtin registration exists")
           (test-assert
            (and (eq (provider-registration-family registration) ':openrouter)
                 (string= (provider-registration-endpoint registration)
                          *openrouter-chat-completions-endpoint*)
                 (string= (provider-registration-model-discovery-endpoint registration)
                          *openrouter-models-endpoint*)
                 (functionp (provider-registration-model-discovery registration)))
            "the OpenRouter registration declares endpoints and discovery")
           (test-assert
            (typep (provider-family-create ':openrouter configuration)
                   'openrouter-chat-completions-provider)
            "provider-family-create returns the OpenRouter provider"))
      (uiop:delete-directory-tree
       (test-configuration-root configuration)
       :validate t
       :if-does-not-exist ':ignore)))
  nil)

(-> test-openrouter-provider () null)
(defun test-openrouter-provider ()
  "Run the offline OpenRouter provider tests."
  (let ((registry-snapshot (provider--registry-snapshot)))
    (unwind-protect
         (progn
           (register-provider
            "openrouter-test"
            :family ':openrouter
            :protocol ':chat-completions
            :models '("openrouter/openai/gpt-5-mini")
            :endpoint *openrouter-chat-completions-endpoint*
            :factory #'provider--openrouter-registration-factory
            :source ':runtime)
           (openrouter-provider-test--credentials)
           (openrouter-provider-test--model-decoding)
           (openrouter-provider-test--provider)
           (openrouter-provider-test--endpoints-and-discovery)
           (openrouter-provider-test--builtin-registration))
      (provider--registry-restore registry-snapshot)))
  nil)

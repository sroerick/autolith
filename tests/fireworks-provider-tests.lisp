(in-package #:autolith)

;;;; -- Fireworks API Key Provider Tests --

(-> fireworks-provider-test--configuration () configuration)
(defun fireworks-provider-test--configuration ()
  "Return an isolated configuration selecting the Fireworks model."
  (configuration-with-model (test-configuration)
                            "accounts/fireworks/models/kimi-k3"))

(-> fireworks-provider-test--selection () null)
(defun fireworks-provider-test--selection ()
  "Test Fireworks model selection and reasoning boundaries."
  (let ((configuration (fireworks-provider-test--configuration)))
    (test-assert
     (eq (model-family (configuration-model configuration)) ':fireworks)
     "the Fireworks model selects its provider family")
    (test-assert
     (string= (configuration-provider-endpoint configuration)
              *fireworks-responses-endpoint*)
     "the Fireworks model selects its endpoint")
    (test-assert
     (= (configuration-context-window configuration) 1048576)
     "the Fireworks model selects its context window")
    (test-assert
     (string= (configuration-fireworks-wire-effort
               (configuration-with-reasoning-effort configuration "none"))
              "low")
     "Fireworks clamps reasoning at its low boundary")
    (test-assert
     (string= (configuration-fireworks-wire-effort
               (configuration-with-reasoning-effort configuration "ultra"))
              "high")
     "Fireworks clamps reasoning at its high boundary"))
  nil)

(-> fireworks-provider-test--credential-source () null)
(defun fireworks-provider-test--credential-source ()
  "Test the Fireworks environment credential source without network access."
  (let* ((configuration (fireworks-provider-test--configuration))
         (root (test-configuration-root configuration))
         (manager (fireworks-credential-manager-create configuration))
         (saved (uiop:getenv "FIREWORKS_API_KEY")))
    (unwind-protect
         (progn
           (credential-source-save
            (credential-manager-primary-source manager)
            (make-instance 'oauth-credentials
                           :access-token "saved-fireworks-key"
                           :refresh-token nil
                           :id-token nil
                           :account-id "fireworks"
                           :expires-at nil
                           :source-path
                           (configuration-fireworks-auth-path configuration)))
           (setf (uiop:getenv "FIREWORKS_API_KEY") "environment-key-a")
           (test-assert
            (string= (oauth-credentials-access-token
                      (credential-manager-load manager))
                     "environment-key-a")
            "the environment key takes precedence over the saved key")
           (setf (uiop:getenv "FIREWORKS_API_KEY") "")
           (test-assert
            (string= (oauth-credentials-access-token
                      (credential-manager-load manager))
                     "saved-fireworks-key")
            "the saved interactive key is the environment fallback"))
      (setf (uiop:getenv "FIREWORKS_API_KEY") (or saved ""))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist ':ignore)))
  nil)


(-> fireworks-provider-test--request-shape () null)
(defun fireworks-provider-test--request-shape ()
  "Test Fireworks-specific Responses request controls."
  (let* ((configuration (fireworks-provider-test--configuration))
         (root (test-configuration-root configuration)))
    (unwind-protect
         (let* ((conversation
                  (conversation-create configuration :identifier "fireworks-shape"))
                (provider (fireworks-provider-create configuration)))
           (conversation-append-user-message conversation "hello")
           (let ((request
                   (provider-request-object provider conversation (json-array))))
            (test-assert
             (string= (json-get request "model")
                      "accounts/fireworks/models/kimi-k3")
             "Fireworks requests select the configured model")
            (test-assert
             (string= (json-get (json-get request "reasoning") "effort")
                      "high")
             "Fireworks requests use the clamped reasoning effort")
            (test-assert
             (null (json-get request "include"))
             "Fireworks requests omit Codex include extensions")
            (test-assert
             (notany (lambda (item)
                       (string= (or (json-get item "type") "")
                                "additional_tools"))
                     (json-get request "input"))
              "Fireworks requests omit Responses Lite additional tools")))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> fireworks-provider-test--reasoning-omission () null)
(defun fireworks-provider-test--reasoning-omission ()
  "Test reasoning-free Fireworks models omit the reasoning object."
  (let* ((configuration
           (configuration-with-model
            (test-configuration) "accounts/fireworks/models/qwen3p7-plus"))
         (root (test-configuration-root configuration)))
    (unwind-protect
         (let* ((conversation
                  (conversation-create configuration
                                       :identifier "fireworks-qwen-shape"))
                (provider (fireworks-provider-create configuration)))
           (conversation-append-user-message conversation "hello")
           (multiple-value-bind (reasoning present-p)
               (gethash "reasoning"
                        (provider-request-object provider conversation #()))
             (declare (ignore reasoning))
             (test-assert (not present-p)
                          "reasoning-free models omit the reasoning object")))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist ':ignore)))
  nil)


(-> test-fireworks-provider () null)
(defun test-fireworks-provider ()
  "Test the Fireworks API key provider without network access."
  (fireworks-provider-test--selection)
  (fireworks-provider-test--credential-source)
  (fireworks-provider-test--request-shape)
  (fireworks-provider-test--reasoning-omission)
  nil)

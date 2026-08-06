(in-package #:autolith)

;;;; -- Fireworks API Key Provider Tests --

(-> fireworks-provider-test--configuration () configuration)
(defun fireworks-provider-test--configuration ()
  "Return an isolated configuration selecting the Fireworks model."
  (configuration-with-model (test-configuration)
                            "accounts/fireworks/models/kimi-k3"))

(-> fireworks-provider-test--selection () null)
(defun fireworks-provider-test--selection ()
  "Test Fireworks model family resolution and endpoint selection."
  (test-assert (eq (model-family "accounts/fireworks/models/kimi-k3")
                   ':fireworks)
               "accounts/fireworks identifiers resolve to the Fireworks family")
  (test-assert (eq (model-family "gpt-5.6-sol") ':codex)
               "GPT identifiers keep the Codex family")
  (test-assert (eq (model-family "grok-4.5") ':grok)
               "Grok identifiers keep the Grok family")
  (let ((configuration (fireworks-provider-test--configuration)))
    (test-assert
     (string= (configuration-provider-endpoint configuration)
              *fireworks-responses-endpoint*)
     "Fireworks configurations select the Fireworks Responses endpoint")
    (test-assert (= (configuration-context-window configuration) 1048576)
                 "kimi-k3 carries the Fireworks context window")
    (loop for (effort . wire) in '(("none" . "low")
                                   ("low" . "low")
                                   ("medium" . "medium")
                                   ("high" . "high")
                                   ("xhigh" . "high")
                                   ("max" . "high")
                                   ("ultra" . "high"))
          do (let ((selected (configuration-with-reasoning-effort
                              configuration effort)))
               (test-assert
                (string= (configuration-fireworks-wire-effort selected) wire)
                (format nil "~A effort clamps to Fireworks ~A" effort wire)))))
  nil)

(-> fireworks-provider-test--credential-source () null)
(defun fireworks-provider-test--credential-source ()
  "Test the Fireworks environment credential source without network access."
  (let ((source (make-instance 'fireworks-environment-credential-source))
        (saved (uiop:getenv "FIREWORKS_API_KEY")))
    (unwind-protect
         (progn
           (setf (uiop:getenv "FIREWORKS_API_KEY") "")
           (test-assert (null (credential-source-load source))
                        "an empty environment yields no Fireworks credentials")
           (setf (uiop:getenv "FIREWORKS_API_KEY") "test-fireworks-key")
           (let ((credentials (credential-source-load source)))
             (test-assert (typep credentials 'oauth-credentials)
                          "the environment key loads as request credentials")
             (test-assert (string= (oauth-credentials-access-token credentials)
                                   "test-fireworks-key")
                          "the API key rides as the bearer access token")
             (test-assert (null (oauth-credentials-expires-at credentials))
                          "static API keys carry no expiry")
             (test-assert (not (credentials-needs-refresh-p credentials))
                          "static API keys never refresh"))
           (test-assert
            (handler-case
                (progn
                  (credential-source-save
                   source
                   (make-instance 'oauth-credentials
                                  :access-token "key"
                                  :account-id "fireworks"
                                  :source-path #P"/tmp/autolith-unwritten"))
                  nil)
              (authentication-error ()
                t))
            "the environment source rejects writes"))
      (setf (uiop:getenv "FIREWORKS_API_KEY") (or saved ""))))
  nil)

(-> fireworks-provider-test--wire-tools () null)
(defun fireworks-provider-test--wire-tools ()
  "Test Fireworks tool flattening and item normalization round trips."
  (let* ((namespaces
           (json-array
            (json-object
             "type" "namespace"
             "name" "fs"
             "description" "Files."
             "tools" (json-array
                      (json-object
                       "type" "function"
                       "name" "read"
                       "description" "Read one file."
                       "strict" false
                       "parameters" (json-object "type" "object"))))))
         (tools (grok-wire-tools namespaces)))
    (test-assert (string= (json-get (aref tools 0) "name") "fs.read")
                 "Fireworks wire tool names join the namespace with a dot"))
  (let ((provider (fireworks-provider-create
                   (fireworks-provider-test--configuration))))
    (let ((call (json-object
                 "type" "function_call"
                 "id" "fc_test"
                 "call_id" "call-1"
                 "name" "fs.read"
                 "arguments" "{}")))
      (provider-normalize-output-item provider call)
      (test-assert (null (gethash "id" call))
                   "normalized Fireworks items discard transient identifiers")
      (test-assert (and (string= (json-get call "namespace") "fs")
                        (string= (json-get call "name") "read"))
                   "flat wire names split into Autolith namespace and name"))
    (let ((dotless (json-object
                    "type" "function_call"
                    "call_id" "call-2"
                    "name" "shell"
                    "arguments" "{}")))
      (provider-normalize-output-item provider dotless)
      (test-assert (and (null (json-get dotless "namespace"))
                        (string= (json-get dotless "name") "shell"))
                   "dotless wire names stay unsplit"))
    (test-assert (string= (provider-account-label provider) "Fireworks")
                 "the Fireworks provider names its account service")
    (test-assert (typep (provider-credential-manager provider)
                        'fireworks-credential-manager)
                 "the Fireworks provider manages Fireworks credentials")
    (test-assert (eq (provider-family provider) ':fireworks)
                 "the Fireworks provider serves the Fireworks family"))
  nil)

(-> fireworks-provider-test--request-shape () null)
(defun fireworks-provider-test--request-shape ()
  "Test the standard Fireworks Responses request shape without network access."
  (let* ((base-configuration (fireworks-provider-test--configuration))
         (root (test-configuration-root base-configuration))
         (configuration
           (configuration--clone base-configuration
                                 :working-directory root)))
    (unwind-protect
         (let* ((conversation (conversation-create configuration
                                                   :identifier "fireworks-shape"))
                (provider (fireworks-provider-create configuration))
                (schemas (json-array
                          (json-object
                           "type" "namespace"
                           "name" "fs"
                           "description" "Files."
                           "tools" (json-array
                                    (json-object
                                     "type" "function"
                                     "name" "read"
                                     "description" "Read one file."
                                     "strict" false
                                     "parameters"
                                     (json-object "type" "object"))))))
                (request nil))
           (conversation-append-user-message conversation "hello")
           (setf request (provider-request-object provider conversation schemas))
           (test-assert
            (string= (json-get request "model")
                     "accounts/fireworks/models/kimi-k3")
            "the Fireworks request names the Fireworks model")
           (test-assert (string= (json-get (json-get request "reasoning")
                                           "effort")
                                 "high")
                        "default Ultra reasoning clamps to Fireworks high effort")
           (test-assert (eq (json-get request "store") false)
                        "Fireworks requests never store server-side responses")
           (test-assert (eq (json-get request "stream") t)
                        "Fireworks requests stream")
           (test-assert (non-empty-string-p (json-get request "prompt_cache_key"))
                        "Fireworks requests carry a prompt cache key")
           (test-assert (null (json-get request "include"))
                        "Fireworks requests omit the Codex include list")
           (let ((input (json-get request "input")))
             (test-assert
              (string= (json-get (aref input 0) "role") "developer")
              "the Autolith system prompt is the first input item")
             (test-assert
              (notany (lambda (item)
                        (string= (or (json-get item "type") "")
                                 "additional_tools"))
                      input)
              "the Responses Lite additional_tools item never rides to Fireworks"))
           (let ((tools (json-get request "tools")))
             (test-assert (string= (json-get (aref tools 0) "name") "fs.read")
                          "Fireworks tools carry dotted wire names"))
           (let* ((compaction-request
                    (provider-request-object
                     provider conversation schemas :compaction-p t)))
             (test-assert
              (zerop (length (json-get compaction-request "tools")))
              "compaction requests carry no tools")))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore)))
  nil)

(-> test-fireworks-provider () null)
(defun test-fireworks-provider ()
  "Test the Fireworks API key provider without network access."
  (fireworks-provider-test--selection)
  (fireworks-provider-test--credential-source)
  (fireworks-provider-test--wire-tools)
  (fireworks-provider-test--request-shape)
  nil)

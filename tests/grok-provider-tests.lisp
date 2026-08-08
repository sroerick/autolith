(in-package #:autolith)

;;;; -- Grok Provider Tests --

(-> grok-provider-test--configuration () configuration)
(defun grok-provider-test--configuration ()
  "Return an isolated configuration selecting the Grok model."
  (configuration-with-model (test-configuration) "grok-4.5"))

(-> grok-provider-test--namespaces () vector)
(defun grok-provider-test--namespaces ()
  "Return one namespaced test tool vector alongside a hosted passthrough tool."
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
              "parameters" (json-object "type" "object"))))
   (json-object "type" "web_search")))

(-> grok-provider-test--wire-tools () null)
(defun grok-provider-test--wire-tools ()
  "Test namespace flattening into standard Responses function tools."
  (let ((tools (grok-wire-tools (grok-provider-test--namespaces))))
    (test-assert (= (length tools) 2)
                 "wire tools keep one entry per tool plus passthroughs")
    (let ((flattened (aref tools 0)))
      (test-assert (string= (json-get flattened "type") "function")
                   "namespaced tools become standard function tools")
      (test-assert (string= (json-get flattened "name") "fs.read")
                   "wire tool names join the namespace with a dot")
      (test-assert (string= (json-get flattened "description")
                            "Read one file.")
                   "wire tools keep their descriptions"))
    (test-assert (string= (json-get (aref tools 1) "type") "web_search")
                 "entries that are not namespaces pass through unchanged"))
  nil)

(-> grok-provider-test--item-normalization () null)
(defun grok-provider-test--item-normalization ()
  "Test wire name splitting and flattening around one provider round trip."
  (let ((provider (grok-provider-create (grok-provider-test--configuration))))
    (test-assert
     (and (typep provider 'responses-api-provider)
          (eq (provider-wire-protocol provider) ':responses-api))
     "Grok declares the shared Responses API wire protocol")
    (let ((call (json-object
                 "type" "function_call"
                 "id" "server-item-1"
                 "call_id" "call-1"
                 "name" "fs.read"
                 "arguments" "{}")))
      (provider-normalize-output-item provider call)
      (test-assert (null (gethash "id" call))
                   "normalized Grok items discard transient identifiers")
      (test-assert (and (string= (json-get call "namespace") "fs")
                        (string= (json-get call "name") "read"))
                   "flat wire names split into Autolith namespace and name"))
    (let ((mcp-call (json-object
                     "type" "function_call"
                     "call_id" "call-2"
                     "name" "mcp__context.resolve-library"
                     "arguments" "{}")))
      (provider-normalize-output-item provider mcp-call)
      (test-assert
       (and (string= (json-get mcp-call "namespace") "mcp__context")
            (string= (json-get mcp-call "name") "resolve-library"))
       "MCP wire names split at the first dot"))
    (let ((dotless (json-object
                    "type" "function_call"
                    "call_id" "call-3"
                    "name" "shell"
                    "arguments" "{}")))
      (provider-normalize-output-item provider dotless)
      (test-assert (and (null (json-get dotless "namespace"))
                        (string= (json-get dotless "name") "shell"))
                   "dotless wire names stay unsplit"))
    (let ((message (json-object
                    "type" "message"
                    "id" "server-item-2"
                    "role" "assistant"
                    "content" (json-array))))
      (provider-normalize-output-item provider message)
      (test-assert (and (null (gethash "id" message))
                        (string= (json-get message "role") "assistant"))
                   "non-call items only lose transient identifiers")))
  (let* ((namespaced (json-object
                      "type" "function_call"
                      "call_id" "call-1"
                      "namespace" "fs"
                      "name" "read"
                      "arguments" "{}"))
         (flattened (grok-wire-input-item namespaced)))
    (test-assert (string= (json-get flattened "name") "fs.read")
                 "replayed function calls flatten to the dotted wire name")
    (test-assert (null (gethash "namespace" flattened))
                 "replayed function calls drop the namespace field")
    (test-assert (string= (json-get namespaced "name") "read")
                 "flattening never mutates the persisted conversation item")
    (let ((output (json-object
                   "type" "function_call_output"
                   "call_id" "call-1"
                   "output" "done")))
      (test-assert (eq (grok-wire-input-item output) output)
                   "non-call input items pass through identically")))
  nil)

(-> grok-provider-test--request-shape () null)
(defun grok-provider-test--request-shape ()
  "Test the standard Grok Responses request shape without network access."
  (let* ((base-configuration (grok-provider-test--configuration))
         (root (test-configuration-root base-configuration))
         (configuration
           (configuration--clone base-configuration
                                 :working-directory root)))
    (unwind-protect
         (let* ((conversation (conversation-create configuration
                                                   :identifier "grok-shape"))
                (provider (grok-provider-create configuration))
                (schemas (subseq (grok-provider-test--namespaces) 0 1))
                (request nil))
           (conversation-append-user-message conversation "hello")
           (setf request (provider-request-object provider conversation schemas))
           (test-assert (string= (json-get request "model") "grok-4.5")
                        "the Grok request names the Grok model")
           (test-assert (null (json-get request "service_tier"))
                        "Grok requests never select a provider service tier")
           (test-assert (null (json-get request "prompt_cache_key"))
                        "Grok requests omit the Codex prompt cache key")
           (test-assert (null (json-get request "text"))
                        "Grok requests omit the Codex verbosity selection")
           (let ((input (json-get request "input")))
             (test-assert (= (length input) 2)
                          "the Grok request prefixes one developer item")
             (test-assert
              (string= (json-get (aref input 0) "role") "developer")
              "the Autolith system prompt is the first input item")
             (test-assert (string= (json-get (aref input 1) "role") "user")
                          "conversation history follows the developer prefix")
             (test-assert
              (notany (lambda (item)
                        (string= (or (json-get item "type") "")
                                 "additional_tools"))
                      input)
              "the Responses Lite additional_tools item never rides to Grok"))
           (let ((tools (json-get request "tools")))
             (test-assert (= (length tools) 1)
                          "Grok tools ride in the flat request tools array")
             (test-assert (string= (json-get (aref tools 0) "name") "fs.read")
                          "Grok tools carry dotted wire names"))
           (test-assert
            (string= (json-get (json-get request "reasoning") "effort") "high")
            "default Ultra reasoning clamps to Grok's high effort")
           (loop for (effort . wire) in '(("none" . "low")
                                          ("low" . "low")
                                          ("medium" . "medium")
                                          ("high" . "high")
                                          ("xhigh" . "high")
                                          ("max" . "high")
                                          ("ultra" . "high"))
                 do (let* ((selected (configuration-with-reasoning-effort
                                      configuration effort))
                           (selected-request
                             (provider-request-object
                              (grok-provider-create selected)
                              conversation
                              schemas)))
                      (test-assert
                       (string= (json-get
                                 (json-get selected-request "reasoning")
                                 "effort")
                                wire)
                       (format nil "Grok maps ~A reasoning onto ~A"
                               effort wire))))
           (multiple-value-bind (value present-p)
               (gethash "summary" (json-get request "reasoning"))
             (declare (ignore value))
             (test-assert (not present-p)
                          "Grok requests never ask for reasoning summaries"))
           (test-assert (string= (json-get request "tool_choice") "auto")
                        "the Grok request permits automatic tool selection")
           (test-assert (eq (json-get request "parallel_tool_calls") false)
                        "the Grok request disables parallel tool calls")
           (test-assert (eq (json-get request "store") false)
                        "the Grok request disables server-side storage")
           (test-assert (eq (json-get request "stream") t)
                        "the Grok request enables event streaming")
           (test-assert
            (equalp (json-get request "include")
                    (json-array "reasoning.encrypted_content"))
            "the Grok request retains encrypted reasoning for replay")
           (let* ((goal-request
                    (provider-request-object
                     provider conversation schemas
                     :goal-context "<goal_context>persist</goal_context>"))
                  (goal-input (json-get goal-request "input")))
             (test-assert (= (length goal-input) 3)
                          "an active goal adds one transient developer message")
             (test-assert
              (string= (json-get (aref goal-input 1) "role") "developer")
              "the goal context is a developer message"))
           (let* ((compaction-request
                    (provider-request-object
                     provider conversation schemas :compaction-p t))
                  (compaction-input (json-get compaction-request "input")))
             (test-assert
              (zerop (length (json-get compaction-request "tools")))
              "compaction requests carry no tools")
             (test-assert
              (search "context checkpoint"
                      (json-get
                       (aref (json-get
                              (aref compaction-input
                                    (1- (length compaction-input)))
                              "content")
                             0)
                       "text"))
              "compaction requests end with the handoff instructions")))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore)))
  nil)

(-> grok-provider-test--transport-headers () null)
(defun grok-provider-test--transport-headers ()
  "Test the authenticated Grok transport headers without network access."
  (let* ((configuration (grok-provider-test--configuration))
         (root (test-configuration-root configuration)))
    (unwind-protect
         (let* ((conversation (conversation-create configuration
                                                   :identifier "grok-wire"))
                (provider (grok-provider-create configuration))
                (credentials
                  (make-instance 'oauth-credentials
                                 :access-token "grok-access-token"
                                 :refresh-token nil
                                 :id-token nil
                                 :account-id "grok-user-1"
                                 :expires-at nil
                                 :source-path
                                 (configuration-grok-auth-path configuration)))
                (captured-url nil)
                (captured-headers nil))
           (test-call-with-function-replacements
            (list
             (list
              'dexador:post
              (lambda (url &key headers content &allow-other-keys)
                (declare (ignore content))
                (setf captured-url url
                      captured-headers headers)
                (values (make-string-input-stream "") 200 nil))))
            (lambda ()
              (provider-open-response-stream
               provider
               (json-object "model" "grok-4.5")
               :credentials credentials
               :conversation conversation)))
           (flet ((header (name)
                    (rest (assoc name captured-headers :test #'string-equal))))
             (test-assert (string= captured-url
                                   (configuration-provider-endpoint
                                    configuration))
                          "the Grok transport posts to the configured proxy")
             (test-assert (string= (header "Authorization")
                                   "Bearer grok-access-token")
                          "the Grok transport sends the bearer access token")
             (test-assert (string= (header "X-XAI-Token-Auth") "xai-grok-cli")
                          "the Grok transport marks user token authentication")
             (test-assert (string= (header "x-authenticateresponse")
                                   "authenticate-response")
                          "the Grok transport requests authenticated responses")
             (test-assert (string= (header "x-grok-model-override") "grok-4.5")
                          "the Grok transport pins the requested model")
             (test-assert (string= (header "x-grok-client-version")
                                   *grok-client-protocol-version*)
                          "the Grok transport passes the proxy version gate")
             (test-assert (string= (header "x-grok-client-mode") "interactive")
                          "the Grok transport reports interactive client mode")
             (test-assert (string= (header "x-grok-conv-id") "grok-wire")
                          "the Grok transport carries the conversation identity")
             (test-assert (string= (header "Accept") "text/event-stream")
                          "the Grok transport requests an event stream")))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore)))
  nil)

(-> grok-provider-test--selection () null)
(defun grok-provider-test--selection ()
  "Test provider construction and reconfiguration across model families."
  (let* ((codex-configuration (test-configuration))
         (root (test-configuration-root codex-configuration)))
    (unwind-protect
         (let* ((grok-configuration
                  (configuration-with-model codex-configuration "grok-4.5"))
                (codex-provider (provider-create codex-configuration))
                (grok-provider (provider-create grok-configuration)))
           (test-assert (typep codex-provider 'codex-subscription-provider)
                        "GPT models select the Codex subscription provider")
           (test-assert (typep grok-provider 'grok-subscription-provider)
                        "Grok models select the Grok subscription provider")
           (test-assert
            (string= (configuration-provider-endpoint grok-configuration)
                     *grok-responses-endpoint*)
            "selecting a Grok model selects the Grok proxy endpoint")
           (test-assert
            (= (configuration-context-window grok-configuration) 500000)
            "selecting a Grok model selects the Grok context window")
           (test-assert
            (typep (provider-with-configuration codex-provider
                                                grok-configuration)
                   'grok-subscription-provider)
            "switching to a Grok model replaces the Codex provider")
           (test-assert
            (typep (provider-with-configuration grok-provider
                                                codex-configuration)
                   'codex-subscription-provider)
            "switching to a GPT model replaces the Grok provider")
           (let ((copied (provider-with-configuration grok-provider
                                                      grok-configuration)))
             (test-assert
              (and (typep copied 'grok-subscription-provider)
                   (string= (provider-session-id copied)
                            (provider-session-id grok-provider))
                   (eq (provider-credential-manager copied)
                       (provider-credential-manager grok-provider)))
              "same-family reconfiguration preserves session and credentials")))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore)))
  nil)

(-> test-grok-provider () null)
(defun test-grok-provider ()
  "Test the Grok subscription provider without network access."
  (grok-provider-test--selection)
  (grok-provider-test--wire-tools)
  (grok-provider-test--item-normalization)
  (let ((provider (grok-provider-create (grok-provider-test--configuration))))
    (test-assert (string= (provider-account-label provider) "Grok")
                 "the Grok provider names its account service")
    (test-assert (typep (provider-credential-manager provider)
                        'grok-credential-manager)
                 "the Grok provider manages Grok credentials")
    (test-assert
     (eq (provider-set-reasoning-summaries provider t) provider)
     "reasoning summary selection leaves the Grok provider unchanged"))
  (grok-provider-test--request-shape)
  (grok-provider-test--transport-headers)
  nil)

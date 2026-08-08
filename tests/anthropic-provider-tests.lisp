(in-package #:autolith)

;;;; -- Anthropic API Key Provider Tests --

(-> anthropic-provider-test--configuration () configuration)
(defun anthropic-provider-test--configuration ()
  "Return an isolated configuration selecting the Anthropic model."
  (configuration-with-model (test-configuration)
                            "claude-haiku-4-5-20251001"))

(-> anthropic-provider-test--selection () null)
(defun anthropic-provider-test--selection ()
  "Test Anthropic model family resolution and endpoint selection."
  (test-assert (eq (model-family "claude-haiku-4-5-20251001") ':anthropic)
               "Claude identifiers resolve to the Anthropic family")
  (test-assert (eq (model-family "gpt-5.6-sol") ':codex)
               "GPT identifiers keep the Codex family")
  (let ((configuration (anthropic-provider-test--configuration)))
    (test-assert
     (string= (configuration-provider-endpoint configuration)
              *anthropic-messages-endpoint*)
     "Anthropic configurations select the Anthropic Messages endpoint")
    (test-assert (= (configuration-context-window configuration) 200000)
                 "Claude models carry the Anthropic context window"))
  nil)

(-> anthropic-provider-test--credential-source () null)
(defun anthropic-provider-test--credential-source ()
  "Test the Anthropic environment credential source without network access."
  (let ((source (make-instance 'anthropic-environment-credential-source))
        (saved (uiop:getenv "ANTHROPIC_API_KEY")))
    (unwind-protect
         (progn
           (setf (uiop:getenv "ANTHROPIC_API_KEY") "")
           (test-assert (null (credential-source-load source))
                        "an empty environment yields no Anthropic credentials")
           (setf (uiop:getenv "ANTHROPIC_API_KEY") "test-anthropic-key")
           (let ((credentials (credential-source-load source)))
             (test-assert (typep credentials 'oauth-credentials)
                          "the environment key loads as request credentials")
             (test-assert (string= (oauth-credentials-access-token credentials)
                                   "test-anthropic-key")
                          "the API key rides as the request access token")
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
                                  :account-id "anthropic"
                                  :source-path #P"/tmp/autolith-unwritten"))
                  nil)
              (authentication-error ()
                t))
            "the environment source rejects writes"))
      (setf (uiop:getenv "ANTHROPIC_API_KEY") saved)))
  (let* ((configuration (anthropic-provider-test--configuration))
         (manager (anthropic-credential-manager-create configuration))
         (saved (uiop:getenv "ANTHROPIC_API_KEY")))
    (unwind-protect
         (progn
           (setf (uiop:getenv "ANTHROPIC_API_KEY") "")
           (test-assert
            (handler-case
                (progn (credential-manager-load manager) nil)
              (credentials-unavailable () t))
            "an empty store and environment require interactive login")
           (setf (uiop:getenv "ANTHROPIC_API_KEY") "environment-key")
           (test-assert
            (string= (oauth-credentials-access-token
                      (credential-manager-load manager))
                     "environment-key")
            "the environment key takes precedence over the saved key"))
      (setf (uiop:getenv "ANTHROPIC_API_KEY") saved)))
  nil)

(-> anthropic-provider-test--request-encoding () null)
(defun anthropic-provider-test--request-encoding ()
  "Test Messages request shape: system blocks, alternation, and tool encoding."
  (let* ((configuration (anthropic-provider-test--configuration))
         (provider (anthropic-provider-create configuration))
         (conversation (conversation-create configuration)))
    (conversation-append-user-message conversation "first question")
    (dolist (item
             (list (json-object "type" "message" "role" "assistant"
                                "content" (json-array
                                           (json-object "type" "output_text"
                                                        "text" "first answer")))
                   (json-object "type" "function_call"
                                "namespace" "plan" "name" "update"
                                "call_id" "call-1"
                                "arguments" "{\"steps\":[]}")
                   (json-object "type" "function_call_output"
                                "call_id" "call-1" "output" "done")
                   (json-object "type" "message" "role" "user"
                                "content" (json-array
                                           (json-object "type" "input_text"
                                                        "text" "second question")))))
      (conversation-append-provider-item conversation item))
    (multiple-value-bind (request delivery)
        (provider-request-object
         provider
         conversation
         (json-array
          (json-object "type" "namespace" "name" "plan"
                       "tools" (json-array
                                (json-object
                                 "name" "update"
                                 "description" "Update the plan."
                                 "parameters" (json-object "type" "object")))))
         :goal-context "remember the goal")
      (declare (ignore delivery))
      (let ((messages (json-get request "messages"))
            (system (json-get request "system"))
            (tools (json-get request "tools")))
        (test-assert (string= (json-get request "model")
                              "claude-haiku-4-5-20251001")
                     "the request selects the configured Claude model")
        (test-assert (and (integerp (json-get request "max_tokens"))
                          (eq (json-get request "stream") t))
                     "the request streams with a bounded output budget")
        (test-assert
         (and (vectorp system)
              (loop for block across system
                    always (json-string= (json-get block "type") "text"))
              (find "remember the goal"
                    (loop for block across system
                          collect (json-get block "text"))
                    :test (lambda (needle text)
                            (and (stringp text) (search needle text)))))
        "system prompt, goal context, and developer text ride as system blocks")
        (test-assert
         (equal (loop for message across messages
                      collect (json-get message "role"))
                '("user" "assistant" "user"))
         "calls, results, and text merge into strict user/assistant alternation")
        (let ((assistant (find "assistant" (coerce messages 'list)
                               :key (lambda (message) (json-get message "role"))
                               :test #'string=)))
          (test-assert
           (equal (loop for block across (json-get assistant "content")
                        collect (json-get block "type"))
                  '("text" "tool_use"))
           "assistant messages join text and tool_use blocks in order"))
        (let ((final-user (first (last (coerce messages 'list)))))
          (test-assert
           (equal (loop for block across (json-get final-user "content")
                        collect (json-get block "type"))
                  '("tool_result" "text"))
           "user messages join tool_result and text blocks in order"))
        (let ((assistant-calls
                (loop for message across messages
                      when (string= (json-get message "role") "assistant")
                        append (loop for block across (json-get message "content")
                                     when (json-string= (json-get block "type")
                                                        "tool_use")
                                       collect block))))
          (test-assert
           (= (length assistant-calls) 1)
           "function calls become tool_use blocks")
          (test-assert
           (and (string= (json-get (first assistant-calls) "id") "call-1")
                (json-object-p (json-get (first assistant-calls) "input")))
           "tool_use blocks carry the call id and decoded input"))
        (let ((results
                (loop for message across messages
                      when (string= (json-get message "role") "user")
                        append (loop for block across (json-get message "content")
                                     when (json-string= (json-get block "type")
                                                        "tool_result")
                                       collect block))))
          (test-assert
           (and (= (length results) 1)
                (string= (json-get (first results) "tool_use_id") "call-1")
                (string= (json-get (first results) "content") "done"))
           "function outputs become user tool_result blocks"))
        (test-assert
         (and (= (length tools) 1)
              (json-object-p (json-get (aref tools 0) "input_schema"))
              (json-object-p (json-get request "tool_choice")))
         "tools flatten into input_schema declarations with automatic choice")
        (let ((wire-name (json-get (aref tools 0) "name")))
          (multiple-value-bind (namespace name)
              (openai-compatible--decode-wire-tool-name wire-name)
            (test-assert (and (string= namespace "plan")
                              (string= name "update"))
                         "wire tool names round-trip through the Base64 encoding"))))))
  nil)


(-> anthropic-provider-test--portable-content () null)
(defun anthropic-provider-test--portable-content ()
  "Test durable cross-provider refusal and multimodal tool-result replay."
  (let* ((configuration (anthropic-provider-test--configuration))
         (provider (anthropic-provider-create configuration))
         (conversation (conversation-create configuration))
         (identifier (conversation-identifier conversation))
         (data-url "data:image/png;base64,AA=="))
    (conversation-append-user-message conversation "Show the image.")
    (conversation-append-provider-item
     conversation
     (json-object "type" "function_call"
                  "namespace" "fs"
                  "name" "view-image"
                  "call_id" "view-1"
                  "arguments" "{}"))
    (conversation-append-provider-item
     conversation
     (json-object
      "type" "function_call_output"
      "call_id" "view-1"
      "output"
      (json-array
       (json-object "type" "input_text" "text" "Before image.")
       (json-object "type" "input_image" "image_url" data-url)
       (json-object "type" "input_text" "text" "After image.")
       (json-object "type" "input_image"
                    "image_url" "https://example.com/image.png"))))
    (conversation-append-provider-item
     conversation
     (json-object
      "type" "message"
      "role" "assistant"
      "content"
      (json-array
       (json-object "type" "refusal" "text" "I cannot do that."))))
    (conversation-append-user-message conversation "Try this instead.")
    (let ((reloaded (conversation-load-by-id configuration identifier)))
      (multiple-value-bind (request delivery)
          (provider-request-object provider reloaded #())
        (declare (ignore delivery))
        (let* ((messages (json-get request "messages"))
               (tool-result
                 (loop for message across messages
                       thereis
                       (loop for block across (json-get message "content")
                             when (json-string= (json-get block "type")
                                                "tool_result")
                               return block)))
               (content (and tool-result (json-get tool-result "content")))
               (refusal-text
                 (loop for message across messages
                       thereis
                       (and (json-string= (json-get message "role")
                                          "assistant")
                            (loop for block across (json-get message "content")
                                  thereis
                                  (and (json-string= (json-get block "type") "text")
                                       (string= (json-get block "text")
                                                "I cannot do that.")
                                       (json-get block "text")))))))
          (test-assert
           (equal (loop for message across messages
                        collect (json-get message "role"))
                  '("user" "assistant" "user" "assistant" "user"))
           "durable refusal replay preserves surrounding message roles")
          (test-assert
           (and (vectorp content)
                (equal (loop for block across content
                             collect (json-get block "type"))
                       '("text" "image" "text" "image"))
                (string= (json-get (aref content 0) "text") "Before image.")
                (string= (json-get (json-get (aref content 1) "source") "data")
                         "AA==")
                (string= (json-get (aref content 2) "text") "After image.")
                (string= (json-get (json-get (aref content 3) "source") "url")
                         "https://example.com/image.png"))
           "durable tool results preserve base64, URL image, and text order")
          (test-assert (string= refusal-text "I cannot do that.")
                       "durable refusal content becomes Anthropic assistant text")))))
  nil)

(-> anthropic-provider-test--request-condition
    (anthropic-api-key-provider list)
    (option provider-error))
(defun anthropic-provider-test--request-condition (provider items)
  "Return the provider condition from translating ITEMS, or NIL."
  (let ((conversation
          (conversation-create (provider-configuration provider))))
    (conversation-append-user-message conversation "Seed message.")
    (dolist (item items)
      (conversation-append-provider-item conversation item))
    (handler-case
        (progn
          (provider-request-object provider conversation #())
          nil)
      (provider-error (condition)
        condition))))

(-> anthropic-provider-test--request-conversion-failures () null)
(defun anthropic-provider-test--request-conversion-failures ()
  "Test explicit failures for outbound content Anthropic cannot preserve."
  (let* ((configuration (anthropic-provider-test--configuration))
         (provider (anthropic-provider-create configuration)))
    (dolist
        (case
         (list
          (list
           "unsupported message content"
           "unsupported Anthropic content"
           (list
            (json-object "type" "message" "role" "assistant"
                         "content"
                         (json-array
                          (json-object "type" "future_content" "value" 1)))))
          (list
           "unsupported image source"
           "unsupported Anthropic content"
           (list
            (json-object "type" "message" "role" "assistant"
                         "content"
                         (json-array
                          (json-object "type" "input_image"
                                       "image_url" "file:///tmp/image.png")))))
          (list
           "array tool arguments"
           "valid Anthropic tool use"
           (list
            (json-object "type" "function_call"
                         "call_id" "call-array"
                         "name" "read"
                         "arguments" "[]")))
          (list
           "scalar tool arguments"
           "valid Anthropic tool use"
           (list
            (json-object "type" "function_call"
                         "call_id" "call-scalar"
                         "name" "read"
                         "arguments" "42")))
          (list
           "missing tool-result identifier"
           "valid Anthropic tool-use identifier"
           (list
            (json-object "type" "function_call_output"
                         "output" "done")))
          (list
           "empty tool-result identifier"
           "valid Anthropic tool-use identifier"
           (list
            (json-object "type" "function_call_output"
                         "call_id" ""
                         "output" "done")))
          (list
           "non-string tool-result identifier"
           "valid Anthropic tool-use identifier"
           (list
            (json-object "type" "function_call_output"
                         "call_id" 7
                         "output" "done")))
          (list
           "orphan tool result"
           "pending Anthropic tool use"
           (list
            (json-object "type" "function_call_output"
                         "call_id" "call-orphan"
                         "output" "done")))
          (list
           "mismatched tool result"
           "pending Anthropic tool use"
           (list
            (json-object "type" "function_call"
                         "call_id" "call-expected"
                         "name" "read"
                         "arguments" "{}")
            (json-object "type" "function_call_output"
                         "call_id" "call-other"
                         "output" "done")))
          (list
           "duplicate tool result"
           "pending Anthropic tool use"
           (list
            (json-object "type" "function_call"
                         "call_id" "call-duplicate-result"
                         "name" "read"
                         "arguments" "{}")
            (json-object "type" "function_call_output"
                         "call_id" "call-duplicate-result"
                         "output" "first")
            (json-object "type" "function_call_output"
                         "call_id" "call-duplicate-result"
                         "output" "second")))
          (list
           "duplicate function-call identifier"
           "reuses an Anthropic tool-use identifier"
           (list
            (json-object "type" "function_call"
                         "call_id" "call-duplicate"
                         "name" "read"
                         "arguments" "{}")
            (json-object "type" "function_call"
                         "call_id" "call-duplicate"
                         "name" "read"
                         "arguments" "{}")))
          (list
           "mixed tool-result content"
           "mixes supported and unsupported content"
           (list
            (json-object "type" "function_call"
                         "call_id" "call-mixed"
                         "name" "read"
                         "arguments" "{}")
            (json-object "type" "function_call_output"
                         "call_id" "call-mixed"
                         "output"
                         (json-array
                          (json-object "type" "input_text" "text" "kept")
                          (json-object "type" "future_content" "value" 1)))))))
      (let ((condition
              (anthropic-provider-test--request-condition
               provider (third case))))
        (test-assert
         (and (typep condition 'provider-error)
              (string= (provider-error-code condition) "unsupported_content")
              (search (second case) (autolith-error-message condition)
                      :test #'char-equal))
         (format nil "request conversion fails explicitly: ~A" (first case)))))
    (let ((fallback
            (anthropic--tool-result-block
             (json-object
              "type" "function_call_output"
              "call_id" "call-fallback"
              "output"
              (json-array
               (json-object "type" "future_content" "value" 1))))))
      (test-assert
       (stringp (json-get fallback "content"))
       "an entirely unsupported tool result uses a bounded textual fallback")))
  nil)

(-> anthropic-provider-test--inherited-reference-order () null)
(defun anthropic-provider-test--inherited-reference-order ()
  "Test that a child-reference boundary retains its transcript position."
  (let* ((configuration (anthropic-provider-test--configuration))
         (provider (anthropic-provider-create configuration))
         (conversation (conversation-create configuration)))
    (conversation-append-inherited-reference
     conversation
     "parent-conversation"
     (list
      (json-object
       "type" "message" "role" "user"
       "content" (json-array
                  (json-object "type" "input_text" "text" "Parent question.")))
      (json-object
       "type" "message" "role" "assistant"
       "content" (json-array
                  (json-object "type" "output_text" "text" "Parent answer.")))))
    (conversation-append-user-message conversation "Child assignment.")
    (multiple-value-bind (request delivery)
        (provider-request-object provider conversation #())
      (declare (ignore delivery))
      (let* ((messages (json-get request "messages"))
             (system (json-get request "system"))
             (final-user (aref messages (1- (length messages))))
             (final-content (json-get final-user "content")))
        (test-assert
         (equal (loop for message across messages
                      collect (json-get message "role"))
                '("user" "assistant" "user"))
         "inherited history and child input preserve transcript role order")
        (test-assert
         (and (= (length final-content) 2)
              (string= (json-get (aref final-content 0) "text")
                       *conversation-inherited-reference-boundary*)
              (string= (json-get (aref final-content 1) "text")
                       "Child assignment."))
         "the inherited-reference boundary remains before the child assignment")
        (test-assert
         (not
          (some (lambda (block)
                  (search *conversation-inherited-reference-boundary*
                          (or (json-get block "text") "")))
                (coerce system 'list)))
         "the positional inherited-reference boundary is not hoisted into system"))))
  nil)

(-> anthropic-provider-test--compaction-request () null)
(defun anthropic-provider-test--compaction-request ()
  "Test Anthropic's tool-free portable compaction request."
  (let* ((configuration (anthropic-provider-test--configuration))
         (provider (anthropic-provider-create configuration))
         (conversation (conversation-create configuration)))
    (conversation-append-user-message conversation "Conversation to summarize.")
    (multiple-value-bind (request delivery)
        (provider-request-object
         provider conversation
         (json-array
          (json-object "type" "namespace" "name" "fs"
                       "tools" (json-array
                                (json-object "name" "read"
                                             "description" "Read a file."
                                             "parameters"
                                             (json-object "type" "object")))))
         :goal-context "request-local goal"
         :compaction-p t)
      (let* ((system (json-get request "system"))
             (texts (loop for block across system
                          collect (json-get block "text"))))
        (test-assert (null delivery)
                     "compaction resolves no request-local context delivery")
        (test-assert (and (null (json-get request "tools"))
                          (null (json-get request "tool_choice")))
                     "compaction requests expose no tools")
        (test-assert
         (and (find *compaction-instructions* texts :test #'string=)
              (not (find "request-local goal" texts :test #'string=)))
         "compaction includes its handoff instruction but not goal context"))))
  nil)

(-> anthropic-provider-test--transport () null)
(defun anthropic-provider-test--transport ()
  "Test Anthropic transport endpoints, headers, and UTF-8 request bodies."
  (let* ((configuration (anthropic-provider-test--configuration))
         (provider (anthropic-provider-create configuration))
         (conversation (conversation-create configuration))
         (credentials
           (make-instance 'oauth-credentials
                          :access-token "synthetic-anthropic-key"
                          :account-id "anthropic"
                          :source-path
                          (configuration-api-keys-path configuration)))
         (captured-url nil)
         (captured-headers nil)
         (captured-content nil)
         (captured-options nil))
    (test-call-with-function-replacements
     (list
      (list
       'dexador:post
       (lambda (url &key headers content want-stream force-string keep-alive
                    connect-timeout read-timeout &allow-other-keys)
         (setf captured-url url
               captured-headers headers
               captured-content content
               captured-options
               (list want-stream force-string keep-alive
                     connect-timeout read-timeout))
         (values (make-string-input-stream "") 200
                 '(("request-id" . "request-transport"))))))
     (lambda ()
       (provider-open-response-stream
        provider
        (json-object "model" "claude-haiku-4-5-20251001" "stream" t)
        :credentials credentials
        :conversation conversation)))
    (test-assert (string= captured-url *anthropic-messages-endpoint*)
                 "Anthropic turns post to the Messages endpoint")
    (test-assert
     (and (string= (response-header captured-headers "x-api-key")
                   "synthetic-anthropic-key")
          (string= (response-header captured-headers "anthropic-version")
                   *anthropic-api-version*)
          (string= (response-header captured-headers "Accept")
                   "text/event-stream"))
     "Anthropic transport sends static-key, version, and SSE headers")
    (test-assert
     (and (equal captured-options '(t t nil 30 300))
          (typep captured-content '(vector (unsigned-byte 8)))
          (string=
           (json-get
            (json-decode
             (sb-ext:octets-to-string captured-content :external-format ':utf-8))
            "model")
           "claude-haiku-4-5-20251001"))
     "Anthropic transport sends bounded streaming options and UTF-8 JSON")
    (let ((validation-url nil)
          (validation-headers nil))
      (test-call-with-function-replacements
       (list
        (list
         'dexador:get
         (lambda (url &key headers &allow-other-keys)
           (setf validation-url url
                 validation-headers headers)
           (values "{}" 200 '(("request-id" . "request-validation"))))))
       (lambda ()
         (anthropic-validate-api-key "synthetic-validation-key")))
      (test-assert
       (and (string= validation-url *anthropic-models-endpoint*)
            (string= (response-header validation-headers "x-api-key")
                     "synthetic-validation-key")
            (string= (response-header validation-headers "anthropic-version")
                     *anthropic-api-version*))
       "API-key validation probes the models endpoint with Anthropic headers")))
  nil)

(-> anthropic-provider-test--sse-event (json-object) string)
(defun anthropic-provider-test--sse-event (event)
  "Return EVENT as one SSE data payload."
  (format nil "data: ~A~2%" (json-encode event)))

(-> anthropic-provider-test--stream-source (list) string)
(defun anthropic-provider-test--stream-source (events)
  "Return EVENTS encoded as one Anthropic SSE stream."
  (with-output-to-string (stream)
    (dolist (event events)
      (write-string (anthropic-provider-test--sse-event event) stream))))

(-> anthropic-provider-test--message-start
    (&key (:id string) (:usage json-object))
    json-object)
(defun anthropic-provider-test--message-start
    (&key (id "msg-test")
          (usage (json-object "input_tokens" 42 "output_tokens" 1)))
  "Return one valid Anthropic message_start event."
  (json-object
   "type" "message_start"
   "message"
   (json-object "id" id
                "type" "message"
                "role" "assistant"
                "model" "claude-haiku-4-5-20251001"
                "content" (json-array)
                "stop_reason" nil
                "stop_sequence" nil
                "usage" usage)))

(-> anthropic-provider-test--message-delta
    (string &key (:usage json-object))
    json-object)
(defun anthropic-provider-test--message-delta
    (stop-reason &key (usage (json-object "output_tokens" 7)))
  "Return one Anthropic message_delta with STOP-REASON and USAGE."
  (json-object "type" "message_delta"
               "delta" (json-object "stop_reason" stop-reason)
               "usage" usage))

(-> anthropic-provider-test--consume
    (anthropic-api-key-provider list &key (:headers t) (:event-callback function))
    provider-result)
(defun anthropic-provider-test--consume
    (provider events &key headers (event-callback #'identity))
  "Consume Anthropic EVENTS through PROVIDER."
  (provider-consume-stream
   provider
   (make-string-input-stream
    (anthropic-provider-test--stream-source events))
   headers
   event-callback))

(-> anthropic-provider-test--consume-condition
    (anthropic-api-key-provider list &key (:headers t))
    (option provider-error))
(defun anthropic-provider-test--consume-condition (provider events &key headers)
  "Return the provider condition signaled while consuming EVENTS, or NIL."
  (handler-case
      (progn
        (anthropic-provider-test--consume provider events :headers headers)
        nil)
    (provider-error (condition)
      condition)))

(-> anthropic-provider-test--stream-decoding () null)
(defun anthropic-provider-test--stream-decoding ()
  "Test Anthropic SSE decoding into a normalized provider result."
  (let* ((configuration (anthropic-provider-test--configuration))
         (provider (anthropic-provider-create configuration))
         (events nil)
         (result
           (anthropic-provider-test--consume
            provider
            (list
             (anthropic-provider-test--message-start)
             (json-object "type" "ping")
             (json-object "type" "future_metadata" "value" 1)
             (json-object "type" "content_block_start"
                          "index" 0
                          "content_block"
                          (json-object "type" "text" "text" ""))
             (json-object "type" "content_block_delta"
                          "index" 0
                          "delta" (json-object "type" "text_delta"
                                               "text" "hel"))
             (json-object "type" "content_block_delta"
                          "index" 0
                          "delta" (json-object "type" "text_delta"
                                               "text" "lo"))
             (json-object "type" "content_block_stop" "index" 0)
             (json-object "type" "content_block_start"
                          "index" 1
                          "content_block"
                          (json-object
                           "type" "tool_use"
                           "id" "toolu-1"
                           "name"
                           (openai-compatible--wire-tool-name "fs" "read")
                           "input" (json-object)))
             (json-object "type" "content_block_delta"
                          "index" 1
                          "delta" (json-object "type" "input_json_delta"
                                               "partial_json" "{\"path\":"))
             (json-object "type" "content_block_delta"
                          "index" 1
                          "delta" (json-object "type" "input_json_delta"
                                               "partial_json" "\"x\"}"))
             (json-object "type" "content_block_stop" "index" 1)
             (anthropic-provider-test--message-delta "tool_use")
             (json-object "type" "message_stop"))
            :event-callback (lambda (event) (push event events))))
         (usage (provider-result-usage result))
         (call (first (provider-result-tool-calls result))))
    (test-assert (string= (provider-result-response-id result) "msg-test")
                 "Anthropic streams retain the message identifier")
    (test-assert (= (length (provider-result-output-items result)) 2)
                 "completed content blocks become ordered output items")
    (test-assert
     (string= (or (provider-result-assistant-text result) "") "hello")
     "fragmented text deltas assemble the assistant message")
    (test-assert
     (and call
          (string= (json-get call "namespace") "fs")
          (string= (json-get call "name") "read")
          (string= (json-get call "call_id") "toolu-1")
          (string= (json-get call "arguments") "{\"path\":\"x\"}"))
     "fragmented tool_use input becomes a namespaced function call")
    (test-assert
     (and (= (json-get usage "input_tokens") 42)
          (= (json-get usage "output_tokens") 7)
          (= (json-get usage "total_tokens") 49))
     "message_start and message_delta usage combine into portable usage")
    (test-assert
     (and (eq (provider-result-turn-completion result) ':continue)
          (find-if (lambda (event) (typep event 'assistant-delta-event)) events)
          (find-if (lambda (event) (typep event 'provider-completed-event))
                   events))
     "tool_use streams emit deltas and request a provider follow-up")
    (test-assert
     (handler-case
         (progn
           (anthropic-provider-test--consume
            provider
            (list (anthropic-provider-test--message-start)))
           nil)
       (response-stream-error ()
         t))
     "streams closing before message_stop remain retryable failures")
    (test-assert
     (handler-case
         (progn
           (anthropic-provider-test--consume
            provider
            (list
             (json-object "type" "error"
                          "error"
                          (json-object "type" "invalid_request_error"
                                       "message" "broken"))))
           nil)
       (provider-error ()
         t))
     "stream error events surface as provider failures"))
  nil)

(-> anthropic-provider-test--stop-reasons () null)
(defun anthropic-provider-test--stop-reasons ()
  "Test documented Anthropic stop reasons and explicit unsupported outcomes."
  (let* ((configuration (anthropic-provider-test--configuration))
         (provider (anthropic-provider-create configuration)))
    (dolist (case '(("end_turn" :end)
                    ("stop_sequence" :end)
                    ("refusal" :end)))
      (let ((result
              (anthropic-provider-test--consume
               provider
               (list (anthropic-provider-test--message-start)
                     (anthropic-provider-test--message-delta (first case))
                     (json-object "type" "message_stop")))))
        (test-assert
         (eq (provider-result-turn-completion result) (second case))
         (format nil "~A maps to ~A turn completion"
                 (first case) (second case)))))
    (let* ((wire-name (openai-compatible--wire-tool-name "fs" "read"))
           (result
             (anthropic-provider-test--consume
              provider
              (list
               (anthropic-provider-test--message-start)
               (json-object "type" "content_block_start" "index" 0
                            "content_block"
                            (json-object "type" "tool_use"
                                         "id" "tool-initial"
                                         "name" wire-name
                                         "input" (json-object "path" "x")))
               (json-object "type" "content_block_stop" "index" 0)
               (anthropic-provider-test--message-delta "tool_use")
               (json-object "type" "message_stop"))))
           (call (first (provider-result-tool-calls result))))
      (test-assert
       (and (eq (provider-result-turn-completion result) ':continue)
            call
            (string= (json-get call "arguments") "{\"path\":\"x\"}"))
       "tool_use continues the turn and accepts complete initial input"))
    (dolist (reason '("max_tokens" "model_context_window_exceeded"))
      (let ((condition
              (anthropic-provider-test--consume-condition
               provider
               (list (anthropic-provider-test--message-start)
                     (anthropic-provider-test--message-delta reason)
                     (json-object "type" "message_stop"))
               :headers '(("request-id" . "request-incomplete")))))
        (test-assert
         (and (typep condition 'provider-incomplete-response)
              (string= (provider-incomplete-response-reason condition) reason)
              (string= (provider-error-request-id condition)
                       "request-incomplete")
              (string= (provider-error-response-id condition) "msg-test"))
         (format nil "~A signals a contextual retryable incomplete response"
                 reason))))
    (let ((condition
            (anthropic-provider-test--consume-condition
             provider
             (list (anthropic-provider-test--message-start)
                   (anthropic-provider-test--message-delta "pause_turn")
                   (json-object "type" "message_stop")))))
      (test-assert
       (and (typep condition 'provider-error)
            (not (typep condition 'provider-retryable-error))
            (search "pause_turn" (autolith-error-message condition)))
       "pause_turn fails instead of starting an unbounded blind follow-up"))
    (let ((condition
            (anthropic-provider-test--consume-condition
             provider
             (list (anthropic-provider-test--message-start)
                   (anthropic-provider-test--message-delta "future_reason")
                   (json-object "type" "message_stop")))))
      (test-assert
       (and (typep condition 'provider-error)
            (not (typep condition 'provider-retryable-error))
            (string= (provider-error-code condition) "invalid_stream"))
       "unknown stop reasons fail explicitly instead of ending silently"))
    (let* ((credential "synthetic-credential-in-stream")
           (*provider-active-credential-values* (list credential))
           (*provider-active-credential-redaction-marker* "[REDACTED]")
           (condition
             (anthropic-provider-test--consume-condition
              provider
              (list (anthropic-provider-test--message-start)
                    (anthropic-provider-test--message-delta credential)
                    (json-object "type" "message_stop")))))
      (test-assert
       (and condition
            (not (search credential (autolith-error-message condition)))
            (not (search credential (or (provider-error-response condition) "")))
            (search "[REDACTED]" (or (provider-error-response condition) "")))
       "Anthropic protocol failures redact credentials from durable error data")))
  nil)

(-> anthropic-provider-test--stream-ordering () null)
(defun anthropic-provider-test--stream-ordering ()
  "Test that live and completed Anthropic blocks retain wire order."
  (let* ((configuration (anthropic-provider-test--configuration))
         (provider (anthropic-provider-create configuration))
         (events nil)
         (result
           (anthropic-provider-test--consume
            provider
            (list
             (anthropic-provider-test--message-start)
             (json-object "type" "content_block_start" "index" 0
                          "content_block"
                          (json-object "type" "text" "text" "first"))
             (json-object "type" "content_block_stop" "index" 0)
             (json-object "type" "content_block_start" "index" 1
                          "content_block"
                          (json-object "type" "text" "text" "second"))
             (json-object "type" "content_block_stop" "index" 1)
             (anthropic-provider-test--message-delta "end_turn")
             (json-object "type" "message_stop"))
            :event-callback (lambda (event) (push event events))))
         (items (provider-result-output-items result))
         (ordered-events (nreverse events))
         (item-events
           (remove-if-not (lambda (event) (typep event 'provider-item-event))
                          ordered-events))
         (delta-events
           (remove-if-not (lambda (event) (typep event 'assistant-delta-event))
                          ordered-events)))
    (test-assert
     (equal
      (loop for event in delta-events collect (assistant-delta-event-text event))
      '("first" "second"))
     "live assistant deltas follow Anthropic content index order")
    (test-assert
     (equal
      (loop for item in items
            collect (json-get (aref (json-get item "content") 0) "text"))
      '("first" "second"))
     "provider results retain Anthropic content index order")
    (test-assert
     (equal
      (loop for event in item-events
            for item = (provider-item-event-item event)
            collect (json-get (aref (json-get item "content") 0) "text"))
      '("first" "second"))
     "completed item events follow the same content index order"))
  nil)

(-> anthropic-provider-test--stream-lifecycle () null)
(defun anthropic-provider-test--stream-lifecycle ()
  "Test malformed Anthropic stream lifecycle and content failures."
  (let* ((configuration (anthropic-provider-test--configuration))
         (provider (anthropic-provider-create configuration))
         (start (anthropic-provider-test--message-start))
         (text-start
           (json-object "type" "content_block_start" "index" 0
                        "content_block"
                        (json-object "type" "text" "text" "")))
         (tool-start
           (json-object "type" "content_block_start" "index" 0
                        "content_block"
                        (json-object
                         "type" "tool_use"
                         "id" "tool-malformed"
                         "name" (openai-compatible--wire-tool-name "fs" "read")
                         "input" (json-object)))))
    (dolist
        (case
         (list
          (list "before message_start"
                (list text-start))
          (list "invalid message_start"
                (list
                 (json-object
                  "type" "message_start"
                  "message"
                  (json-object "id" "" "role" "assistant"
                               "content" (json-array)))))
          (list "invalid initial usage"
                (list
                 (anthropic-provider-test--message-start
                  :usage (json-object "input_tokens" "42" "output_tokens" 1))))
          (list "invalid initial usage"
                (list
                 (anthropic-provider-test--message-start
                  :usage (json-object "input_tokens" -1 "output_tokens" 1))))
          (list "invalid initial usage"
                (list
                 (anthropic-provider-test--message-start
                  :usage (json-object "input_tokens" 42))))
          (list "invalid delta usage"
                (list
                 start
                 (anthropic-provider-test--message-delta
                  "end_turn" :usage (json-object "output_tokens" "7"))))
          (list "invalid delta usage"
                (list
                 start
                 (anthropic-provider-test--message-delta
                  "end_turn" :usage (json-object "output_tokens" -1))))
          (list "invalid delta usage"
                (list
                 start
                 (anthropic-provider-test--message-delta
                  "end_turn" :usage (json-object))))
          (list "duplicate message_start"
                (list start start))
          (list "invalid content block index"
                (list start
                      (json-object "type" "content_block_start" "index" -1
                                   "content_block"
                                   (json-object "type" "text" "text" ""))))
          (list "duplicate content block"
                (list start text-start text-start))
          (list "previous block stopped"
                (list start text-start
                      (json-object "type" "content_block_start" "index" 1
                                   "content_block"
                                   (json-object "type" "text" "text" "next"))))
          (list "unopened content block"
                (list start
                      (json-object "type" "content_block_delta" "index" 0
                                   "delta"
                                   (json-object "type" "text_delta"
                                                "text" "x"))))
          (list "unsupported content block"
                (list start
                      (json-object "type" "content_block_start" "index" 0
                                   "content_block"
                                   (json-object "type" "thinking"
                                                "thinking" "hidden"))))
          (list "unsupported content block"
                (list start
                      (json-object "type" "content_block_start" "index" 0
                                   "content_block"
                                   (json-object "type" 7))))
          (list "invalid text delta content"
                (list start text-start
                      (json-object "type" "content_block_delta" "index" 0
                                   "delta"
                                   (json-object "type" "text_delta" "text" 7))))
          (list "malformed tool-use arguments"
                (list start tool-start
                      (json-object "type" "content_block_delta" "index" 0
                                   "delta"
                                   (json-object "type" "input_json_delta"
                                                "partial_json" "{"))
                      (json-object "type" "content_block_stop" "index" 0)))
          (list "open content block"
                (list start text-start
                      (anthropic-provider-test--message-delta "end_turn")))
          (list "without a stop reason"
                (list start
                      (json-object "type" "message_delta"
                                   "delta" (json-object)
                                   "usage" (json-object "output_tokens" 1))
                      (json-object "type" "message_stop")))
          (list "out-of-order content block index"
                (list start
                      (json-object "type" "content_block_start" "index" 1
                                   "content_block"
                                   (json-object "type" "text" "text" "second"))))))
      (let ((condition
              (anthropic-provider-test--consume-condition
               provider (second case)
               :headers '(("request-id" . "request-malformed")))))
        (test-assert
         (and (typep condition 'provider-error)
              (not (typep condition 'provider-retryable-error))
              (string= (provider-error-code condition) "invalid_stream")
              (string= (provider-error-request-id condition)
                       "request-malformed")
              (search (first case) (autolith-error-message condition)
                      :test #'char-equal))
         (format nil "malformed stream case signals a typed failure: ~A"
                 (first case))))))
  nil)

(-> test-anthropic-provider () null)
(defun test-anthropic-provider ()
  "Run the Anthropic provider suite."
  (anthropic-provider-test--selection)
  (anthropic-provider-test--credential-source)
  (anthropic-provider-test--request-encoding)
  (anthropic-provider-test--portable-content)
  (anthropic-provider-test--request-conversion-failures)
  (anthropic-provider-test--inherited-reference-order)
  (anthropic-provider-test--compaction-request)
  (anthropic-provider-test--transport)
  (anthropic-provider-test--stream-decoding)
  (anthropic-provider-test--stop-reasons)
  (anthropic-provider-test--stream-ordering)
  (anthropic-provider-test--stream-lifecycle)
  nil)

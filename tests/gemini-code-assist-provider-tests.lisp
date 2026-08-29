(in-package #:autolith)

;;;; -- Gemini Code Assist Provider Tests --

(-> gemini-code-assist-test--provider () gemini-code-assist-provider)
(defun gemini-code-assist-test--provider ()
  "Return an isolated Code Assist provider with an existing test manager."
  (let ((configuration (test-configuration)))
    (gemini-code-assist-provider-create
     configuration
     :credential-manager (credential-manager-create configuration))))

(-> gemini-code-assist-test--model-catalog () null)
(defun gemini-code-assist-test--model-catalog ()
  "Test stable aliases and static discovery for the private API."
  (let ((provider (gemini-code-assist-test--provider)))
    (test-assert
     (string= (gemini-code-assist-model-name "pro")
              "gemini-3-pro-preview")
     "Code Assist resolves the pro alias")
    (test-assert
     (find "gemini-2.5-flash"
           (gemini-code-assist-discover-models provider)
           :key (lambda (entry) (getf entry ':name))
           :test #'string=)
     "Code Assist discovery returns upstream model identifiers"))
  nil)

(-> gemini-code-assist-test--request-conversion () null)
(defun gemini-code-assist-test--request-conversion ()
  "Test messages, calls, results, declarations, and thinking conversion."
  (let* ((provider (gemini-code-assist-test--provider))
         (configuration (provider-configuration provider))
         (conversation (conversation-create configuration))
         (tools
           (json-array
            (json-object
             "type" "namespace"
             "name" "fs"
             "description" "Files"
             "tools"
             (json-array
              (json-object "name" "read"
                           "description" "Read a file"
                           "parameters"
                           (json-object "type" "object"
                                        "properties" (json-object))))))))
    (conversation-append-user-message conversation "hello")
    (conversation-append-provider-item
     conversation
     (json-object "type" "reasoning_content" "content" "considering"))
    (conversation-append-provider-item
     conversation
     (json-object "type" "function_call" "namespace" "fs" "name" "read"
                  "call_id" "call-1" "arguments" "{\"path\":\"a\"}"))
    (conversation-append-provider-item
     conversation
     (json-object "type" "function_call_output" "call_id" "call-1"
                  "output" "{\"text\":\"ok\"}"))
    (multiple-value-bind (request delivery)
        (provider-request-object provider conversation tools)
      (declare (ignore delivery))
      (let* ((inner (json-get request "request"))
             (contents (json-get inner "contents"))
             (declarations
               (json-get (aref (json-get inner "tools") 0)
                         "functionDeclarations"))
             (call-part
               (json-get (aref (json-get (aref contents 2) "parts") 0)
                         "functionCall"))
             (response-part
               (json-get (aref (json-get (aref contents 3) "parts") 0)
                         "functionResponse")))
        (test-assert
         (string= (json-get (aref declarations 0) "name")
                  (provider-wire-function-name--encode "fs" "read"))
         "Code Assist emits flat function declarations")
        (test-assert
         (string= (json-get call-part "name")
                  (provider-wire-function-name--encode "fs" "read"))
         "Code Assist emits function calls")
        (test-assert
         (string= (json-get response-part "name")
                  (provider-wire-function-name--encode "fs" "read"))
         "Code Assist matches function responses to calls")
        (test-assert
         (json-get (aref (json-get (aref contents 1) "parts") 0) "thought")
         "Code Assist preserves thinking parts"))))
  nil)

(-> gemini-code-assist-test--stream-fixture () null)
(defun gemini-code-assist-test--stream-fixture ()
  "Test streamed text, thinking, calls, usage, and finish handling."
  (let* ((provider (gemini-code-assist-test--provider))
         (wire-name (provider-wire-function-name--encode "fs" "read"))
         (event
           (json-object
            "traceId" "trace-1"
            "response"
            (json-object
             "candidates"
             (json-array
              (json-object
               "content"
               (json-object
                "role" "model"
                "parts"
                (json-array
                 (json-object "text" "thinking" "thought" t)
                 (json-object "text" "answer")
                 (json-object
                  "functionCall"
                  (json-object "id" "call-1" "name" wire-name
                               "args" (json-object "path" "a")))))
               "finishReason" "STOP"))
             "usageMetadata"
             (json-object "promptTokenCount" 10
                          "candidatesTokenCount" 4
                          "thoughtsTokenCount" 2
                          "totalTokenCount" 16))))
         (stream (make-string-input-stream
                  (format nil "data: ~A~%~%" (json-encode event))))
         (events nil)
         (result
           (provider-consume-stream
            provider stream nil
            (lambda (provider-event) (push provider-event events)))))
    (test-assert
     (string= (provider-result-response-id result) "trace-1")
     "Code Assist retains the trace identifier")
    (test-assert
     (= (json-get (provider-result-usage result) "input_tokens") 10)
     "Code Assist normalizes prompt usage")
    (test-assert
     (= (length (provider-result-tool-calls result)) 1)
     "Code Assist returns one portable tool call")
    (test-assert
     (string= (json-get (first (provider-result-tool-calls result)) "namespace")
              "fs")
     "Code Assist decodes the tool namespace")
    (test-assert
     (find-if (lambda (event) (typep event 'reasoning-delta-event)) events)
     "Code Assist emits thinking deltas")
    (test-assert
     (find-if (lambda (event) (typep event 'assistant-delta-event)) events)
     "Code Assist emits assistant deltas"))
  nil)

(-> gemini-code-assist-test--setup-and-retry () null)
(defun gemini-code-assist-test--setup-and-retry ()
  "Test loadCodeAssist project setup and transient non-stream retry."
  (let ((provider (gemini-code-assist-test--provider))
        (attempts 0)
        (*gemini-code-assist-nonstream-retry-delay* 0))
    (test-call-with-function-replacements
     (list
      (list
       'gemini-code-assist--post-once
       (lambda (actual-provider credentials method request)
         (declare (ignore actual-provider credentials request))
         (incf attempts)
         (if (= attempts 1)
             (values "{}" 503 nil)
             (values
              (json-encode
               (json-object "currentTier" (json-object "id" "free-tier")
                            "cloudaicompanionProject" "managed-project"))
              200
              nil)))))
     (lambda ()
       (gemini-code-assist-ensure-setup
        provider
        (make-instance 'oauth-credentials
                       :access-token "fixture-token"
                       :expires-at (+ (get-universal-time) 3600)))))
    (test-assert (= attempts 2)
                 "Code Assist retries one transient setup response")
    (test-assert
     (string= (gemini-code-assist-provider-project provider) "managed-project")
     "Code Assist records the managed project")
    (test-assert
     (string= (gemini-code-assist-provider-tier provider) "free-tier")
     "Code Assist records the selected tier"))
  nil)

(-> run-gemini-code-assist-provider-tests () null)
(defun run-gemini-code-assist-provider-tests ()
  "Run Gemini Code Assist wire and transport tests."
  (gemini-code-assist-test--model-catalog)
  (gemini-code-assist-test--request-conversion)
  (gemini-code-assist-test--stream-fixture)
  (gemini-code-assist-test--setup-and-retry)
  nil)

(in-package #:autolith)

;;;; -- Fireworks API Key Provider --

;;; The Fireworks Responses API speaks the standard streaming Responses
;;; dialect, verified against accounts/fireworks/models/kimi-k3 on
;;; 2026-08-06: input accepts developer messages, prior function calls, and
;;; function_call_output items; streams emit response.output_item.done items
;;; and response.completed without an end_turn flag; requests accept
;;; reasoning.effort, store=false, and prompt_cache_key. Like the Grok
;;; proxy, tools ride in the request's flat tools array and function calls
;;; return one flat wire name, so this provider joins Autolith's namespaced
;;; tool names with a dot on the way out and splits them again on completed
;;; items. Conversations therefore persist in the same namespaced shape
;;; regardless of the serving provider.

(defclass fireworks-api-key-provider (subscription-provider)
  ()
  (:documentation
   "A static API key client for the Fireworks Responses API."))

(defmethod provider-account-label ((provider fireworks-api-key-provider))
  "Name the Fireworks account service in user-visible failures."
  (declare (ignore provider))
  "Fireworks")

(defmethod provider-family ((provider fireworks-api-key-provider))
  "The Fireworks provider serves the Fireworks model family."
  (declare (ignore provider))
  ':fireworks)

(defmethod provider-family-create
    ((family (eql ':fireworks))
     (configuration configuration)
     &key reasoning-summaries-p)
  "Create the Fireworks API key provider; summaries always stream when served."
  (declare (ignore reasoning-summaries-p))
  (fireworks-provider-create configuration))

(-> fireworks-provider-create (configuration) fireworks-api-key-provider)
(defun fireworks-provider-create (configuration)
  "Create the Fireworks API key provider for CONFIGURATION."
  (make-instance 'fireworks-api-key-provider
                 :configuration configuration
                 :credential-manager (fireworks-credential-manager-create
                                      configuration)
                 :session-id (make-identifier)))

(defmethod provider-with-configuration
    ((provider fireworks-api-key-provider) (configuration configuration))
  "Copy PROVIDER with CONFIGURATION, retaining credentials and session state."
  (make-instance 'fireworks-api-key-provider
                 :configuration configuration
                 :credential-manager (provider-credential-manager provider)
                 :session-id (provider-session-id provider)))


;;;; -- Fireworks Request Encoding --

(defmethod provider-request-object
    ((provider fireworks-api-key-provider)
     (conversation conversation)
     (tool-namespaces vector)
     &key goal-context compaction-p)
  "Build the complete stateless Fireworks Responses request for CONVERSATION.

GOAL-CONTEXT and resolved context contributions ride as transient developer
messages exactly as they do for the subscription providers. COMPACTION-P
builds a tool-free summarization request whose trailing developer message
asks for a context checkpoint handoff. The second value is the context
delivery that the transport consumes only after a completed response."
  (let* ((configuration (provider-configuration provider))
         (effective-namespaces
           (if compaction-p #() tool-namespaces))
         (prefix (append
                  (list (responses-lite-developer-message
                         (let ((*system-prompt-hosted-web-search-p* nil))
                           (system-prompt configuration))))
                  (when (and goal-context
                             (not compaction-p))
                    (list (responses-lite-developer-message goal-context)))))
         (delivery
           (unless compaction-p
             (context-resolve-request
              configuration
              conversation
              effective-namespaces
              :goal-context goal-context)))
         (context-message
           (and delivery
                (context-delivery-rendered delivery)
                (responses-lite-developer-message
                 (context-delivery-rendered delivery))))
         (input (coerce (append prefix
                                (mapcar #'grok-wire-input-item
                                        (conversation-input-items-for-family
                                         conversation
                                         (provider-family provider)
                                         :include-ephemeral-p
                                         (not compaction-p)))
                                (when context-message
                                  (list context-message))
                                (when compaction-p
                                  (list (responses-lite-developer-message
                                         *compaction-instructions*))))
                        'vector)))
    (values
     ;; The service rejects a tool choice without tools, so a tool-free
     ;; compaction request must omit the choice rather than send "auto".
     (let ((tools (grok-wire-tools effective-namespaces)))
       (apply #'json-object
              (append
               (list "model" (configuration-model configuration)
                     "input" input
                     "tools" tools)
               (when (plusp (length tools))
                 (list "tool_choice" "auto"))
               (list "parallel_tool_calls" false
                     "reasoning"
                     (json-object
                      "effort"
                      (configuration-fireworks-wire-effort configuration))
                     "store" false
                     "stream" t
                     "prompt_cache_key"
                     (conversation-prompt-cache-key conversation)))))
     delivery)))

(defmethod provider-normalize-output-item
    ((provider fireworks-api-key-provider) (item hash-table))
  "Strip server identifiers and split flat wire names into namespaced calls."
  (call-next-method)
  (when (function-call-item-p item)
    (let* ((name (json-get item "name"))
           (dot (and (stringp name) (position #\. name))))
      (when (and dot (plusp dot) (< (1+ dot) (length name)))
        (setf (gethash "namespace" item) (subseq name 0 dot)
              (gethash "name" item) (subseq name (1+ dot))))))
  item)


;;;; -- Fireworks Transport --

(defmethod provider-open-response-stream
    ((provider fireworks-api-key-provider)
     (request hash-table)
     &key credentials conversation)
  "Open a direct authenticated SSE request to the Fireworks Responses API."
  (declare (type oauth-credentials credentials)
           (type conversation conversation))
  (dexador:post
   (configuration-provider-endpoint (provider-configuration provider))
   :headers (list
             (cons "Authorization"
                   (format nil "Bearer ~A"
                           (oauth-credentials-access-token credentials)))
             (cons "Content-Type" "application/json")
             (cons "Accept" "text/event-stream")
             (cons "User-Agent" (provider-user-agent)))
   :content (json-encode-utf8 request)
   :want-stream t
   :force-string t
   :keep-alive nil
   :connect-timeout 30
   :read-timeout 300))

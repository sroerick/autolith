(in-package #:autolith)

;;;; -- Provider Wire Protocols --

(defmethod provider-wire-protocol ((provider codex-subscription-provider))
  "Identify Codex as a standard Responses API provider."
  (declare (ignore provider))
  ':responses-api)

(defmethod provider-wire-tool
    ((provider responses-api-provider) (namespace string) (tool hash-table))
  "Encode one namespaced tool as a standard Responses function tool."
  (json-object
   "type" "function"
   "name" (provider-wire-tool-name provider namespace (json-get tool "name"))
   "description" (json-get tool "description")
   "strict" false
   "parameters" (json-get tool "parameters")))

(defmethod provider-wire-tools
    ((provider responses-api-provider) (tool-namespaces vector))
  "Flatten namespaced tools while retaining hosted tool declarations."
  (coerce
   (loop for entry across tool-namespaces
         if (and (json-object-p entry)
                 (json-string= (json-get entry "type") "namespace")
                 (vectorp (json-get entry "tools"))
                 (non-empty-string-p (json-get entry "name")))
           append (loop for tool across (json-get entry "tools")
                        when (and (json-object-p tool)
                                  (non-empty-string-p
                                   (json-get tool "name")))
                          collect (provider-wire-tool
                                   provider
                                   (json-get entry "name")
                                   tool))
         else
           collect entry)
   'vector))

(defmethod provider-wire-input-item ((provider responses-api-provider) item)
  "Flatten a namespaced function-call ITEM for a standard Responses request."
  (if (and (json-object-p item)
           (function-call-item-p item)
           (non-empty-string-p (json-get item "namespace"))
           (non-empty-string-p (json-get item "name")))
      (let ((copy (json-object-copy item)))
        (setf (gethash "name" copy)
              (provider-wire-tool-name
               provider
               (json-get item "namespace")
               (json-get item "name")))
        (remhash "namespace" copy)
        copy)
      item))

(defmethod provider-normalize-output-item
    ((provider responses-api-provider) (item hash-table))
  "Strip server identifiers and restore flat wire calls to namespaced calls."
  (call-next-method)
  (when (function-call-item-p item)
    (let* ((name (json-get item "name"))
           (dot (and (stringp name) (position #\. name))))
      (when (and dot (plusp dot) (< (1+ dot) (length name)))
        (setf (gethash "namespace" item) (subseq name 0 dot)
              (gethash "name" item) (subseq name (1+ dot))))))
  item)

(defmethod provider-normalize-output-item
    ((provider codex-subscription-provider) (item hash-table))
  "Restore standard Codex Responses calls to their local namespace shape."
  (call-next-method)
  (when (function-call-item-p item)
    (multiple-value-bind (namespace name)
        (responses-standard-tool-name->components (json-get item "name"))
      (when (and namespace name)
        (setf (gethash "namespace" item) namespace
              (gethash "name" item) name))))
  item)

(defmethod provider-request-object
    ((provider responses-api-provider)
     (conversation conversation)
     (tool-namespaces vector)
     &key goal-context compaction-p)
  "Build a standard stateless Responses API request for CONVERSATION.

Concrete providers specialize the reasoning effort, hosted tools, served
namespaces, and extra request fields. The second value is the context
delivery consumed only after a completed response."
  (let* ((configuration (provider-configuration provider))
         (hosted-tools
           (and (not compaction-p)
                (provider-responses-hosted-tools provider configuration)))
         (effective-namespaces
           (if compaction-p
               #()
               (concatenate 'vector
                            (provider-responses-request-namespaces
                             provider tool-namespaces)
                            (coerce hosted-tools 'vector))))
         (prefix
           (append
            (list (responses-developer-message
                   (let ((*system-prompt-hosted-web-search-p*
                           (not (null hosted-tools))))
                     (system-prompt configuration))))
            (when (and goal-context (not compaction-p))
              (list (responses-developer-message goal-context)))))
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
                (responses-developer-message
                 (context-delivery-rendered delivery))))
         (input
           (coerce
            (append
             prefix
             (mapcar
              (lambda (item)
                (provider-wire-input-item provider item))
              (conversation-input-items-for-family
               conversation
               (provider-family provider)
               :include-ephemeral-p (not compaction-p)))
             (when context-message
               (list context-message))
             (when compaction-p
               (list (responses-developer-message
                      *compaction-instructions*))))
            'vector))
         (tools (provider-wire-tools provider effective-namespaces)))
    (values
     (apply #'json-object
            (append
             (list "model" (configuration-model configuration)
                   "input" input
                   "tools" tools)
             (when (plusp (length tools))
               (list "tool_choice" "auto"))
             (list "parallel_tool_calls" false)
             ;; A NIL wire effort means the serving stack rejects the
             ;; reasoning parameter entirely; omit the reasoning object.
             (let ((effort
                     (provider-responses-wire-effort provider configuration))
                   (summary
                     (provider-responses-reasoning-summary
                      provider configuration)))
               (when effort
                 (list "reasoning"
                       (apply #'json-object
                              (append
                               (list "effort" effort)
                               (when summary
                                 (list "summary" summary)))))))
             (when (and *provider-maximum-output-tokens*
                        (provider-output-ceiling-p provider))
               (list "max_output_tokens" *provider-maximum-output-tokens*))
             (list "store" false
                   "stream" t)
             (provider-responses-request-fields provider conversation)))
     delivery)))

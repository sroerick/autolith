(in-package #:autolith)

;;;; -- Grok Subscription Provider --

;;; The Grok subscription proxy speaks the standard streaming Responses API,
;;; as read from grok-build reference commit 47348d13. Unlike the Codex
;;; Responses Lite dialect, tools ride in the request's flat tools array and
;;; function calls return one flat wire name, so this provider joins
;;; Autolith's namespaced tool names with a dot on the way out and splits
;;; them again on completed items. Conversations therefore persist in the
;;; same namespaced shape regardless of the serving provider.

(defclass grok-subscription-provider (responses-api-provider)
  ()
  (:documentation "A direct Grok subscription client for the xAI Responses proxy."))

(defmethod provider-account-label ((provider grok-subscription-provider))
  "Name the Grok account service in user-visible failures."
  (declare (ignore provider))
  "Grok")

(defmethod provider-family ((provider grok-subscription-provider))
  "The Grok provider serves the Grok model family."
  (declare (ignore provider))
  ':grok)

(defmethod provider-family-create
    ((family (eql ':grok))
     (configuration configuration)
     &key reasoning-summaries-p)
  "Create the direct Grok subscription provider, which has no summary switch."
  (declare (ignore reasoning-summaries-p))
  (grok-provider-create configuration))

(defmethod provider-device-authentication-client
    ((provider grok-subscription-provider))
  "Return the Grok device authentication client."
  (declare (ignore provider))
  (grok-device-authentication-client-create))

(-> grok-provider-create (configuration) grok-subscription-provider)
(defun grok-provider-create (configuration)
  "Create the direct Grok subscription provider for CONFIGURATION."
  (make-instance 'grok-subscription-provider
                 :configuration configuration
                 :credential-manager (grok-credential-manager-create
                                      configuration)
                 :session-id (make-identifier)))

(defmethod provider-with-configuration
    ((provider grok-subscription-provider) (configuration configuration))
  "Copy PROVIDER with CONFIGURATION, retaining credentials and session state."
  (make-instance 'grok-subscription-provider
                 :configuration configuration
                 :registration (model-provider-registration provider)
                 :credential-manager (provider-credential-manager provider)
                 :session-id (provider-session-id provider)))


;;;; -- Wire Tool Names --

(-> grok-wire-tool-name (string string) string)
(defun grok-wire-tool-name (namespace name)
  "Return the flat Grok wire name of NAME inside NAMESPACE."
  (format nil "~A.~A" namespace name))

(-> grok-wire-tool (string json-object) json-object)
(defun grok-wire-tool (namespace tool)
  "Return namespaced TOOL as a standard Responses function tool."
  (json-object
   "type" "function"
   "name" (grok-wire-tool-name namespace (json-get tool "name"))
   "description" (json-get tool "description")
   "strict" false
   "parameters" (json-get tool "parameters")))

(-> grok-wire-tools (vector) vector)
(defun grok-wire-tools (tool-namespaces)
  "Flatten namespaced TOOL-NAMESPACES into standard Responses function tools.

Entries that are not tool namespaces pass through unchanged so hosted tools
remain expressible."
  (coerce
   (loop for entry across tool-namespaces
         if (and (json-object-p entry)
                 (json-string= (json-get entry "type") "namespace")
                 (vectorp (json-get entry "tools")))
           append (loop for tool across (json-get entry "tools")
                        when (json-object-p tool)
                          collect (grok-wire-tool (json-get entry "name") tool))
         else
           collect entry)
   'vector))

(-> grok-wire-input-item (t) t)
(defun grok-wire-input-item (item)
  "Return ITEM with namespaced function calls flattened for the Grok wire."
  (if (and (json-object-p item)
           (function-call-item-p item)
           (non-empty-string-p (json-get item "namespace")))
      (let ((copy (json-object-copy item)))
        (setf (gethash "name" copy)
              (grok-wire-tool-name (json-get item "namespace")
                                   (json-get item "name")))
        (remhash "namespace" copy)
        copy)
      item))


;;;; -- Grok Protocol Specializations --

(defmethod provider-responses-wire-effort
    ((provider grok-subscription-provider) (configuration configuration))
  "Return CONFIGURATION's Grok reasoning effort."
  (declare (ignore provider))
  (configuration-grok-wire-effort configuration))

(defmethod provider-responses-hosted-tool
    ((provider grok-subscription-provider) (configuration configuration))
  "Return Grok's hosted tool declaration when one is enabled."
  (declare (ignore provider))
  (provider-web-search-tool configuration))

(defmethod provider-responses-request-fields
    ((provider grok-subscription-provider) (conversation conversation))
  "Include encrypted reasoning content in Grok Responses requests."
  (declare (ignore provider conversation))
  (list "include" (json-array "reasoning.encrypted_content")))


;;;; -- Grok Transport --

(-> grok--request-headers
    (grok-subscription-provider oauth-credentials conversation
     &key (:accept string))
    list)
(defun grok--request-headers (provider credentials conversation &key accept)
  "Return authenticated Grok headers for one request to CONVERSATION."
  (let ((configuration (provider-configuration provider)))
    (list
     (cons "Authorization"
           (format nil "Bearer ~A"
                   (oauth-credentials-access-token credentials)))
     (cons "X-XAI-Token-Auth" "xai-grok-cli")
     (cons "x-authenticateresponse" "authenticate-response")
     (cons "Content-Type" "application/json")
     (cons "Accept" accept)
     (cons "User-Agent" (provider-user-agent))
     (cons "x-grok-client-version" *grok-client-protocol-version*)
     (cons "x-grok-client-mode" "interactive")
     (cons "x-grok-client-identifier" "autolith")
     (cons "x-grok-session-id" (provider-session-id provider))
     (cons "x-grok-conv-id" (conversation-identifier conversation))
     (cons "x-grok-req-id" (make-identifier))
     (cons "x-grok-model-override"
           (configuration-model configuration)))))

(defmethod provider-open-response-stream
    ((provider grok-subscription-provider)
     (request hash-table)
     &key credentials conversation)
  "Open a direct authenticated SSE request to the Grok CLI chat proxy."
  (declare (type oauth-credentials credentials)
           (type conversation conversation))
  (let ((configuration (provider-configuration provider)))
    (dexador:post
     (configuration-provider-endpoint configuration)
     :headers (grok--request-headers
               provider credentials conversation :accept "text/event-stream")
     :content (json-encode-utf8 request)
     :want-stream t
     :force-string t
     :keep-alive nil
     :connect-timeout 30
     :read-timeout 300)))

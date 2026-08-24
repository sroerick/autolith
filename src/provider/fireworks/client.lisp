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

(defclass fireworks-api-key-provider
    (session-preserving-provider-mixin responses-api-provider)
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


;;;; -- Fireworks Protocol Specializations --

;; Verified 2026-08-11 against the live Fireworks Responses API: every
;; effort level Autolith can send (low, medium, high) fails for this model
;; with a 404 'Model not found, inaccessible, and/or not deployed', while
;; omitting the reasoning object succeeds.
(defparameter *fireworks-reasoning-effort-models-blacklist*
  '("accounts/fireworks/models/qwen3p7-plus")
  "Fireworks model identifiers whose serving stacks reject any reasoning
effort parameter.  Requests for these models must omit the reasoning
object entirely; see PROVIDER-RESPONSES-WIRE-EFFORT.")

(-> fireworks-model-reasoning-effort-p (string) boolean)
(defun fireworks-model-reasoning-effort-p (model)
  "Return true when MODEL accepts a reasoning effort parameter."
  (not (member model *fireworks-reasoning-effort-models-blacklist*
               :test #'string=)))


(defmethod provider-responses-wire-effort
    ((provider fireworks-api-key-provider) (configuration configuration))
  "Return CONFIGURATION's Fireworks reasoning effort, or NIL to omit it.

Models in *fireworks-reasoning-effort-models-blacklist* reject every
reasoning effort value at the serving stack, so NIL tells the shared
request builder to omit the reasoning object entirely."
  (declare (ignore provider))
  (when (fireworks-model-reasoning-effort-p
         (configuration-model configuration))
    (configuration-fireworks-wire-effort configuration)))

(defmethod provider-responses-request-fields
    ((provider fireworks-api-key-provider)
     (conversation conversation)
     &key compaction-p)
  "Add CONVERSATION's stable Fireworks prompt cache key."
  (declare (ignore provider compaction-p))
  (list "prompt_cache_key" (conversation-prompt-cache-key conversation)))


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

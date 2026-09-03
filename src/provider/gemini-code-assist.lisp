(in-package #:autolith)

;;;; -- Gemini Code Assist Protocol --

(defparameter *gemini-code-assist-endpoint*
  "https://cloudcode-pa.googleapis.com/v1internal"
  "Base endpoint for the private Gemini Code Assist subscription API.")

(defparameter *gemini-code-assist-model-aliases*
  '(("auto" . "gemini-3-pro-preview")
    ("pro" . "gemini-3-pro-preview")
    ("flash" . "gemini-3-flash-preview")
    ("flash-lite" . "gemini-3.1-flash-lite")
    ("auto-gemini-3" . "gemini-3-pro-preview")
    ("auto-gemini-2.5" . "gemini-2.5-pro")
    ("gemini-auto" . "gemini-3-pro-preview")
    ("gemini-pro" . "gemini-3-pro-preview")
    ("gemini-flash" . "gemini-3-flash-preview"))
  "Stable user-facing aliases for Code Assist model identifiers.")

(defparameter *gemini-code-assist-models*
  '((:name "gemini-3.1-flash-lite"
     :description "Gemini 3.1 Flash Lite through a Google Code Assist subscription."
     :context-window 1048576)
    (:name "gemini-3.1-pro-preview"
     :description "Gemini 3.1 Pro Preview through a Google Code Assist subscription."
     :context-window 1048576)
    (:name "gemini-3-pro-preview"
     :description "Gemini 3 Pro Preview through a Google Code Assist subscription."
     :context-window 1048576)
    (:name "gemini-3-flash-preview"
     :description "Gemini 3 Flash Preview through a Google Code Assist subscription."
     :context-window 1048576)
    (:name "gemini-3.5-flash"
     :description "Gemini 3.5 Flash through a Google Code Assist subscription."
     :context-window 1048576)
    (:name "gemini-2.5-pro"
     :description "Gemini 2.5 Pro through a Google Code Assist subscription."
     :context-window 1048576)
    (:name "gemini-2.5-flash"
     :description "Gemini 2.5 Flash through a Google Code Assist subscription."
     :context-window 1048576)
    (:name "gemini-2.5-flash-lite"
     :description "Gemini 2.5 Flash Lite through a Google Code Assist subscription."
     :context-window 1048576))
  "Known Code Assist models exposed when no model-list RPC exists.")

(defparameter *gemini-code-assist-nonstream-maximum-attempts* 4
  "Maximum attempts for transient Code Assist non-streaming RPC failures.")

(defparameter *gemini-code-assist-nonstream-retry-delay* 1
  "Seconds between transient Code Assist non-streaming RPC attempts.")

(define-condition gemini-code-assist-error (provider-error)
  ()
  (:documentation "A Gemini Code Assist protocol operation failed."))

(define-condition gemini-code-assist-setup-error (gemini-code-assist-error)
  ((stage
    :initarg :stage
    :reader gemini-code-assist-setup-error-stage
    :type keyword
    :documentation "The load, tier selection, onboarding, or operation stage."))
  (:documentation "Gemini Code Assist account setup could not complete."))

(define-condition gemini-code-assist-project-required
    (gemini-code-assist-setup-error)
  ()
  (:documentation "The Code Assist account requires a Google Cloud project."))

(define-condition gemini-code-assist-invalid-project
    (gemini-code-assist-setup-error)
  ((project
    :initarg :project
    :reader gemini-code-assist-invalid-project-project
    :type non-empty-string
    :documentation "The invalid numeric Google Cloud project identifier."))
  (:documentation "A numeric project number was supplied where a project ID is required."))

(defclass gemini-code-assist-provider
    (session-preserving-provider-mixin subscription-provider)
  ((endpoint
    :initarg :endpoint
    :initform *gemini-code-assist-endpoint*
    :reader gemini-code-assist-provider-endpoint
    :type non-empty-string
    :documentation "The v1internal Code Assist endpoint.")
   (project
    :initarg :project
    :initform nil
    :accessor gemini-code-assist-provider-project
    :type (option string)
    :documentation "The effective Code Assist companion project.")
   (tier
    :initarg :tier
    :initform nil
    :accessor gemini-code-assist-provider-tier
    :type (option string)
    :documentation "The effective Code Assist subscription tier identifier.")
   (setup-complete-p
    :initarg :setup-complete-p
    :initform nil
    :accessor gemini-code-assist-provider-setup-complete-p
    :type boolean
    :documentation "Whether loadCodeAssist and any onboarding have completed."))
  (:documentation "A direct Gemini CLI subscription provider for Code Assist."))

(-> gemini-code-assist-credential-manager-create (configuration) credential-manager)
(defgeneric gemini-code-assist-credential-manager-create (configuration)
  (:documentation "Return the OAuth credential manager used by Gemini Code Assist."))

(defmethod gemini-code-assist-credential-manager-create
    ((configuration configuration))
  "Create the installed-application OAuth manager for Code Assist."
  (gemini-credential-manager-create configuration))

(-> gemini-code-assist-provider-create
    (configuration &key
                   (:credential-manager (option credential-manager))
                   (:endpoint non-empty-string))
    gemini-code-assist-provider)
(defun gemini-code-assist-provider-create
    (configuration &key credential-manager
                          (endpoint *gemini-code-assist-endpoint*))
  "Create a Code Assist provider."
  (make-instance
   'gemini-code-assist-provider
   :configuration configuration
   :credential-manager (or credential-manager
                           (gemini-code-assist-credential-manager-create
                            configuration))
   :session-id (make-identifier)
   :endpoint endpoint))

(defmethod provider-account-label ((provider gemini-code-assist-provider))
  "Name the Google Code Assist account service."
  (declare (ignore provider))
  "Google Code Assist")

(defmethod provider-family ((provider gemini-code-assist-provider))
  "Use a dedicated family for Gemini wire history."
  (declare (ignore provider))
  ':gemini-code-assist)

(defmethod provider-output-ceiling-p ((provider gemini-code-assist-provider))
  "Gemini generationConfig accepts maxOutputTokens."
  (declare (ignore provider))
  t)

(defmethod provider-reconfiguration-initargs append
    ((provider gemini-code-assist-provider))
  "Preserve Code Assist setup state and endpoint across reconfiguration."
  (list :endpoint (gemini-code-assist-provider-endpoint provider)
        :project (gemini-code-assist-provider-project provider)
        :tier (gemini-code-assist-provider-tier provider)
        :setup-complete-p
        (gemini-code-assist-provider-setup-complete-p provider)))

(-> gemini-code-assist-model-name (string) string)
(defun gemini-code-assist-model-name (name)
  "Resolve a Code Assist model alias NAME to its wire identifier."
  (or (rest (assoc name *gemini-code-assist-model-aliases* :test #'string=))
      name))

(-> gemini-code-assist-discover-models (gemini-code-assist-provider) list)
(defun gemini-code-assist-discover-models (provider)
  "Return Code Assist's known model catalog.

The v1internal service used by Gemini CLI exposes no model-list RPC, so this
catalog follows the exact model identifiers consumed by streamGenerateContent."
  (declare (ignore provider))
  (copy-tree *gemini-code-assist-models*))


;;;; -- JSON RPC Transport --

(-> gemini-code-assist--headers (oauth-credentials non-empty-string) list)
(defun gemini-code-assist--headers (credentials accept)
  "Return authenticated Code Assist HTTP headers."
  (list (cons "Authorization"
              (format nil "Bearer ~A"
                      (oauth-credentials-access-token credentials)))
        (cons "Content-Type" "application/json")
        (cons "Accept" accept)
        (cons "User-Agent" (provider-user-agent))))

(-> gemini-code-assist--method-url
    (gemini-code-assist-provider non-empty-string) non-empty-string)
(defun gemini-code-assist--method-url (provider method)
  "Return PROVIDER's v1internal METHOD RPC URL."
  (format nil "~A:~A" (gemini-code-assist-provider-endpoint provider) method))

(-> gemini-code-assist--operation-url
    (gemini-code-assist-provider non-empty-string) non-empty-string)
(defun gemini-code-assist--operation-url (provider operation)
  "Return the long-running OPERATION URL."
  (format nil "~A/~A" (gemini-code-assist-provider-endpoint provider) operation))

(-> gemini-code-assist--transient-status-p (integer) boolean)
(defun gemini-code-assist--transient-status-p (status)
  "Return true for the non-stream retry statuses used by Gemini CLI."
  (or (= status 429)
      (= status 499)
      (<= 500 status 599)))

(-> gemini-code-assist--decode-json-response (string keyword) json-object)
(defun gemini-code-assist--decode-json-response (body stage)
  "Decode BODY as a JSON object for setup STAGE."
  (let ((value
          (handler-case
              (json-decode body)
            (error ()
              (error 'gemini-code-assist-setup-error
                     :message "Gemini Code Assist returned invalid JSON."
                     :stage stage
                     :status nil
                     :request-id nil
                     :response (bounded-string body :limit 2000))))))
    (unless (json-object-p value)
      (error 'gemini-code-assist-setup-error
             :message "Gemini Code Assist returned a non-object JSON response."
             :stage stage
             :status nil
             :request-id nil
             :response (bounded-string body :limit 2000)))
    value))

(-> gemini-code-assist--post-once
    (gemini-code-assist-provider oauth-credentials non-empty-string json-object)
    (values string integer t))
(defun gemini-code-assist--post-once (provider credentials method request)
  "Perform one non-streaming Code Assist METHOD request."
  (handler-case
      (dexador:post
       (gemini-code-assist--method-url provider method)
       :headers (gemini-code-assist--headers credentials "application/json")
       :content (json-encode-utf8 request)
       :force-string t
       :keep-alive nil
       :connect-timeout 30
       :read-timeout 300)
    (http-request-failed (condition)
      (provider-signal-http-failure provider condition))))

(-> gemini-code-assist--get-once
    (gemini-code-assist-provider oauth-credentials non-empty-string)
    (values string integer t))
(defun gemini-code-assist--get-once (provider credentials operation)
  "Perform one non-streaming Code Assist long-running operation request."
  (handler-case
      (dexador:get
       (gemini-code-assist--operation-url provider operation)
       :headers (gemini-code-assist--headers credentials "application/json")
       :force-string t
       :keep-alive nil
       :connect-timeout 30
       :read-timeout 300)
    (http-request-failed (condition)
      (provider-signal-http-failure provider condition))))

(-> gemini-code-assist--nonstream-request
    (gemini-code-assist-provider oauth-credentials keyword function)
    json-object)
(defun gemini-code-assist--nonstream-request
    (provider credentials stage request-function)
  "Run one setup RPC with Gemini CLI's bounded transient retry behavior."
  (loop for attempt from 1 to *gemini-code-assist-nonstream-maximum-attempts*
        do (handler-case
               (multiple-value-bind (body status headers)
                   (funcall request-function)
                 (cond
                   ((<= 200 status 299)
                    (return-from gemini-code-assist--nonstream-request
                      (gemini-code-assist--decode-json-response body stage)))
                   ((and (gemini-code-assist--transient-status-p status)
                         (< attempt *gemini-code-assist-nonstream-maximum-attempts*))
                    (sleep *gemini-code-assist-nonstream-retry-delay*))
                   (t
                    (provider--signal-http-status-failure
                     provider status :headers headers :raw-body body))))
             (provider-retryable-error (condition)
               (if (< attempt *gemini-code-assist-nonstream-maximum-attempts*)
                   (sleep *gemini-code-assist-nonstream-retry-delay*)
                   (error condition)))))
  (error 'gemini-code-assist-setup-error
         :message "Gemini Code Assist setup exhausted its retry budget."
         :stage stage
         :status nil
         :request-id nil
         :response nil))

(-> gemini-code-assist--post
    (gemini-code-assist-provider oauth-credentials non-empty-string keyword json-object)
    json-object)
(defun gemini-code-assist--post (provider credentials method stage request)
  "POST one non-streaming Code Assist RPC."
  (gemini-code-assist--nonstream-request
   provider credentials stage
   (lambda ()
     (gemini-code-assist--post-once provider credentials method request))))

(-> gemini-code-assist--operation
    (gemini-code-assist-provider oauth-credentials non-empty-string)
    json-object)
(defun gemini-code-assist--operation (provider credentials operation)
  "GET one Code Assist long-running OPERATION."
  (gemini-code-assist--nonstream-request
   provider credentials ':operation
   (lambda ()
     (gemini-code-assist--get-once provider credentials operation))))


;;;; -- Account Setup --

(-> gemini-code-assist--environment-project () (option string))
(defun gemini-code-assist--environment-project ()
  "Return the configured Google Cloud project ID, if any."
  (or (let ((value (uiop:getenv "GOOGLE_CLOUD_PROJECT")))
        (and (non-empty-string-p value) value))
      (let ((value (uiop:getenv "GOOGLE_CLOUD_PROJECT_ID")))
        (and (non-empty-string-p value) value))))

(-> gemini-code-assist--numeric-string-p (string) boolean)
(defun gemini-code-assist--numeric-string-p (value)
  "Return true when VALUE consists only of decimal digits."
  (and (plusp (length value)) (every #'digit-char-p value) t))

(-> gemini-code-assist--metadata (&optional (option string)) json-object)
(defun gemini-code-assist--metadata (&optional project)
  "Return Gemini CLI-compatible Code Assist client metadata."
  (let ((metadata
          (json-object "ideType" "IDE_UNSPECIFIED"
                       "platform" "PLATFORM_UNSPECIFIED"
                       "pluginType" "GEMINI")))
    (when project
      (setf (gethash "duetProject" metadata) project))
    metadata))

(-> gemini-code-assist--tier-id (json-object) (option string))
(defun gemini-code-assist--tier-id (response)
  "Return the paid or current tier ID from a load response."
  (let ((paid (json-get response "paidTier"))
        (current (json-get response "currentTier")))
    (or (and (json-object-p paid) (json-get paid "id"))
        (and (json-object-p current) (json-get current "id"))
        "standard-tier")))

(-> gemini-code-assist--default-tier (json-object) json-object)
(defun gemini-code-assist--default-tier (response)
  "Return the default allowed tier or the upstream legacy fallback."
  (let ((tiers (json-get response "allowedTiers")))
    (or (and (vectorp tiers)
             (loop for tier across tiers
                   when (and (json-object-p tier)
                             (json-get tier "isDefault"))
                     return tier))
        (json-object "id" "legacy-tier"
                     "name" ""
                     "userDefinedCloudaicompanionProject" t))))

(-> gemini-code-assist--project-required
    (json-object keyword) null)
(defun gemini-code-assist--project-required (load-response stage)
  "Signal a structured missing-project or ineligibility failure."
  (let* ((tiers (json-get load-response "ineligibleTiers"))
         (first-tier (and (vectorp tiers) (plusp (length tiers)) (aref tiers 0)))
         (reason (and (json-object-p first-tier)
                      (json-get first-tier "reasonMessage"))))
    (error 'gemini-code-assist-project-required
           :message (or (and (non-empty-string-p reason) reason)
                        "This Google account requires GOOGLE_CLOUD_PROJECT to be set.")
           :stage stage
           :status nil
           :request-id nil
           :response nil)))

(-> gemini-code-assist-ensure-setup
    (gemini-code-assist-provider oauth-credentials)
    gemini-code-assist-provider)
(defun gemini-code-assist-ensure-setup (provider credentials)
  "Load Code Assist state and onboard the account when necessary."
  (unless (gemini-code-assist-provider-setup-complete-p provider)
    (let ((project (or (gemini-code-assist-provider-project provider)
                       (gemini-code-assist--environment-project))))
      (when (and project (gemini-code-assist--numeric-string-p project))
        (error 'gemini-code-assist-invalid-project
               :message (format nil
                                "Google Cloud project ~A is numeric; Code Assist requires a project ID."
                                project)
               :project project
               :stage ':load
               :status nil
               :request-id nil
               :response nil))
      (let* ((load
               (gemini-code-assist--post
                provider credentials "loadCodeAssist" ':load
                (json-object "cloudaicompanionProject" project
                             "metadata" (gemini-code-assist--metadata project))))
             (current (json-get load "currentTier"))
             (loaded-project (json-get load "cloudaicompanionProject")))
        (if (json-object-p current)
            (progn
              (unless (or loaded-project project)
                (gemini-code-assist--project-required load ':load))
              (setf (gemini-code-assist-provider-project provider)
                    (or loaded-project project)
                    (gemini-code-assist-provider-tier provider)
                    (gemini-code-assist--tier-id load)))
            (let* ((tier (gemini-code-assist--default-tier load))
                   (tier-id (or (json-get tier "id") "standard-tier"))
                   (free-p (string= tier-id "free-tier"))
                   (onboard
                     (gemini-code-assist--post
                      provider credentials "onboardUser" ':onboard
                      (json-object
                       "tierId" tier-id
                       "cloudaicompanionProject" (unless free-p project)
                       "metadata"
                       (gemini-code-assist--metadata (unless free-p project))))))
              (loop while (and (not (json-get onboard "done"))
                               (non-empty-string-p (json-get onboard "name")))
                    do (sleep 5)
                       (setf onboard
                             (gemini-code-assist--operation
                              provider credentials (json-get onboard "name"))))
              (let* ((response (json-get onboard "response"))
                     (companion (and (json-object-p response)
                                     (json-get response
                                               "cloudaicompanionProject")))
                     (onboard-project
                       (and (json-object-p companion) (json-get companion "id"))))
                (unless (or onboard-project project)
                  (gemini-code-assist--project-required load ':onboard))
                (setf (gemini-code-assist-provider-project provider)
                      (or onboard-project project)
                      (gemini-code-assist-provider-tier provider) tier-id))))
        (setf (gemini-code-assist-provider-setup-complete-p provider) t))))
  provider)


;;;; -- Request Conversion --

(-> gemini-code-assist--wire-name (json-object) string)
(defun gemini-code-assist--wire-name (item)
  "Return ITEM's flat Gemini function name."
  (let ((namespace (json-get item "namespace"))
        (name (json-get item "name")))
    (if (non-empty-string-p namespace)
        (provider-wire-function-name--encode namespace name)
        name)))

(-> gemini-code-assist--decode-arguments (t) json-object)
(defun gemini-code-assist--decode-arguments (arguments)
  "Return function ARGUMENTS as a Gemini JSON object."
  (cond
    ((json-object-p arguments) arguments)
    ((stringp arguments)
     (let ((decoded (handler-case (json-decode arguments) (error () nil))))
       (if (json-object-p decoded)
           decoded
           (json-object "value" arguments))))
    (t
     (json-object))))

(-> gemini-code-assist--text-parts (t) vector)
(defun gemini-code-assist--text-parts (content)
  "Translate portable message CONTENT into Gemini text parts."
  (coerce
   (cond
     ((stringp content) (list (json-object "text" content)))
     ((vectorp content)
      (loop for part across content
            when (json-object-p part)
              append
              (cond
                ((non-empty-string-p (json-get part "text"))
                 (list (json-object "text" (json-get part "text"))))
                ((and (json-string= (json-get part "type") "output_text")
                      (stringp (json-get part "text")))
                 (list (json-object "text" (json-get part "text"))))
                ((and (json-string= (json-get part "type") "input_text")
                      (stringp (json-get part "text")))
                 (list (json-object "text" (json-get part "text"))))
                (t nil))))
     (t (list (json-object "text" (bounded-string content :limit 2000)))))
   'vector))

(-> gemini-code-assist--tool-result-response (t) json-object)
(defun gemini-code-assist--tool-result-response (output)
  "Translate portable tool OUTPUT into Gemini functionResponse.response."
  (cond
    ((json-object-p output) output)
    ((stringp output)
     (let ((decoded (handler-case (json-decode output) (error () nil))))
       (if (json-object-p decoded)
           decoded
           (json-object "output" output))))
    (t (json-object "output" output))))

(-> gemini-code-assist--content-items (list) vector)
(defun gemini-code-assist--content-items (items)
  "Translate portable conversation ITEMS into Gemini contents."
  (let ((contents nil)
        (call-names (make-hash-table :test #'equal)))
    (dolist (item items)
      (when (json-object-p item)
        (cond
          ((json-string= (json-get item "type") "message")
           (let ((role (json-get item "role")))
             (when (json-string-member-p role '("user" "assistant"))
               (push (json-object
                      "role" (if (string= role "assistant") "model" "user")
                      "parts" (gemini-code-assist--text-parts
                               (json-get item "content")))
                     contents))))
          ((function-call-item-p item)
           (let ((name (gemini-code-assist--wire-name item)))
             (setf (gethash (json-get item "call_id") call-names) name)
             (push
              (json-object
               "role" "model"
               "parts"
               (json-array
                (json-object
                 "functionCall"
                 (json-object "name" name
                              "args" (gemini-code-assist--decode-arguments
                                      (json-get item "arguments"))))))
              contents)))
          ((json-string= (json-get item "type") "function_call_output")
           (let ((name (or (gethash (json-get item "call_id") call-names)
                           (json-get item "name")
                           "unknown_function")))
             (push
              (json-object
               "role" "user"
               "parts"
               (json-array
                (json-object
                 "functionResponse"
                 (json-object
                  "name" name
                  "response" (gemini-code-assist--tool-result-response
                              (json-get item "output"))))))
              contents)))
          ((json-string= (json-get item "type") "reasoning_content")
           (push
            (json-object
             "role" "model"
             "parts"
             (json-array
              (json-object "text" (or (json-get item "content") "")
                           "thought" t)))
            contents)))))
    (coerce (nreverse contents) 'vector)))

(-> gemini-code-assist--context-content (list) (option json-object))
(defun gemini-code-assist--context-content (texts)
  "Return volatile nonempty TEXTS as one trailing Gemini user content."
  (let ((text
          (format nil "~{~A~^~%~%~}"
                  (remove-if-not #'non-empty-string-p texts))))
    (when (non-empty-string-p text)
      (json-object "role" "user"
                   "parts" (json-array (json-object "text" text))))))

(-> gemini-code-assist--function-declarations (vector) vector)
(defun gemini-code-assist--function-declarations (tool-namespaces)
  "Flatten Autolith tools into Gemini function declarations."
  (coerce
   (loop for entry across tool-namespaces
         append
         (cond
           ((and (json-object-p entry)
                 (json-string= (json-get entry "type") "namespace")
                 (non-empty-string-p (json-get entry "name"))
                 (vectorp (json-get entry "tools")))
            (loop for tool across (json-get entry "tools")
                  when (json-object-p tool)
                    collect
                    (json-object
                     "name" (provider-wire-function-name--encode
                             (json-get entry "name") (json-get tool "name"))
                     "description" (json-get tool "description")
                     "parameters" (json-get tool "parameters"))))
           ((and (json-object-p entry)
                 (json-string= (json-get entry "type") "function"))
            (list
             (json-object "name" (json-get entry "name")
                          "description" (json-get entry "description")
                          "parameters" (json-get entry "parameters"))))
           (t nil)))
   'vector))

(defmethod provider-request-object
    ((provider gemini-code-assist-provider)
     (conversation conversation)
     (tool-namespaces vector)
     &key goal-context compaction-p)
  "Build one Code Assist streamGenerateContent request."
  (let* ((configuration (provider-configuration provider))
         (effective-tools
           (if compaction-p
               #()
               (provider-request-tool-namespaces configuration tool-namespaces)))
         (delivery
           (unless compaction-p
             (context-resolve-request configuration conversation effective-tools
                                      :goal-context goal-context
                                      :compaction-p compaction-p)))
         (system-text
           (format nil "~{~A~^~%~%~}"
                   (remove-if-not
                    #'non-empty-string-p
                    (list (system-prompt configuration)
                          (and compaction-p *compaction-instructions*)))))
         (contents
           (gemini-code-assist--content-items
            (conversation-input-items-for-family
             conversation (provider-family provider)
             :include-ephemeral-p (not compaction-p))))
         (context-content
           (unless compaction-p
             (gemini-code-assist--context-content
              (list goal-context
                    (and delivery (context-delivery-rendered delivery))))))
         (declarations
           (gemini-code-assist--function-declarations effective-tools))
         (inner
           (json-object
            "contents"
            (if context-content
                (concatenate 'vector contents (vector context-content))
                contents)
            "systemInstruction"
            (json-object "role" "user"
                         "parts" (json-array (json-object "text" system-text)))
            "session_id" (provider-session-id provider)))
         (generation (json-object)))
    (when (plusp (length declarations))
      (setf (gethash "tools" inner)
            (json-array (json-object "functionDeclarations" declarations))))
    (when *provider-maximum-output-tokens*
      (setf (gethash "maxOutputTokens" generation)
            *provider-maximum-output-tokens*))
    (let ((effort (configuration-reasoning-effort configuration)))
      (unless (string= effort "none")
        (setf (gethash "thinkingConfig" generation)
              (json-object "includeThoughts" t))))
    (when (plusp (hash-table-count generation))
      (setf (gethash "generationConfig" inner) generation))
    (values
     (json-object
      "model" (gemini-code-assist-model-name
               (configuration-model configuration))
      "project" (gemini-code-assist-provider-project provider)
      "user_prompt_id" (make-identifier)
      "request" inner)
     delivery)))


;;;; -- Streaming Transport and Decoding --

(defmethod provider-open-response-stream
    ((provider gemini-code-assist-provider)
     (request hash-table)
     &key credentials conversation)
  "Open one authenticated Code Assist streamGenerateContent SSE response."
  (declare (ignore conversation)
           (type oauth-credentials credentials))
  (gemini-code-assist-ensure-setup provider credentials)
  (setf (gethash "project" request)
        (gemini-code-assist-provider-project provider))
  (dexador:post
   (format nil "~A?alt=sse"
           (gemini-code-assist--method-url provider "streamGenerateContent"))
   :headers (gemini-code-assist--headers credentials "text/event-stream")
   :content (json-encode-utf8 request)
   :want-stream t
   :force-string t
   :keep-alive nil
   :connect-timeout 30
   :read-timeout 300))

(-> gemini-code-assist--normalize-call
    (gemini-code-assist-provider json-object integer t) json-object)
(defun gemini-code-assist--normalize-call (provider function-call index headers)
  "Return one portable function-call item from FUNCTION-CALL."
  (let* ((wire-name (json-get function-call "name"))
         (call-id (or (json-get function-call "id")
                      (format nil "gemini-call-~D-~A" index (make-identifier))))
         (item
           (json-object
            "type" "function_call"
            "call_id" call-id
            "name" wire-name
            "arguments" (json-encode (or (json-get function-call "args")
                                         (json-object))))))
    (multiple-value-bind (namespace name)
        (provider-wire-function-name--decode wire-name)
      (when (and namespace name)
        (setf (gethash "namespace" item) namespace
              (gethash "name" item) name)))
    (when (provider--response-request-id headers)
      (setf (gethash "request_id" item)
            (provider--response-request-id headers)))
    (provider-normalize-output-item provider item)))

(-> gemini-code-assist--usage (json-object) json-object)
(defun gemini-code-assist--usage (metadata)
  "Translate Gemini usage metadata to portable usage counters."
  (let ((usage (json-object)))
    (dolist (mapping '(("promptTokenCount" . "input_tokens")
                       ("candidatesTokenCount" . "output_tokens")
                       ("totalTokenCount" . "total_tokens")
                       ("cachedContentTokenCount" . "cached_input_tokens")
                       ("thoughtsTokenCount" . "reasoning_tokens")))
      (let ((value (json-get metadata (first mapping))))
        (when (typep value '(integer 0))
          (setf (gethash (rest mapping) usage) value))))
    usage))

(-> gemini-code-assist--finish-error
    (string t (option string)) null)
(defun gemini-code-assist--finish-error (reason headers response-id)
  "Signal the typed failure represented by Gemini finish REASON."
  (error 'gemini-code-assist-error
         :message (format nil "Gemini Code Assist ended generation with ~A." reason)
         :code reason
         :status nil
         :request-id (provider--response-request-id headers)
         :response-id response-id
         :response nil))

(defmethod provider-consume-stream
    ((provider gemini-code-assist-provider) stream headers event-callback)
  "Consume Code Assist's GenerateContent SSE dialect into a provider result."
  (let ((response-id nil)
        (usage nil)
        (finish-reason nil)
        (text-stream (make-string-output-stream))
        (reasoning-stream (make-string-output-stream))
        (calls nil)
        (call-index 0))
    (loop
      for data = (provider--read-sse-data stream headers)
      until (eq data *sse-end-of-stream*)
      do (let* ((event (provider--decode-sse-data data headers))
                (error-object (and (json-object-p event) (json-get event "error"))))
           (when (json-object-p error-object)
             (error 'gemini-code-assist-error
                    :message (or (json-get error-object "message")
                                 "Gemini Code Assist returned an error event.")
                    :code (json-get error-object "status")
                    :status (json-get error-object "code")
                    :request-id (provider--response-request-id headers)
                    :response-id response-id
                    :response (bounded-string data :limit 2000)))
           (when (json-object-p event)
             (let ((trace (json-get event "traceId"))
                   (response (json-get event "response")))
               (when (non-empty-string-p trace)
                 (setf response-id trace))
               (when (json-object-p response)
                 (let ((metadata (json-get response "usageMetadata")))
                   (when (json-object-p metadata)
                     (setf usage (gemini-code-assist--usage metadata))))
                 (let ((candidates (json-get response "candidates")))
                   (when (vectorp candidates)
                     (loop for candidate across candidates
                           when (json-object-p candidate)
                             do (let ((finish (json-get candidate "finishReason"))
                                      (content (json-get candidate "content")))
                                  (when (non-empty-string-p finish)
                                    (setf finish-reason finish))
                                  (when (json-object-p content)
                                    (let ((parts (json-get content "parts")))
                                      (when (vectorp parts)
                                        (loop for part across parts
                                              when (json-object-p part)
                                                do (let ((text (json-get part "text"))
                                                         (function-call
                                                           (json-get part
                                                                     "functionCall")))
                                                     (when (stringp text)
                                                       (if (json-get part "thought")
                                                           (progn
                                                             (write-string text reasoning-stream)
                                                             (funcall event-callback
                                                                      (make-instance
                                                                       'reasoning-delta-event
                                                                       :text text)))
                                                           (progn
                                                             (write-string text text-stream)
                                                             (funcall event-callback
                                                                      (make-instance
                                                                       'assistant-delta-event
                                                                       :text text)))))
                                                     (when (json-object-p function-call)
                                                       (push
                                                        (gemini-code-assist--normalize-call
                                                         provider function-call
                                                         (incf call-index) headers)
                                                         calls))))))))))))))))
    (when (and finish-reason
               (not (member finish-reason '("STOP" "MAX_TOKENS")
                            :test #'string=)))
      (gemini-code-assist--finish-error finish-reason headers response-id))
    (let ((items nil)
          (reasoning (get-output-stream-string reasoning-stream))
          (text (get-output-stream-string text-stream)))
      (when (plusp (length reasoning))
        (push (json-object "type" "reasoning_content" "content" reasoning) items))
      (when (plusp (length text))
        (push (json-object "type" "message" "role" "assistant"
                           "content"
                           (json-array (json-object "type" "output_text"
                                                   "text" text)))
              items))
      (dolist (call (nreverse calls))
        (push call items))
      (setf items (nreverse items))
      (dolist (item items)
        (funcall event-callback (make-instance 'provider-item-event :item item)))
      (let ((turn-completion (if calls ':continue ':end)))
        (funcall event-callback
                 (make-instance 'provider-completed-event
                                :response-id response-id
                                :usage usage
                                :turn-completion turn-completion))
        (make-instance 'provider-result
                       :response-id response-id
                       :output-items items
                       :tool-calls (remove-if-not #'function-call-item-p items)
                       :usage usage
                       :turn-state nil
                       :turn-completion turn-completion)))))

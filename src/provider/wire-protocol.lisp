(in-package #:autolith)

;;;; -- Shared Function Names --

(defparameter *provider-wire-function-name-maximum-length* 64
  "Maximum function name length accepted by the shared provider wire codec.")

(-> provider-wire-function-name--valid-p (t) boolean)
(defun provider-wire-function-name--valid-p (name)
  "Return true when NAME obeys the standard provider function-name grammar."
  (and (stringp name)
       (plusp (length name))
       (<= (length name) *provider-wire-function-name-maximum-length*)
       (every (lambda (character)
                (or (and (char<= #\a character) (char<= character #\z))
                    (and (char<= #\A character) (char<= character #\Z))
                    (and (char<= #\0 character) (char<= character #\9))
                    (find character "_-")))
              name)
       t))

(-> provider-wire-function-name--encode (string string) string)
(defun provider-wire-function-name--encode (namespace name)
  "Encode NAMESPACE and NAME as one reversible standard function name."
  (let* ((payload
           (concatenate 'string namespace (string (code-char 0)) name))
         (encoded
           (string-right-trim
            "."
            (usb8-array-to-base64-string
             (sb-ext:string-to-octets payload :external-format ':utf-8)
             :uri t)))
         (wire-name (format nil "a~A" encoded)))
    (unless (provider-wire-function-name--valid-p wire-name)
      (error 'configuration-error
             :message
             (format nil
                     "Tool name ~A.~A cannot fit the provider function-name grammar."
                     namespace name)))
    wire-name))

(-> provider-wire-function-name--decode
    (string)
    (values (option string) (option string)))
(defun provider-wire-function-name--decode (wire-name)
  "Decode an Autolith provider function WIRE-NAME into namespace and name."
  (if (and (provider-wire-function-name--valid-p wire-name)
           (char= (char wire-name 0) #\a))
      (handler-case
          (let* ((decoded
                   (base64-string-to-string
                    (padded-base64url (subseq wire-name 1))
                    :uri t))
                 (separator (position (code-char 0) decoded)))
            (if (and separator
                     (plusp separator)
                     (< (1+ separator) (length decoded)))
                (values (subseq decoded 0 separator)
                        (subseq decoded (1+ separator)))
                (values nil nil)))
        (error ()
          (values nil nil)))
      (values nil nil)))


;;;; -- Responses Protocol --
(defmethod provider-wire-tool-name
    ((provider codex-subscription-provider) (namespace string) (name string))
  "Encode one Codex tool name with the shared grammar-safe wire codec."
  (declare (ignore provider))
  (provider-wire-function-name--encode namespace name))

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
        (provider-wire-function-name--decode (json-get item "name"))
      (when (and namespace name)
        (setf (gethash "namespace" item) namespace
              (gethash "name" item) name))))
  item)

(defmethod provider-responses-wire-effort
    ((provider codex-subscription-provider) configuration)
  "Return CONFIGURATION's Codex reasoning effort."
  (declare (ignore provider))
  (configuration-wire-effort configuration))

(defmethod provider-responses-reasoning-summary
    ((provider codex-subscription-provider) configuration)
  "Request automatic Codex summaries when visible reasoning is enabled."
  (declare (ignore configuration))
  (when (provider-reasoning-summaries-p provider)
    "auto"))

(defmethod provider-responses-hosted-tools
    ((provider codex-subscription-provider) configuration)
  "Return Codex's enabled hosted tool declarations."
  (declare (ignore provider))
  (let ((web-search-tool (provider-web-search-tool configuration)))
    (when web-search-tool
      (list web-search-tool))))

(defmethod provider-responses-instructions-placement
    ((provider codex-subscription-provider))
  "Place Codex instructions in the top-level Responses request field."
  (declare (ignore provider))
  ':top-level)

(defmethod provider-responses-request-fields
    ((provider codex-subscription-provider)
     (conversation conversation)
     &key compaction-p)
  "Return the fields sent by one Codex Responses request."
  (provider--codex-responses-request-fields
   provider conversation :compaction-p compaction-p))

(defmethod provider-request-object
    ((provider responses-api-provider)
     (conversation conversation)
     (tool-namespaces vector)
     &key goal-context compaction-p)
  "Build a standard stateless Responses API request for CONVERSATION.

Concrete providers specialize instruction placement, reasoning effort, hosted
tools, served namespaces, and extra request fields. The second value is the
context delivery consumed only after a completed response."
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
         (delivery
           (unless compaction-p
             (context-resolve-request
              configuration
              conversation
              effective-namespaces
              :goal-context goal-context)))
         (rendered-context
           (and delivery (context-delivery-rendered delivery)))
         (instruction-prefix
           (list
            (let ((*system-prompt-hosted-web-search-p*
                    (not (null hosted-tools))))
              (system-prompt configuration))
            (and (not compaction-p) goal-context)))
         (instruction-suffix
           (list rendered-context
                 (and compaction-p *compaction-instructions*)))
         (instruction-placement
           (provider-responses-instructions-placement provider))
         (top-level-instructions
           (case instruction-placement
             (:input
              nil)
             (:top-level
              (responses-standard-instructions
               (append instruction-prefix instruction-suffix)))
             (otherwise
              (error 'configuration-error
                     :message
                     (format nil
                             "Provider ~S returned unsupported Responses instruction placement ~S."
                             (class-name (class-of provider))
                             instruction-placement)))))
         (input-prefix
           (when (eq instruction-placement ':input)
             (mapcar #'responses-developer-message
                     (remove-if-not #'non-empty-string-p instruction-prefix))))
         (input-suffix
           (when (eq instruction-placement ':input)
             (mapcar #'responses-developer-message
                     (remove-if-not #'non-empty-string-p instruction-suffix))))
         (input
           (coerce
            (append
             input-prefix
             (mapcar
              (lambda (item)
                (provider-wire-input-item provider item))
              (conversation-input-items-for-family
               conversation
               (provider-family provider)
               :include-ephemeral-p (not compaction-p)))
             input-suffix)
            'vector))
         (tools (provider-wire-tools provider effective-namespaces)))
    (values
     (apply #'json-object
            (append
             (list "model" (configuration-model configuration))
             (when top-level-instructions
               (list "instructions" top-level-instructions))
             (list "input" input
                   "tools" tools)
             (when (plusp (length tools))
               (list "tool_choice" "auto"))
             (list "parallel_tool_calls" false)
             ;; A NIL wire effort means the serving stack rejects the
             ;; reasoning parameter entirely; omit the reasoning object.
             (let ((effort
                     (provider-responses-wire-effort provider configuration))
                   (summary
                     (and (not compaction-p)
                          (provider-responses-reasoning-summary
                           provider configuration))))
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
             (provider-responses-request-fields
              provider conversation :compaction-p compaction-p)))
     delivery)))

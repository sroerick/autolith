(in-package #:autolith)


;;;; -- Request Tool Filtering --

(-> provider-hosted-web-search-tool-p (t) boolean)
(defun provider-hosted-web-search-tool-p (tool)
  "Return true when TOOL declares provider-hosted web or social search."
  (and (json-object-p tool)
       (let ((type (json-get tool "type")))
         (and (stringp type)
              (or (uiop:string-prefix-p "web_search" type)
                  (string= type "x_search"))))
       t))

(-> provider-hosted-web-search-tools-p (list) boolean)
(defun provider-hosted-web-search-tools-p (tools)
  "Return true when TOOLS contains a provider-hosted search declaration."
  (and (some #'provider-hosted-web-search-tool-p tools) t))

(-> provider-request--without-web-run (json-object) (option json-object))
(defun provider-request--without-web-run (entry)
  "Return a copy of namespace ENTRY without web.run, or NIL when empty.

web.run is the only provider-backed web search tool. Independent web
namespace tools such as web_extra.gist page retrieval keep working without
provider search and stay advertised."
  (if (and (json-object-p entry)
           (json-string= (json-get entry "type") "namespace")
           (json-string= (json-get entry "name") "web"))
      (let ((tools
              (remove "run"
                      (coerce (json-get entry "tools") 'list)
                      :key (lambda (tool)
                             (and (json-object-p tool)
                                  (json-get tool "name")))
                      :test #'json-string=)))
        (and tools
             (json-object
              "type" (json-get entry "type")
              "name" (json-get entry "name")
              "description" (json-get entry "description")
              "tools" (coerce tools 'vector))))
      entry))

(-> provider-request-tool-namespaces
    (configuration vector &key (:hosted-web-search-p boolean))
    vector)
(defun provider-request-tool-namespaces
    (configuration tool-namespaces &key hosted-web-search-p)
  "Omit local web.run when search is disabled or a hosted search tool is served.

Independent web namespace tools, such as web_extra.gist page retrieval,
stay available because they do not depend on provider web search."
  (if (or hosted-web-search-p
          (string= (config :web-search-mode configuration) "disabled"))
      (coerce
       (loop for entry across tool-namespaces
             for filtered = (provider-request--without-web-run entry)
             when filtered
               collect filtered)
       'vector)
      tool-namespaces))


;;;; -- Responses Protocol --

(-> provider-deferred-tool-loading-p (model-provider) boolean)
(defgeneric provider-deferred-tool-loading-p (provider)
  (:documentation
   "Return true when PROVIDER supports native deferred namespace discovery."))

(defmethod provider-deferred-tool-loading-p ((provider model-provider))
  "Disable deferred discovery for providers without an explicit capability."
  (declare (ignore provider))
  nil)

(-> provider-deferred-tool-model-p (string) boolean)
(defun provider-deferred-tool-model-p (model)
  "Return true when MODEL names documented GPT-5.4 or later.

The name carries a major version and an optional dotted minor version, so
gpt-5.6-terra and gpt-6.1-sol both qualify while gpt-5.3-codex does not. The
Codex model catalog at https://github.com/openai/codex commit
444da310e108da16aaeb18fd790b0ac464f08aca marks every GPT-5.6, GPT-6, and GPT-6.1
entry with supports_search_tool."
  (handler-case
      (let* ((major-start 4)
             (major-end (and (uiop:string-prefix-p "gpt-" model)
                             (or (position-if-not #'digit-char-p model
                                                  :start major-start)
                                 (length model))))
             (major (and major-end
                         (> major-end major-start)
                         (parse-integer model
                                        :start major-start
                                        :end major-end)))
             (minor-start (and major
                               (< major-end (length model))
                               (char= (char model major-end) #\.)
                               (1+ major-end)))
             (minor-end (and minor-start
                             (position-if-not #'digit-char-p model
                                              :start minor-start)))
             (minor (cond
                      ((null major)
                       nil)
                      ((null minor-start)
                       0)
                      ((> (or minor-end (length model)) minor-start)
                       (parse-integer model
                                      :start minor-start
                                      :end minor-end))
                      (t
                       nil))))
        (and major minor
             (or (> major 5)
                 (and (= major 5) (>= minor 4)))
             t))
    (error ()
      nil)))

(defmethod provider-deferred-tool-loading-p
    ((provider codex-subscription-provider))
  "Enable native tool search on documented GPT-5.4 and later Codex models."
  (provider-deferred-tool-model-p
   (config :model (provider-configuration provider))))

(-> provider-deferred-namespace-tool (json-object) json-object)
(defun provider-deferred-namespace-tool (tool)
  "Convert one local TOOL schema to a deferred native namespace child."
  (json-object
   "type" "function"
   "name" (json-get tool "name")
   "description" (json-get tool "description")
   "strict" (json-false)
   "defer_loading" t
   "parameters" (json-get tool "parameters")))

(-> provider-deferred-namespace (json-object) json-object)
(defun provider-deferred-namespace (namespace)
  "Convert one local NAMESPACE to native deferred Responses wire form."
  (json-object
   "type" "namespace"
   "name" (json-get namespace "name")
   "description" (json-get namespace "description")
   "tools" (map 'vector #'provider-deferred-namespace-tool
                (json-get namespace "tools"))))
;;; Client-executed tool search

(defparameter *provider-tool-search-default-limit* 8
  "How many deferred tools one tool search exposes when the model sets no limit.")

(defparameter *provider-history-trimming-p* nil
  "True while history is projected for a compaction request.

Consumed tool search expansions are replayed empty there, as the Codex
reference does when trimming, and intact everywhere else so the server keeps
the expanded tools loaded without another search.")

(-> provider-tool-search-tool () json-object)
(defun provider-tool-search-tool ()
  "Return the client-executed tool_search declaration for deferred namespaces.

The client execution mode and parameter schema follow the Codex reference at
commit 18194bfd3534ca567d886eac454028dafaa68b6c: the model asks for a query and
an optional limit, Autolith answers from its own registry, and the resulting
tool_search_output replays in history so the expansion stays loaded."
  (json-object
   "type" "tool_search"
   "execution" "client"
   "description"
   (format nil "# Tool discovery~2%Searches the deferred tool namespaces by namespace name, tool name, and description, and exposes the matching tools for the next model call. Some tools are not provided upfront; find them here before calling them. A namespace name such as resource or shell loads that whole namespace. Loaded tools stay available for the rest of the conversation, so search for each namespace once.")
   "parameters"
   (json-object
    "type" "object"
    "properties"
    (json-object
     "query" (json-object
              "type" "string"
              "description" "Search terms: namespace names, tool names, or words from tool descriptions.")
     "limit" (json-object
              "type" "number"
              "description" (format nil "Maximum number of tools to return. Defaults to ~D."
                                    *provider-tool-search-default-limit*)))
    "required" (json-array "query")
    "additionalProperties" (json-false))))

(-> provider-tool-search-call-p (t) boolean)
(defun provider-tool-search-call-p (item)
  "Return true when ITEM is a tool_search_call the client must answer."
  (and (json-object-p item)
       (json-string= (json-get item "type") "tool_search_call")
       (json-string= (json-get item "execution") "client")
       t))

(-> provider-result-tool-search-calls (provider-result) list)
(defun provider-result-tool-search-calls (result)
  "Return RESULT's client-executed tool search calls in wire order."
  (remove-if-not #'provider-tool-search-call-p
                 (provider-result-output-items result)))

(-> provider-tool-search--tokens (t) list)
(defun provider-tool-search--tokens (text)
  "Return the lowercase alphanumeric words of TEXT at least two characters long."
  (if (stringp text)
      (let ((tokens '())
            (start nil))
        (loop for index from 0 to (length text)
              for boundary-p = (or (= index (length text))
                                   (not (alphanumericp (char text index))))
              do (cond
                   ((and boundary-p start)
                    (when (>= (- index start) 2)
                      (push (string-downcase (subseq text start index)) tokens))
                    (setf start nil))
                   ((and (not boundary-p) (null start))
                    (setf start index))))
        (nreverse tokens))
      '()))

(-> provider-tool-search--arguments (t) (option json-object))
(defun provider-tool-search--arguments (arguments)
  "Return the tool search ARGUMENTS as a JSON object, decoding a JSON string."
  (cond
    ((json-object-p arguments)
     arguments)
    ((stringp arguments)
     (let ((decoded (handler-case (json-decode arguments)
                      (error ()
                        nil))))
       (and (json-object-p decoded) decoded)))
    (t
     nil)))

(-> provider-tool-search--terms (json-object) list)
(defun provider-tool-search--terms (arguments)
  "Return the distinct search terms named by the tool search ARGUMENTS.

The declared query string supplies the terms. A paths array, the shape the
server-executed search used before Autolith answered searches itself, is
accepted as a list of namespace names."
  (let ((paths (json-get arguments "paths")))
    (remove-duplicates
     (append (provider-tool-search--tokens (json-get arguments "query"))
             (and (vectorp paths)
                  (not (stringp paths))
                  (loop for path across paths
                        append (provider-tool-search--tokens path))))
     :test #'string=)))

(-> provider-tool-search--limit (json-object) (integer 1))
(defun provider-tool-search--limit (arguments)
  "Return the positive tool limit requested by ARGUMENTS, or the default."
  (let ((limit (json-get arguments "limit")))
    (if (and (realp limit) (>= limit 1))
        (floor limit)
        *provider-tool-search-default-limit*)))

(-> provider-tool-search--token-match-p (string list) boolean)
(defun provider-tool-search--token-match-p (term tokens)
  "Return true when TERM equals one of TOKENS or prefixes one with 4+ characters."
  (and (some (lambda (token)
               (or (string= term token)
                   (and (>= (length term) 4)
                        (uiop:string-prefix-p term token))))
             tokens)
       t))

(-> provider-tool-search--tool-score (string json-object list) integer)
(defun provider-tool-search--tool-score (namespace-name tool terms)
  "Return how strongly TOOL in NAMESPACE-NAME matches TERMS, zero for no match.

An exact tool name outranks a name fragment, which outranks a description word.
A term naming the namespace counts for every tool inside it."
  (let ((name (or (json-get tool "name") ""))
        (name-tokens (provider-tool-search--tokens (json-get tool "name")))
        (namespace-tokens (provider-tool-search--tokens namespace-name))
        (description-tokens
          (provider-tool-search--tokens (json-get tool "description"))))
    (loop for term in terms
          sum (cond
                ((string-equal term name)
                 5)
                ((provider-tool-search--token-match-p term name-tokens)
                 3)
                ((provider-tool-search--token-match-p term namespace-tokens)
                 2)
                ((provider-tool-search--token-match-p term description-tokens)
                 1)
                (t
                 0)))))

(-> provider-tool-search--namespace-named-p (string list) boolean)
(defun provider-tool-search--namespace-named-p (namespace-name terms)
  "Return true when one of TERMS names NAMESPACE-NAME itself."
  (and (member (string-downcase namespace-name) terms :test #'string=) t))

(-> provider-tool-search (vector list &key (:limit (integer 1))) vector)
(defun provider-tool-search
    (tool-namespaces terms &key (limit *provider-tool-search-default-limit*))
  "Return the deferred namespaces of TOOL-NAMESPACES whose tools match TERMS.

At most LIMIT scored tools are exposed, best matches first, except that a term
naming a namespace exposes that whole namespace. The result keeps namespace and
tool order from TOOL-NAMESPACES and uses the deferred wire shape, so it is
valid both as a fresh tool_search_output and on every later replay."
  (let ((scored '()))
    (loop for namespace across tool-namespaces
          when (and (json-object-p namespace)
                    (json-string= (json-get namespace "type") "namespace"))
            do (let* ((namespace-name (or (json-get namespace "name") ""))
                      (named-p (provider-tool-search--namespace-named-p
                                namespace-name terms))
                      (tools (json-get namespace "tools")))
                 (when (vectorp tools)
                   (loop for tool across tools
                         for position from 0
                         when (json-object-p tool)
                           do (let ((score (provider-tool-search--tool-score
                                            namespace-name tool terms)))
                                (when (plusp score)
                                  (push (list :namespace namespace
                                              :tool tool
                                              :score score
                                              :position position
                                              :named-p named-p)
                                        scored)))))))
    (let* ((ordered (stable-sort (nreverse scored) #'> :key (lambda (entry)
                                                               (getf entry :score))))
           (exposed (loop for entry in ordered
                          for index from 0
                          when (or (getf entry :named-p) (< index limit))
                            collect entry))
           (result (make-deque)))
      (loop for namespace across tool-namespaces
            for children = (loop for entry in exposed
                                 when (eq (getf entry :namespace) namespace)
                                   collect entry)
            when children
              do (deque-push-back
                  result
                  (json-object
                   "type" "namespace"
                   "name" (json-get namespace "name")
                   "description" (json-get namespace "description")
                   "tools" (map 'vector
                                (lambda (entry)
                                  (provider-deferred-namespace-tool
                                   (getf entry :tool)))
                                (sort (copy-list children) #'<
                                      :key (lambda (entry)
                                             (getf entry :position)))))))
      (deque->vector result))))

(-> provider-tool-search-output (json-object vector) json-object)
(defun provider-tool-search-output (call tool-namespaces)
  "Return the tool_search_output answering CALL from TOOL-NAMESPACES.

The output replays in history under CALL's call_id and marks itself
client-executed, as the Codex reference does. Unreadable arguments expose no
tools rather than failing the turn."
  (let ((arguments (provider-tool-search--arguments (json-get call "arguments"))))
    (json-object
     "type" "tool_search_output"
     "call_id" (json-get call "call_id")
     "status" "completed"
     "execution" "client"
     "tools" (if arguments
                 (provider-tool-search tool-namespaces
                                       (provider-tool-search--terms arguments)
                                       :limit (provider-tool-search--limit arguments))
                 (json-array)))))

(-> provider-tool-search-namespaces (model-provider vector) vector)
(defgeneric provider-tool-search-namespaces (provider tool-namespaces)
  (:documentation
   "Return the subset of TOOL-NAMESPACES a tool search may expose for PROVIDER."))

(defmethod provider-tool-search-namespaces
    ((provider model-provider) (tool-namespaces vector))
  "Expose every offered namespace for providers without request filtering."
  (declare (ignore provider))
  tool-namespaces)

(defmethod provider-tool-search-namespaces
    ((provider responses-api-provider) (tool-namespaces vector))
  "Apply the same local tool filtering a Responses request applies."
  (let ((configuration (provider-configuration provider)))
    (provider-responses-request-namespaces
     provider
     (provider-request-tool-namespaces
      configuration tool-namespaces
      :hosted-web-search-p
      (provider-hosted-web-search-tools-p
       (provider-responses-hosted-tools provider configuration))))))

(-> provider-answer-tool-search (model-provider json-object vector) json-object)
(defun provider-answer-tool-search (provider call tool-namespaces)
  "Return PROVIDER's tool_search_output for CALL over the offered TOOL-NAMESPACES."
  (provider-tool-search-output
   call
   (provider-tool-search-namespaces provider tool-namespaces)))

(-> provider-tool-search--replay-namespace (t) (option json-object))
(defun provider-tool-search--replay-namespace (entry)
  "Return namespace ENTRY rebuilt in the deferred wire shape, or NIL to drop it.

Children keep only the fields a deferred function declares. Expansions the
server produced before Autolith answered searches itself carried a null
output_schema that the request validator rejects, so every child is rebuilt
rather than copied, and a child without an object parameter schema is dropped."
  (when (and (json-object-p entry)
             (json-string= (json-get entry "type") "namespace")
             (non-empty-string-p (json-get entry "name")))
    (let ((tools (json-get entry "tools")))
      (json-object
       "type" "namespace"
       "name" (json-get entry "name")
       "description" (or (json-get entry "description") "")
       "tools" (if (and (vectorp tools) (not (stringp tools)))
                   (coerce
                    (loop for tool across tools
                          when (and (json-object-p tool)
                                    (non-empty-string-p (json-get tool "name"))
                                    (json-object-p (json-get tool "parameters")))
                            collect (provider-deferred-namespace-tool tool))
                    'vector)
                   (json-array))))))

(-> provider-tool-search-output-replay (json-object) json-object)
(defun provider-tool-search-output-replay (item)
  "Return the tool_search_output ITEM as the next request should replay it.

History trimming replays the expansion empty. Otherwise every namespace is
rebuilt in the deferred wire shape so the server keeps those tools loaded."
  (let ((copy (json-object-copy item))
        (tools (json-get item "tools")))
    (setf (gethash "tools" copy)
          (if (or *provider-history-trimming-p*
                  (not (vectorp tools))
                  (stringp tools))
              (json-array)
              (coerce (loop for entry across tools
                            for replay = (provider-tool-search--replay-namespace entry)
                            when replay
                              collect replay)
                      'vector)))
    copy))

(defmethod provider-wire-tool-name
    ((provider codex-subscription-provider) (namespace string) (name string))
  "Encode one Codex tool name with the shared grammar-safe wire codec."
  (declare (ignore provider))
  (provider-wire-function-name--encode namespace name))

(defmethod provider-wire-tools
    ((provider codex-subscription-provider) (tool-namespaces vector))
  "Use native deferred namespaces on capable Codex models, else eager tools."
  (if (provider-deferred-tool-loading-p provider)
      (let ((deferred-p nil))
        (concatenate
         'vector
         (map 'vector
              (lambda (entry)
                (if (and (json-object-p entry)
                         (json-string= (json-get entry "type") "namespace"))
                    (progn
                      (setf deferred-p t)
                      (provider-deferred-namespace entry))
                    entry))
              tool-namespaces)
         (if deferred-p
             (json-array (provider-tool-search-tool))
             #())))
      (call-next-method)))

(defmethod provider-wire-input-item
    ((provider codex-subscription-provider) item)
  "Preserve namespace calls and replay tool search expansions intact.

The server remembers which deferred tools are loaded only through the
tool_search_output items in the request history, so an expansion must
replay with its tools on every later request of the conversation; replaying
it empty made the model search the same namespace again on every round. The
Codex reference at commit 18194bfd3534ca567d886eac454028dafaa68b6c empties the
tools only when trimming history for compaction, which
*provider-history-trimming-p* marks here."
  (cond
    ((and (json-object-p item)
          (json-string= (json-get item "type") "tool_search_output"))
     (provider-tool-search-output-replay item))
    ((and (provider-deferred-tool-loading-p provider)
          (json-object-p item)
          (function-call-item-p item)
          (non-empty-string-p (json-get item "namespace")))
     item)
    (t
     (call-next-method))))

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
  "Place Codex's stable system prompt in the top-level instructions field."
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
    ((provider responses-api-provider) (conversation conversation)
     (tool-namespaces vector)
     &key goal-context compaction-p)
  "Project product history, prompt policy, and context into a Responses request."
  (let* ((configuration (provider-configuration provider))
         (hosted-tools
           (and (not compaction-p)
                (provider-responses-hosted-tools provider configuration)))
         (hosted-web-search-p (provider-hosted-web-search-tools-p hosted-tools))
         (request-namespaces
           (provider-request-tool-namespaces configuration tool-namespaces
                                             :hosted-web-search-p hosted-web-search-p))
         (effective-namespaces
           (if compaction-p
               #()
               (concatenate 'vector
                            (provider-responses-request-namespaces provider
                                                                   request-namespaces)
                            (coerce hosted-tools 'vector))))
         (delivery
           (unless compaction-p
             (context-resolve-request configuration conversation request-namespaces
                                      :goal-context goal-context)))
         (projection
           (make-instance 'cl-llm-provider-api::wire-request :model
                          (config :model configuration) :items
                          (conversation-input-items-for-family conversation
                                                               (provider-family
                                                                provider)
                                                               :include-ephemeral-p
                                                               (not compaction-p))
                          :prefix
                          (list
                           (let ((*system-prompt-hosted-web-search-p*
                                   hosted-web-search-p))
                             (system-prompt configuration)))
                          :suffix
                          (list (and (not compaction-p) goal-context)
                                (and delivery (context-delivery-rendered delivery))
                                (and compaction-p *compaction-instructions*))
                          :options
                          (list :reasoning-effort
                                (provider-responses-wire-effort provider configuration)
                                :reasoning-summary
                                (and (not compaction-p)
                                     (provider-responses-reasoning-summary provider
                                                                           configuration))
                                :maximum-output-tokens
                                (and (provider-output-ceiling-p provider)
                                     *provider-maximum-output-tokens*)
                                :fields
                                (provider-responses-request-fields provider conversation
                                                                   :compaction-p
                                                                   compaction-p)))))
    (values
     (provider-request-object provider projection effective-namespaces :compaction-p
                              compaction-p)
     delivery)))

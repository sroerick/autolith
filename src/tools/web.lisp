(in-package #:autolith)

;;;; -- Provider Web Search --

(-> web--search-endpoint (configuration) string)
(defun web--search-endpoint (configuration)
  "Return the standalone provider search endpoint for CONFIGURATION."
  (let ((endpoint (string-right-trim '(#\/) (configuration-provider-endpoint configuration))))
    (unless (uiop:string-suffix-p endpoint "/responses")
      (error 'tool-error
             :message "The configured provider has no standalone search endpoint."
             :tool-name "web.run"))
    (format nil "~A/alpha/search"
            (subseq endpoint 0 (- (length endpoint) (length "/responses"))))))

(-> web--search-request (tool-context string) json-object)
(defun web--search-request (context query)
  "Return the standalone provider search request for QUERY."
  (let ((configuration (tool-context-configuration context)))
    (json-object
     "id" (make-identifier)
     "model" (configuration-model configuration)
     "input" (json-array
              (json-object "type" "message"
                           "role" "user"
                           "content" (json-array
                                      (json-object "type" "input_text" "text" query))))
     "commands" (json-object
                 "search_query" (json-array (json-object "q" query)))
     "settings" (json-object
                 "allowed_callers" (json-array "direct")
                 "external_web_access"
                 (string= (configuration-web-search-mode configuration) "live"))
     "max_output_tokens" 4000)))

(defmethod tool-execute ((tool web-run-tool) (context tool-context) (arguments hash-table))
  "Run one provider-backed web search and return its cited text result."
  (declare (ignore tool))
  (let* ((query (tool-argument arguments "query" :required t))
         (configuration (tool-context-configuration context))
         (provider (provider-create configuration))
         (conversation (tool-context-conversation context)))
    (unless (non-empty-string-p query)
      (error 'tool-error
             :message "web.run requires a non-empty string query."
             :tool-name "web.run"))
    (when (string= (configuration-web-search-mode configuration) "disabled")
      (error 'tool-error
             :message "Provider web search is disabled by configuration."
             :tool-name "web.run"))
    (with-credentials (credentials (provider-credential-manager provider))
      (multiple-value-bind (body status headers)
          (dexador:post (web--search-endpoint configuration)
                        :headers (provider--codex-request-headers
                                  provider credentials conversation
                                  :accept "application/json")
                        :content (json-encode (web--search-request context query)))
        (declare (ignore headers))
        (unless (= status 200)
          (error 'tool-error
                 :message (format nil "Provider web search returned HTTP ~D." status)
                 :tool-name "web.run"))
        (let ((output (json-get (json-decode body) "output")))
          (unless (non-empty-string-p output)
            (error 'tool-error
                   :message "Provider web search returned no text output."
                   :tool-name "web.run"))
          (tool-success output))))))

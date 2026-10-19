(in-package #:autolith)

;;;; -- Provider Catalog Integration --

(defun test-provider-catalog--fixture ()
  "Provide a new public model plus a non-tool model that must not enter the picker."
  (json-encode
   (json-object
    "openrouter"
    (json-object
     "models"
     (json-object
      "lab/new-agent"
      (json-object "id" "lab/new-agent" "name" "New Agent" "tool_call" t
                   "reasoning" t "reasoning_options"
                   (vector (json-object "type" "effort" "values" #("low" "high")))
                   "modalities" (json-object "output" #("text"))
                   "limit" (json-object "context" 123456))
      "lab/image"
      (json-object "id" "lab/image" "tool_call" nil
                   "modalities" (json-object "output" #("image"))
                   "limit" (json-object "context" 4096)))))))

(defun test-provider-catalog-picker ()
  "Refresh models.dev into the real registry, picker, wire route and offline startup."
  (with-test-configuration (configuration)
    (let ((snapshot (provider--registry-snapshot))
          (calls 0)
          (original (provider-registration-find "openrouter")))
      (unwind-protect
           (let ((*model-catalog-fetch-function*
                   (lambda (context)
                     (test-assert (typep context 'provider-catalog-context)
                                  "Picker discovery is independent of telemetry consent.")
                     (incf calls)
                     (test-provider-catalog--fixture))))
             (test-assert (null (provider-refresh-models configuration :provider-name "openrouter"))
                          "Public discovery succeeds without account credentials.")
             (test-assert (= calls 1) "One explicit refresh fetches the public catalog once.")
             (let ((metadata (provider-model-for "openrouter/lab/new-agent"))
                   (registration (provider-registration-find "openrouter")))
               (test-assert (and metadata (= (provider-model-context-window metadata) 123456)
                                 (equal (provider-model-reasoning-efforts metadata) '("low" "high")))
                            "Public context and reasoning metadata reach configuration validation.")
               (test-assert (equal (openrouter--wire-model-name "openrouter/lab/new-agent") "lab/new-agent")
                            "Picker names preserve exact provider wire identifiers.")
               (test-assert (and (eq (provider-registration-factory registration)
                                     (provider-registration-factory original))
                                 (eq (provider-registration-authenticator registration)
                                     (provider-registration-authenticator original)))
                            "Public metadata preserves the account transport and authentication."))
             (test-call-with-function-replacements
              (list (list 'application--credentialed-provider-names
                          (lambda (application) (declare (ignore application)) '("openrouter"))))
              (lambda ()
                (let ((items (application--model-items
                              (make-instance 'application :configuration configuration))))
                  (test-assert (find "openrouter/lab/new-agent" items
                                     :key (lambda (item) (getf item :name)) :test #'equal)
                               "A newly cataloged model is offered by the real picker.")
                  (test-assert (not (find "openrouter/lab/image" items
                                          :key (lambda (item) (getf item :name)) :test #'equal))
                               "Non-tool image models are excluded."))))
             (provider--registry-restore snapshot)
             (let ((*model-catalog-fetch-function* (lambda (context)
                                                    (declare (ignore context))
                                                    (error "Startup must remain offline."))))
               (provider-load-model-cache configuration)
               (test-assert (provider-model-for "openrouter/lab/new-agent")
                            "A fresh registry loads public metadata from the last-good disk cache."))
             (let ((*model-catalog-fetch-function* (lambda (context) (declare (ignore context)) nil)))
               (test-assert (null (provider-refresh-models configuration :provider-name "openrouter"))
                            "Warm offline refresh retains usable public metadata.")))
        (provider--registry-restore snapshot))))
  nil)

(defun test-provider-catalog-failures ()
  "Cold and malformed sources preserve built-ins; extension providers retain their discovery."
  (with-test-configuration (configuration)
    (let ((snapshot (provider--registry-snapshot))
          (original (provider-registration-find "anthropic")))
      (unwind-protect
           (progn
             (dolist (body '(nil "{}" "{\"anthropic\":{\"models\":{\"bad\":{\"id\":\"bad\"}}}}"))
               (let ((*model-catalog-fetch-function* (lambda (context) (declare (ignore context)) body)))
                 (test-assert (provider-refresh-models configuration :provider-name "anthropic")
                              "Cold unavailable or unusable catalogs report discovery failure.")
                 (test-assert (eq original (provider-registration-find "anthropic"))
                              "Failed refresh does not replace usable fallback metadata.")))
             (register-provider "anthropic" :models '("extension/model")
                                :factory (provider-registration-factory original) :source ':runtime)
             (let ((*model-catalog-fetch-function* (lambda (context)
                                                    (declare (ignore context))
                                                    (error "An extension must not fetch the public catalog."))))
               (test-assert (null (provider-refresh-models configuration :provider-name "anthropic"))
                            "A custom provider keeps its registration and explicit model list.")
               (test-assert (equal (mapcar #'provider-model-name
                                           (provider-registration-models (provider-registration-find "anthropic")))
                                   '("extension/model"))
                            "The public mapping cannot replace a higher-precedence extension.")))
        (provider--registry-restore snapshot))))
  nil)

(defun test-provider-catalog-subscriptions ()
  "Public API metadata must not invent subscription access to additional models."
  (let ((snapshot (provider--registry-snapshot)))
    (unwind-protect
         (multiple-value-bind (identifiers providers)
             (model-catalog-parse
              (json-encode
               (json-object
                "openai"
                (json-object
                 "models"
                 (let ((models (make-hash-table :test #'equal)))
                   (dolist (name (list *default-model* "api-only-model"))
                     (setf (gethash name models)
                           (json-object "id" name "tool_call" t
                                        "modalities" (json-object "output" #("text"))
                                        "limit" (json-object "context" 765432))))
                   models)))))
           (provider-catalog--install (model-catalog-create :identifiers identifiers :providers providers)
                                      (provider-registration-find "chatgpt"))
           (test-assert (= (provider-model-context-window-for *default-model*) 765432)
                        "Supported subscription models receive public context metadata.")
           (test-assert (null (provider-registration-for-model "api-only-model"))
                        "Public API membership cannot grant subscription model access."))
      (provider--registry-restore snapshot)))
  nil)

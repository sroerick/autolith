(in-package #:autolith)

;;;; -- Built-in Provider Registrations --

(-> provider--codex-registration-factory
    (configuration &key (:reasoning-summaries-p boolean))
    model-provider)
(defun provider--codex-registration-factory (configuration &key reasoning-summaries-p)
  "Create the built-in ChatGPT provider from registry metadata."
  (provider-family-create ':codex
                           configuration
                           :reasoning-summaries-p reasoning-summaries-p))

(-> provider--grok-registration-factory
    (configuration &key (:reasoning-summaries-p boolean))
    model-provider)
(defun provider--grok-registration-factory (configuration &key reasoning-summaries-p)
  "Create the built-in Grok provider from registry metadata."
  (declare (ignore reasoning-summaries-p))
  (provider-family-create ':grok configuration))

(-> provider--nous-registration-factory
    (configuration &key (:reasoning-summaries-p boolean))
    model-provider)
(defun provider--nous-registration-factory
    (configuration &key reasoning-summaries-p)
  "Create the built-in Nous Research provider from registry metadata."
  (declare (ignore reasoning-summaries-p))
  (provider-family-create ':nous configuration))

(-> provider--nous-registration-authenticator
    (model-provider &key (:stream stream) (:open-browser-p boolean))
    string)
(defun provider--nous-registration-authenticator
    (provider &key stream open-browser-p)
  "Run browser device login for the built-in Nous Research provider."
  (nous-provider-authenticate provider
                              :stream stream
                              :open-browser-p open-browser-p))

(-> provider--fireworks-registration-factory
    (configuration &key (:reasoning-summaries-p boolean))
    model-provider)
(defun provider--fireworks-registration-factory
    (configuration &key reasoning-summaries-p)
  "Create the built-in Fireworks provider from registry metadata."
  (declare (ignore reasoning-summaries-p))
  (provider-family-create ':fireworks configuration))

(-> provider--opencode-registration-factory
    (configuration &key (:reasoning-summaries-p boolean))
    model-provider)
(defun provider--opencode-registration-factory
    (configuration &key reasoning-summaries-p)
  "Create the built-in OpenCode provider from registry metadata."
  (declare (ignore reasoning-summaries-p))
  (provider-family-create ':opencode configuration))

(-> provider--opencode-registration-authenticator
    (model-provider &key (:stream stream) (:open-browser-p boolean))
    string)
(defun provider--opencode-registration-authenticator
    (provider &key stream open-browser-p)
  "Prompt for and save the built-in OpenCode provider's API key."
  (declare (ignore open-browser-p))
  (opencode-api-key-login (provider-credential-manager provider)
                           :stream (or stream *standard-output*)))

(-> provider--openrouter-registration-factory
    (configuration &key (:reasoning-summaries-p boolean))
    model-provider)
(defun provider--openrouter-registration-factory
    (configuration &key reasoning-summaries-p)
  "Create the built-in OpenRouter provider from registry metadata."
  (declare (ignore reasoning-summaries-p))
  (provider-family-create ':openrouter configuration))

(-> provider--openrouter-registration-authenticator
    (model-provider &key (:stream stream) (:open-browser-p boolean))
    string)
(defun provider--openrouter-registration-authenticator
    (provider &key stream open-browser-p)
  "Prompt for and save the built-in OpenRouter provider's API key."
  (declare (ignore open-browser-p))
  (openrouter-api-key-login (provider-credential-manager provider)
                            :stream (or stream *standard-output*)))

(-> provider--mistral-registration-factory
    (configuration &key (:reasoning-summaries-p boolean))
    model-provider)
(defun provider--mistral-registration-factory
    (configuration &key reasoning-summaries-p)
  "Create the built-in Mistral provider from registry metadata."
  (declare (ignore reasoning-summaries-p))
  (provider-family-create ':mistral configuration))

(-> provider--mistral-registration-authenticator
    (model-provider &key (:stream stream) (:open-browser-p boolean))
    string)
(defun provider--mistral-registration-authenticator
    (provider &key stream open-browser-p)
  "Prompt for and save the built-in Mistral provider's API key."
  (declare (ignore open-browser-p))
  (mistral-api-key-login (provider-credential-manager provider)
                         :stream (or stream *standard-output*)))

(-> provider--fireworks-registration-authenticator
    (model-provider &key (:stream stream) (:open-browser-p boolean))
    string)
(defun provider--fireworks-registration-authenticator
    (provider &key stream open-browser-p)
  "Prompt for and save the built-in Fireworks provider's API key."
  (declare (ignore open-browser-p))
  (fireworks-api-key-login (provider-credential-manager provider)
                           :stream (or stream *standard-output*)))

(-> provider--anthropic-registration-factory
    (configuration &key (:reasoning-summaries-p boolean))
    model-provider)
(defun provider--anthropic-registration-factory
    (configuration &key reasoning-summaries-p)
  "Create the built-in Anthropic provider from registry metadata."
  (declare (ignore reasoning-summaries-p))
  (provider-family-create ':anthropic configuration))

(-> provider--anthropic-registration-authenticator
    (model-provider &key (:stream stream) (:open-browser-p boolean))
    string)
(defun provider--anthropic-registration-authenticator
    (provider &key stream open-browser-p)
  "Prompt for and save the built-in Anthropic provider's API key."
  (declare (ignore open-browser-p))
  (anthropic-api-key-login (provider-credential-manager provider)
                           :stream (or stream *standard-output*)))


(register-provider
 "chatgpt"
 :description "ChatGPT Codex subscription"
 :family ':codex
 :protocol ':responses
 :models '("gpt-5.6-sol"
           "gpt-5.6-luna"
           "gpt-5.6-terra")
 :factory #'provider--codex-registration-factory
 :source ':builtin)

(register-provider
 "grok"
 :description "Grok subscription"
 :family ':grok
 :protocol ':responses
 :models '((:name "grok-4.6" :context-window 500000)
           (:name "grok-4.5" :context-window 500000))
 :factory #'provider--grok-registration-factory
 :source ':builtin)

(register-provider
 "nous"
 :description "Nous Research subscription"
 :family ':nous
 :protocol ':chat-completions+messages
 :endpoint (nous-chat-completions-endpoint)
 :factory #'provider--nous-registration-factory
 :authenticator #'provider--nous-registration-authenticator
 :model-discovery #'nous--fetch-models
 :model-discovery-endpoint (nous-models-endpoint)
 :model-discovery-endpoint-resolver #'nous-models-endpoint
 :source ':builtin)

(register-provider
 "fireworks"
 :description "Fireworks AI"
 :family ':fireworks
 :protocol ':responses
 :models '((:name "accounts/fireworks/models/kimi-k3"
            :context-window 1048576)
           ;; Fireworks advertises a 262k token context window.
           (:name "accounts/fireworks/models/qwen3p7-plus"
            :context-window 262144
            :reasoning-efforts ("none")))
 :factory #'provider--fireworks-registration-factory
 :authenticator #'provider--fireworks-registration-authenticator
 :endpoint *fireworks-responses-endpoint*
 :source ':builtin)

(register-provider
 "anthropic"
 :description "Anthropic API (pay-per-token)"
 :family ':anthropic
 :protocol ':messages
 :models '((:name "claude-opus-5" :context-window 200000
            :reasoning-efforts ("low" "medium" "high"))
           (:name "claude-sonnet-5" :context-window 200000
            :reasoning-efforts ("low" "medium" "high"))
           (:name "claude-haiku-4-5-20251001" :context-window 200000
            :reasoning-efforts ("low" "medium" "high")))
 :factory #'provider--anthropic-registration-factory
 :authenticator #'provider--anthropic-registration-authenticator
 :endpoint *anthropic-messages-endpoint*
 :source ':builtin)

(register-provider
 "opencode"
 :description "OpenCode (zen/go)"
 :family ':opencode
 :protocol ':chat-completions
 :endpoint *opencode-chat-completions-endpoint*
 :factory #'provider--opencode-registration-factory
 :authenticator #'provider--opencode-registration-authenticator
 :model-discovery #'opencode--fetch-models
 :model-discovery-endpoint *opencode-models-endpoint*
 :model-discovery-endpoint-resolver #'opencode-models-endpoint
 :source ':builtin)

(register-provider
 "openrouter"
 :description "OpenRouter (pay-per-token)"
 :family ':openrouter
 :protocol ':chat-completions
 :endpoint *openrouter-chat-completions-endpoint*
 :factory #'provider--openrouter-registration-factory
 :authenticator #'provider--openrouter-registration-authenticator
 :model-discovery #'openrouter--fetch-models
 :model-discovery-endpoint *openrouter-models-endpoint*
 :model-discovery-endpoint-resolver #'openrouter-models-endpoint
 :source ':builtin)

(register-provider
 "mistral"
 :description "Mistral AI"
 :family ':mistral
 :protocol ':chat-completions
 :endpoint *mistral-chat-completions-endpoint*
 :factory #'provider--mistral-registration-factory
 :authenticator #'provider--mistral-registration-authenticator
 :model-discovery #'mistral--fetch-models
 :model-discovery-endpoint *mistral-models-endpoint*
 :model-discovery-endpoint-resolver #'mistral-models-endpoint
 :source ':builtin)

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

(-> provider--fireworks-registration-factory
    (configuration &key (:reasoning-summaries-p boolean))
    model-provider)
(defun provider--fireworks-registration-factory
    (configuration &key reasoning-summaries-p)
  "Create the built-in Fireworks provider from registry metadata."
  (declare (ignore reasoning-summaries-p))
  (provider-family-create ':fireworks configuration))

(-> provider--fireworks-registration-authenticator
    (model-provider &key (:stream stream) (:open-browser-p boolean))
    string)
(defun provider--fireworks-registration-authenticator
    (provider &key stream open-browser-p)
  "Prompt for and save the built-in Fireworks provider's API key."
  (declare (ignore open-browser-p))
  (fireworks-api-key-login (provider-credential-manager provider)
                           :stream (or stream *standard-output*)))

(register-provider
 "chatgpt"
 :description "ChatGPT Codex subscription"
 :family ':codex
 :protocol ':responses-lite
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
 :models '((:name "grok-4.5" :context-window 500000))
 :factory #'provider--grok-registration-factory
 :source ':builtin)

(register-provider
 "fireworks"
 :description "Fireworks AI"
 :family ':fireworks
 :protocol ':responses
 :models '((:name "accounts/fireworks/models/kimi-k3"
            :context-window 1048576))
 :factory #'provider--fireworks-registration-factory
 :authenticator #'provider--fireworks-registration-authenticator
 :endpoint *fireworks-responses-endpoint*
 :source ':builtin)

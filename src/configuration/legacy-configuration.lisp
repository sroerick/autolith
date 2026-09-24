(in-package #:autolith)

;;;; -- Legacy Configuration API --

;;; Every definition in this file is deleted in the release named by
;;; *LEGACY-CONFIGURATION-REMOVAL-VERSION*. Internal code reads settings
;;; through CONFIG; these entry points keep user initialization files and
;;; private replay scripts working until then.

(defmacro define-legacy-configuration-readers (&rest names)
  "Define a deprecated CONFIGURATION-<name> reader for each setting in NAMES."
  `(progn
     ,@(loop for name in names
             collect
             (let ((function (intern (format nil "CONFIGURATION-~A" (symbol-name name))))
                   (replacement (format nil "(config ~(~S~) configuration)" name)))
               `(define-deprecated-function ,function (configuration) ,replacement
                  ,(format nil "Return CONFIGURATION's ~(~A~) setting." name)
                  (config ,name configuration))))))

(define-legacy-configuration-readers
  :source-root :working-directory :config-root :data-root :state-root
  :cache-root :codex-auth-path :grok-bootstrap-auth-path :model
  :reasoning-effort :codex-fast-mode-p :fullscreen-p :immutable-p
  :management-repl-enabled-p :management-repl-transport
  :management-repl-unix-socket-path :management-repl-tcp-address
  :management-repl-tcp-port :management-repl-token-file-path
  :management-repl-evaluation-timeout :management-repl-maximum-frame-size
  :management-repl-maximum-source-size :management-repl-maximum-output-size
  :management-repl-queue-capacity :management-repl-maximum-clients
  :management-repl-authentication-timeout :web-search-mode :context-window
  :compaction-threshold-percent :provider-endpoint)

(define-deprecated-function configuration-with-working-directory (configuration location)
    "(configuration-copy configuration :working-directory location)"
  "Copy CONFIGURATION with its workspace changed to existing directory LOCATION."
  (configuration-copy configuration :working-directory location))

(define-deprecated-function configuration-with-reasoning-effort (configuration reasoning-effort)
    "(configuration-copy configuration :reasoning-effort reasoning-effort)"
  "Copy CONFIGURATION with only its REASONING-EFFORT changed."
  (configuration-copy configuration :reasoning-effort reasoning-effort))

(define-deprecated-function configuration-with-codex-fast-mode (configuration enabled-p)
    "(configuration-copy configuration :codex-fast-mode-p enabled-p)"
  "Copy CONFIGURATION with Codex Fast mode set to ENABLED-P."
  (configuration-copy configuration :codex-fast-mode-p enabled-p))

(define-deprecated-function configuration-with-fullscreen (configuration enabled-p)
    "(configuration-copy configuration :fullscreen-p enabled-p)"
  "Copy CONFIGURATION with fullscreen terminal UI selection ENABLED-P."
  (configuration-copy configuration :fullscreen-p enabled-p))

(define-deprecated-function configuration-with-model (configuration model)
    "(configuration-copy configuration :model model)"
  "Copy CONFIGURATION with only its MODEL changed."
  (configuration-copy configuration :model model))

(define-deprecated-function configuration--clone
    (configuration &rest overrides &key working-directory model reasoning-effort
                   fullscreen-p codex-fast-mode-p immutable-p web-search-mode)
    "(configuration-copy configuration ...)"
  "Copy CONFIGURATION, replacing only the supplied choices."
  (declare (ignore working-directory model reasoning-effort fullscreen-p
                   codex-fast-mode-p immutable-p web-search-mode))
  (apply #'configuration-copy configuration overrides))

(define-deprecated-function configuration--management-repl-initargs (configuration)
    "(config :management-repl-<setting> configuration)"
  "Return CONFIGURATION's management endpoint settings as constructor initargs."
  (loop for name in '(:management-repl-enabled-p :management-repl-transport
                      :management-repl-unix-socket-path :management-repl-tcp-address
                      :management-repl-tcp-port :management-repl-token-file-path
                      :management-repl-evaluation-timeout
                      :management-repl-maximum-frame-size
                      :management-repl-maximum-source-size
                      :management-repl-maximum-output-size
                      :management-repl-queue-capacity :management-repl-maximum-clients
                      :management-repl-authentication-timeout)
        append (list name (config name configuration))))

(in-package #:autolith)

;;;; -- Durable Settings Tests --

(-> preferences-tests--without-model-environment (function) t)
(defun preferences-tests--without-model-environment (function)
  "Call FUNCTION while model, effort, and Fast mode overrides are absent."
  (with-test-environment (("AUTOLITH_MODEL" nil)
                          ("AUTOLITH_REASONING_EFFORT" nil)
                          ("AUTOLITH_CODEX_FAST_MODE" nil))
    (funcall function)))

(-> preferences-tests--create (configuration &rest t) configuration)
(defun preferences-tests--create (configuration &rest overrides)
  "Create a process configuration on CONFIGURATION's roots, reading its preferences."
  (apply #'configuration-create
         :source-root (config :source-root configuration)
         :working-directory (config :working-directory configuration)
         :config-root (config :config-root configuration)
         :data-root (config :data-root configuration)
         :state-root (config :state-root configuration)
         :cache-root (config :cache-root configuration)
         :codex-auth-path (config :codex-auth-path configuration)
         :grok-bootstrap-auth-path (config :grok-bootstrap-auth-path configuration)
         overrides))

(-> preferences-tests--file-plist (configuration) (values list boolean))
(defun preferences-tests--file-plist (configuration)
  "Return the durable plist in CONFIGURATION's preferences file and whether it is version 8."
  (multiple-value-bind (form sole-form-p)
      (snapshot-read (configuration-preferences-path configuration))
    (values (preferences--form->plist form)
            (and sole-form-p (eql (getf (rest form) :version) 8) t))))

(-> preferences-tests--warned-p (function) boolean)
(defun preferences-tests--warned-p (function)
  "Return true when FUNCTION signals a preferences load warning."
  (let ((warned-p nil))
    (handler-bind ((preferences-load-warning
                     (lambda (warning)
                       (setf warned-p t)
                       (muffle-warning warning))))
      (funcall function))
    warned-p))

(-> test-preferences () null)
(defun test-preferences ()
  "Test durable settings persist, merge, migrate, and recover from damage."
  (preferences-tests--without-model-environment
   (lambda ()
     (with-test-configuration (configuration)
       (let ((pathname (configuration-preferences-path configuration)))
         (test-assert
          (equal pathname
                 (merge-pathnames "preferences.sexp" (config :state-root configuration)))
          "global preferences live under the state root")
         (let ((created (preferences-tests--create configuration)))
           (dolist (case '((:reasoning-traces-p nil) (:compact-view-p t)
                           (:turn-timestamps-p nil) (:cache-miss-notices-p nil)
                           (:simple-technical-english-p nil)
                           (:session-title-generation-p t) (:fullscreen-p nil)
                           (:codex-fast-mode-p nil) (:permission-mode nil)))
             (destructuring-bind (name expected) case
               (test-assert (eq (config name created) expected)
                            (format nil "a missing file leaves ~(~A~) at its default" name))))
           (test-assert (string= (config :model created) *default-model*)
                        "a missing file leaves the default model")
           (test-assert (null (configuration-setting-source created :model))
                        "a default value records no source")
           (test-assert (not (probe-file pathname))
                        "reading defaults does not create the file")
           (setf (config :reasoning-traces-p created) t)
           (multiple-value-bind (plist version-8-p)
               (preferences-tests--file-plist configuration)
             (test-assert (and version-8-p (eq (getf plist :reasoning-traces-p) t))
                          "a durable change writes a version 8 record"))
           (test-assert (eq (configuration-setting-source created :reasoning-traces-p) ':session)
                        "an interactive change records the session source")
           (setf (config :model created) "gpt-5.6-luna"
                 (config :reasoning-effort created) "high"
                 (config :permission-mode created) ':ask)
           (let ((reloaded (preferences-tests--create configuration)))
             (test-assert (and (config :reasoning-traces-p reloaded)
                               (string= (config :model reloaded) "gpt-5.6-luna")
                               (string= (config :reasoning-effort reloaded) "high")
                               (eq (config :permission-mode reloaded) ':ask))
                          "durable values survive into a new configuration")
             (test-assert (eq (configuration-setting-source reloaded :model) ':durable)
                          "a value read from the file records the durable source"))
           (setf (config :permission-mode created) nil)
           (test-assert (null (config :permission-mode (preferences-tests--create configuration)))
                        "an optional choice can be unset durably")
           (test-assert
            (handler-case (progn (setf (config :permission-mode created) ':sandboxed) nil)
              (configuration-error () t))
            "session-only permission modes cannot be saved")
           (test-assert
            (handler-case (progn (setf (config :context-window created) 1) nil)
              (configuration-error () t))
            "derived settings cannot be set"))
         (with-test-environment (("AUTOLITH_MODEL" "gpt-5.6-terra")
                                 ("AUTOLITH_CODEX_FAST_MODE" "on"))
           (let ((created (preferences-tests--create configuration)))
             (test-assert (string= (config :model created) "gpt-5.6-terra")
                          "the environment beats the durable model")
             (test-assert (eq (configuration-setting-source created :model) ':environment)
                          "an environment value records its source")
             (test-assert (config :codex-fast-mode-p created)
                          "the environment can enable Codex Fast mode"))
           (test-assert
            (not (config :codex-fast-mode-p
                         (preferences-tests--create configuration :codex-fast-mode-p nil)))
            "an explicit choice beats the environment"))
         (with-test-environment (("AUTOLITH_CODEX_FAST_MODE" "invalid"))
           (test-assert
            (handler-case (progn (preferences-tests--create configuration) nil)
              (configuration-error () t))
            "invalid Codex Fast mode environment values are rejected"))
         (snapshot-write pathname
                         '(:preferences :version 8
                           :model "gpt-5.6-typo" :reasoning-effort "bogus"
                           :compact-view-p nil :future-key 7))
         (let ((created (preferences-tests--create configuration)))
           (test-assert (string= (config :model created) *default-model*)
                        "an unsupported durable model is dropped")
           (test-assert (string= (config :reasoning-effort created)
                                 *default-reasoning-effort*)
                        "an unsupported durable effort is dropped")
           (test-assert (not (config :compact-view-p created))
                        "valid durable values beside dropped ones still apply")
           (setf (config :turn-timestamps-p created) t)
           (let ((plist (preferences-tests--file-plist configuration)))
             (test-assert (and (= (getf plist :future-key) 7)
                               (not (getf plist :compact-view-p))
                               (eq (getf plist :turn-timestamps-p) t))
                          "storing one value keeps unknown and unrelated keys"))
           (snapshot-write pathname '(:preferences :version 8 :cache-miss-notices-p t))
           (setf (config :fullscreen-p created) t)
           (let ((plist (preferences-tests--file-plist configuration)))
             (test-assert (and (eq (getf plist :cache-miss-notices-p) t)
                               (eq (getf plist :fullscreen-p) t)
                               (null (member :turn-timestamps-p plist)))
                          "storing merges into the file as another process left it")))
         (snapshot-write pathname
                         '(:preferences :version 3
                           :model "gpt-5.6-luna" :reasoning-effort "high"
                           :reasoning-traces-p t :compact-view-p nil
                           :turn-timestamps-p t))
         (let ((created (preferences-tests--create configuration)))
           (test-assert (and (string= (config :model created) "gpt-5.6-luna")
                             (string= (config :reasoning-effort created) "high")
                             (config :reasoning-traces-p created)
                             (not (config :compact-view-p created))
                             (config :turn-timestamps-p created)
                             (not (config :codex-fast-mode-p created))
                             (config :session-title-generation-p created))
                        "version three preferences remain readable with their defaults")
           (multiple-value-bind (plist version-8-p)
               (preferences-tests--file-plist configuration)
             (test-assert (and version-8-p
                               (not (getf plist :compact-view-p))
                               (getf plist :turn-timestamps-p)
                               (member :cache-miss-notices-p plist)
                               (getf plist :session-title-generation-p))
                          "version three preferences migrate to version eight")))
         (with-open-file (stream pathname :direction ':output :if-exists ':supersede
                                          :external-format ':utf-8)
           (prin1 '(:preferences :version 1 :reasoning-traces-p t) stream)
           (terpri stream))
         (let ((created (preferences-tests--create configuration)))
           (test-assert (and (config :reasoning-traces-p created)
                             (string= (config :model created) *default-model*)
                             (config :compact-view-p created))
                        "version one preferences remain readable")
           (test-assert (nth-value 1 (preferences-tests--file-plist configuration))
                        "version one preferences migrate to version eight"))
         (snapshot-write pathname
                         '(:preferences :version 7
                           :model "gpt-5.6-sol" :reasoning-effort "ultra"
                           :codex-fast-mode-p nil :reasoning-traces-p nil
                           :compact-view-p t :turn-timestamps-p nil
                           :cache-miss-notices-p nil :simple-technical-english-p nil
                           :session-title-generation-p t :permission-mode nil))
         (let ((created nil))
           (test-assert
            (preferences-tests--warned-p
             (lambda () (setf created (preferences-tests--create configuration))))
            "a version seven record missing fullscreen is rejected with a warning")
           (test-assert (not (config :fullscreen-p created))
                        "a rejected file leaves defaults in place"))
         (with-open-file (stream pathname :direction ':output :if-exists ':supersede
                                          :external-format ':utf-8)
           (write-string "(:preferences :version 8 :model" stream))
         (let ((created nil))
           (test-assert
            (preferences-tests--warned-p
             (lambda () (setf created (preferences-tests--create configuration))))
            "a truncated file is reported and ignored")
           (setf (config :compact-view-p created) nil)
           (test-assert (equal (preferences-tests--file-plist configuration)
                               '(:compact-view-p nil))
                        "the next store replaces a damaged file"))
         (let ((created (preferences-tests--create configuration)))
           (preferences-set-fullscreen created t)
           (test-assert (and (config :fullscreen-p created)
                             (preference-state-fullscreen-p (preferences-load created))
                             (preferences-fullscreen-p created))
                        "legacy preference entry points still read and write settings")
           (preferences-set-model-selection created)
           (test-assert (string= (getf (preferences-tests--file-plist configuration) :model)
                                 (config :model created))
                        "the legacy model selection writer persists the current model"))))))
  nil)

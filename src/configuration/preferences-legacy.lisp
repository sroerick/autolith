(in-package #:autolith)

;;;; -- Legacy Preferences --

;;; Everything in this file is deleted in the release named by
;;; *LEGACY-CONFIGURATION-REMOVAL-VERSION*: the versions 1 to 7 reader that
;;; migrates older files, the preference-state object those versions
;;; described, and the entry points user initialization may still call.

(defclass preference-state ()
  ((model
    :initarg :model
    :initform nil
    :reader preference-state-model
    :type (option non-empty-string)
    :documentation "The last interactively selected provider model, if any.")
   (reasoning-effort
    :initarg :reasoning-effort
    :initform nil
    :reader preference-state-reasoning-effort
    :type (option non-empty-string)
    :documentation "The last interactively selected reasoning effort, if any.")
   (codex-fast-mode-p
    :initarg :codex-fast-mode-p
    :initform nil
    :reader preference-state-codex-fast-mode-p
    :type boolean
    :documentation "Whether Codex Fast mode is enabled for future requests.")
   (reasoning-traces-p
    :initarg :reasoning-traces-p
    :initform nil
    :reader preference-state-reasoning-traces-p
    :type boolean
    :documentation "Whether provider reasoning summaries are requested and shown.")
   (compact-view-p
    :initarg :compact-view-p
    :initform t
    :reader preference-state-compact-view-p
    :type boolean
    :documentation "Whether verbose tool calls are condensed and routine results hidden.")
   (turn-timestamps-p
    :initarg :turn-timestamps-p
    :initform nil
    :reader preference-state-turn-timestamps-p
    :type boolean
    :documentation "Whether transcript turn headers include local timestamps.")
   (cache-miss-notices-p
    :initarg :cache-miss-notices-p
    :initform nil
    :reader preference-state-cache-miss-notices-p
    :type boolean
    :documentation "Whether provider requests that re-read uncached context are reported.")
   (simple-technical-english-p
    :initarg :simple-technical-english-p
    :initform nil
    :reader preference-state-simple-technical-english-p
    :type boolean
    :documentation "Whether natural-language replies use Simple Technical English.")
   (session-title-generation-p
    :initarg :session-title-generation-p
    :initform t
    :reader preference-state-session-title-generation-p
    :type boolean
    :documentation "Whether the provider may refresh locally derived session titles.")
   (fullscreen-p
    :initarg :fullscreen-p
    :initform nil
    :reader preference-state-fullscreen-p
    :type boolean
    :documentation "Whether interactive sessions use the fullscreen terminal UI.")
   (permission-mode
    :initarg :permission-mode
    :initform nil
    :reader preference-state-permission-mode
    :type (option (member :ask :auto))
    :documentation "The last saved durable command-permission mode, or NIL when unset."))
  (:documentation "The durable choices as versions 1 to 7 of the preferences file described them."))

(defparameter *preferences-legacy-record-fields*
  (list (list :indicator ':reasoning-traces-p
              :validate (lambda (value) (typep value 'boolean))
              :required t)
        (list :indicator ':model
              :validate (lambda (value)
                          (or (null value) (non-empty-string-p value)))
              :required '(2 3 4 5 6 7))
        (list :indicator ':reasoning-effort
              :validate (lambda (value)
                          (or (null value) (non-empty-string-p value)))
              :required '(2 3 4 5 6 7))
        (list :indicator ':compact-view-p
              :validate (lambda (value) (typep value 'boolean))
              :required '(3 4 5 6 7))
        (list :indicator ':turn-timestamps-p
              :validate (lambda (value) (typep value 'boolean)))
        (list :indicator ':cache-miss-notices-p
              :validate (lambda (value) (typep value 'boolean))
              :required '(6 7))
        (list :indicator ':simple-technical-english-p
              :validate (lambda (value) (typep value 'boolean)))
        (list :indicator ':codex-fast-mode-p
              :validate (lambda (value) (typep value 'boolean))
              :required '(5 6 7))
        (list :indicator ':session-title-generation-p
              :validate (lambda (value) (typep value 'boolean))
              :required '(4 5 6 7))
        (list :indicator ':fullscreen-p
              :validate (lambda (value) (typep value 'boolean))
              :required '(7))
        (list :indicator ':permission-mode
              :validate (lambda (value)
                          (member value '(nil :ask :auto) :test #'eq))))
  "Versioned field descriptions for the versions 1 to 7 preferences record.")

(-> preferences-legacy-form-p (t) boolean)
(defun preferences-legacy-form-p (form)
  "Return true when FORM is one complete versions 1 to 7 preferences record."
  (values (record-check form
                        :tag ':preferences
                        :versions '(1 2 3 4 5 6 7)
                        :fields *preferences-legacy-record-fields*)))

(-> preferences-legacy-form->plist (list) list)
(defun preferences-legacy-form->plist (form)
  "Return the durable values a versions 1 to 7 FORM carries, with their defaults.

Every field the old record could describe is present in the result, so the
rewritten version 8 file carries the same choices the old one implied."
  (let ((properties (rest form)))
    (list :model (getf properties :model)
          :reasoning-effort (getf properties :reasoning-effort)
          :codex-fast-mode-p (getf properties :codex-fast-mode-p nil)
          :reasoning-traces-p (getf properties :reasoning-traces-p nil)
          :compact-view-p (getf properties :compact-view-p t)
          :turn-timestamps-p (getf properties :turn-timestamps-p nil)
          :cache-miss-notices-p (getf properties :cache-miss-notices-p nil)
          :simple-technical-english-p
          (getf properties :simple-technical-english-p nil)
          :session-title-generation-p
          (getf properties :session-title-generation-p t)
          :fullscreen-p (getf properties :fullscreen-p nil)
          :permission-mode (getf properties :permission-mode))))

(define-deprecated-function preferences-load (configuration)
    "(config :<setting> configuration)"
  "Return CONFIGURATION's durable choices as a legacy preference state."
  (make-instance 'preference-state
                 :model (config :model configuration)
                 :reasoning-effort (config :reasoning-effort configuration)
                 :codex-fast-mode-p (config :codex-fast-mode-p configuration)
                 :reasoning-traces-p (config :reasoning-traces-p configuration)
                 :compact-view-p (config :compact-view-p configuration)
                 :turn-timestamps-p (config :turn-timestamps-p configuration)
                 :cache-miss-notices-p (config :cache-miss-notices-p configuration)
                 :simple-technical-english-p
                 (config :simple-technical-english-p configuration)
                 :session-title-generation-p
                 (config :session-title-generation-p configuration)
                 :fullscreen-p (config :fullscreen-p configuration)
                 :permission-mode (config :permission-mode configuration)))

(defmacro define-legacy-preference-accessors (&rest entries)
  "Define deprecated PREFERENCES-<getter> and PREFERENCES-SET-<name> pairs.

Each entry is (GETTER SETTER SETTING)."
  `(progn
     ,@(loop for (getter setter setting) in entries
             append
             (list
              `(define-deprecated-function ,getter (configuration)
                   ,(format nil "(config ~(~S~) configuration)" setting)
                 ,(format nil "Return CONFIGURATION's ~(~A~) setting." setting)
                 (config ,setting configuration))
              `(define-deprecated-function ,setter (configuration value)
                   ,(format nil "(setf (config ~(~S~) configuration) value)" setting)
                 ,(format nil "Persist VALUE as CONFIGURATION's ~(~A~) setting." setting)
                 (setf (config ,setting configuration) value)
                 nil)))))

(define-legacy-preference-accessors
  (preferences-reasoning-traces-p preferences-set-reasoning-traces :reasoning-traces-p)
  (preferences-compact-view-p preferences-set-compact-view :compact-view-p)
  (preferences-turn-timestamps-p preferences-set-turn-timestamps :turn-timestamps-p)
  (preferences-cache-miss-notices-p preferences-set-cache-miss-notices :cache-miss-notices-p)
  (preferences-simple-technical-english-p preferences-set-simple-technical-english
   :simple-technical-english-p)
  (preferences-session-title-generation-p preferences-set-session-title-generation
   :session-title-generation-p)
  (preferences-fullscreen-p preferences-set-fullscreen :fullscreen-p)
  (preferences-permission-mode preferences-set-permission-mode :permission-mode))

(define-deprecated-function preferences-set-codex-fast-mode (configuration enabled-p)
    "(setf (config :codex-fast-mode-p configuration) enabled-p)"
  "Persist ENABLED-P as CONFIGURATION's Codex Fast mode."
  (setf (config :codex-fast-mode-p configuration) enabled-p)
  nil)

(define-deprecated-function preferences-set-model-selection (configuration)
    "(setf (config :model configuration) model)"
  "Persist CONFIGURATION's current model and reasoning effort."
  (setf (config :model configuration) (config :model configuration)
        (config :reasoning-effort configuration)
        (config :reasoning-effort configuration))
  nil)

(define-deprecated-function preferences-apply-model-selection (configuration)
    "configuration-create, which applies durable values itself"
  "Return CONFIGURATION; durable model choices already applied at creation."
  configuration)

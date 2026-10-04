(in-package #:autolith)

;;;; -- Command Permission Tests --

(-> test-command-permission-persistence () null)
(defun test-command-permission-persistence ()
  "Test exact command approvals persist, remain directory-scoped, and clear."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (other (merge-pathnames "other/" root))
         (state (permissions-load configuration)))
    (unwind-protect
         (progn
           (test-assert
            (equal (configuration-permissions-path configuration)
                   (merge-pathnames "permissions.sexp"
                                    (config :state-root configuration)))
            "persistent command approvals live under the state root")
           (ensure-directories-exist other)
           (test-assert
            (not (permissions-allowed-p state "git status" root))
            "unknown commands are denied by persistent permission lookup")
            (let ((stale-a (permissions-load configuration))
                  (stale-b (permissions-load configuration)))
              (permissions-allow :configuration configuration
                                 :state         stale-a
                                 :command       "git diff"
                                 :directory     root)
              (permissions-allow :configuration configuration
                                 :state         stale-b
                                 :command       "git show"
                                 :directory     root)
              (test-assert (and (permissions-allowed-p stale-b "git diff" root)
                                (permissions-allowed-p stale-b "git show" root))
                           "stale approval callers merge fresh persisted permissions"))
           (permissions-allow :configuration configuration
                              :state         state
                              :command       "git status"
                              :directory     root)
           (test-assert (test-fixture-permissions-p
                         *platform*
                         (configuration-permissions-path configuration)
                         ':private-file)
                        "command permissions are private on disk")
           (let ((loaded (permissions-load configuration)))
             (test-assert (permissions-allowed-p loaded "git status" root)
                          "an exact command approval survives reload")
             (test-assert (not (permissions-allowed-p loaded "git status " root))
                          "command approvals match exact shell text")
             (test-assert (not (permissions-allowed-p loaded "git status" other))
                          "command approvals are scoped to their working directory")
             (permissions-clear configuration loaded)
             (test-assert
              (not (permissions-allowed-p
                    (permissions-load configuration) "git status" root))
              "clearing approvals persists an empty permission state")))
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-command-permission-corruption () null)
(defun test-command-permission-corruption ()
  "Test corrupt permissions fail closed and explicit writes recover them."
  (with-test-configuration (configuration root)
    (let ((pathname (configuration-permissions-path configuration)))
      (dolist (source '("#.(error \"must not evaluate\")"
                        "(:permissions :version"
                        "(:permissions :version 99 :rules nil)"
                        "(:permissions :version 1 :rules nil :rules nil)"
                        "(:permissions :version 1 :rules ((:command \"\" :directory \"/\")))"
                        "(:permissions :version 1 :rules ((:command \"anything\" :directory \"/\" :command \"other\")))"))
        (snapshot-write-text pathname source)
        (let ((warned-p nil)
              (state nil))
          (handler-bind
              ((permissions-load-warning
                 (lambda (condition)
                   (declare (ignore condition))
                   (setf warned-p t)
                   (muffle-warning))))
            (setf state (permissions-load configuration)))
          (test-assert warned-p "corrupt command permissions emit a warning")
          (test-assert (not (permissions-allowed-p state "anything" root))
                       "corrupt command permissions fail closed")
          (test-assert (string= source (uiop:read-file-string pathname))
                       "loading corrupt permissions preserves their bytes")
          (permissions-allow :configuration configuration :state state
                             :command "git status" :directory root)
          (test-assert (permissions-allowed-p (permissions-load configuration)
                                              "git status" root)
                       "an explicit approval replaces corrupt permission state")))
      (snapshot-write pathname
                      (list ':permissions ':version *permissions-version*
                            ':rules (list (list ':directory
                                                (permissions--directory-name root)
                                                ':command "git diff"))))
      (test-assert (permissions-allowed-p (permissions-load configuration)
                                          "git diff" root)
                   "permission records decode properties independently of key order")))
  nil)

(-> test-command-permission-write-failure () null)
(defun test-command-permission-write-failure ()
  "Test that a failed approval publication does not mutate its caller state."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (state (permissions-load configuration)))
    (unwind-protect
         (progn
           (ensure-directories-exist
            (merge-pathnames "permissions.sexp/"
                             (config :state-root configuration)))
           (test-assert
            (handler-case
                (progn
                  (permissions-allow :configuration configuration
                                     :state state
                                     :command "git status"
                                     :directory root)
                  nil)
              (permissions-error () t))
            "a permission publication failure is reported")
           (test-assert (null (permission-state-rules state))
                        "a failed approval does not mutate caller state"))
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore)))
  nil)

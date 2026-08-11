(in-package #:autolith)

;;;; -- Workspace Tool Classes --

(defclass workspace-tool (tool)
  ()
  (:documentation
   "A tool touching only workspace files and subprocesses, never the active image."))

(defclass fs-view-image-tool (workspace-tool)
  ()
  (:documentation "Attach one local image to the model for visual inspection."))

(defclass fs-list-tool (workspace-tool)
  ()
  (:documentation "List one workspace directory with entry kinds and sizes."))

(defclass fs-write-tool (workspace-tool)
  ()
  (:documentation "Create or replace one workspace file with supplied content."))

(defclass shell-run-tool (workspace-tool)
  ()
  (:documentation "Run one authorized external command in the workspace."))

(defmethod tool-child-safe-p ((tool fs-view-image-tool))
  "Permit native workspace image inspection inside child agents."
  t)

(defmethod tool-child-safe-p ((tool fs-list-tool))
  "Permit bounded workspace directory listings inside child agents."
  t)

(defmethod tool-child-safe-p ((tool fs-write-tool))
  "Permit workspace writes through the ordinary child capability boundary."
  t)

(defmethod tool-child-safe-p ((tool shell-run-tool))
  "Permit authorized workspace commands inside child agents."
  t)


;;;; -- Workspace Defaults --

(defparameter *shell-default-timeout-seconds* 60
  "The seconds one shell.run command may take by default.")

(defparameter *shell-maximum-output-characters* 65536
  "The maximum combined output characters returned by shell.run.")

(defparameter *workspace-tool-readable-roots* nil
  "Optional pathname roots confining workspace-tool reads for the current call.")

(defvar *workspace-file-mutation-lock*
  (make-recursive-lock "Autolith workspace file mutations")
  "Serialize native workspace file writes and revision-gated publication.")


;;;; -- Path Resolution --

(-> workspace-tool--read-path-allowed-p (pathname list) boolean)
(defun workspace-tool--read-path-allowed-p (path roots)
  "Return true when PATH resolves beneath one of the readable ROOTS."
  (let ((candidate (or (ignore-errors (truename path)) path)))
    (not
     (null
      (some (lambda (root)
              (uiop:subpathp candidate
                             (or (ignore-errors (truename root)) root)))
            roots)))))

(-> workspace-tool-path (tool-context (option string)) pathname)
(defun workspace-tool-path (context path)
  "Return PATH resolved against CONTEXT's working directory.

When *WORKSPACE-TOOL-READABLE-ROOTS* is non-NIL, reject paths outside those
roots after resolving existing symlinks."
  (let* ((working-directory (configuration-working-directory
                             (tool-context-configuration context)))
         (resolved (if (non-empty-string-p path)
                       (merge-pathnames (pathname path) working-directory)
                       working-directory)))
    (when (and *workspace-tool-readable-roots*
               (not (workspace-tool--read-path-allowed-p
                     resolved *workspace-tool-readable-roots*)))
      (error 'tool-error
             :message
             (format nil "Path ~A is outside the readable workspace and source roots."
                     resolved)
             :tool-name "fs"))
    resolved))

(-> workspace-tool-protected-path-p (tool-context pathname) boolean)
(defun workspace-tool-protected-path-p (context path)
  "Return true when PATH is off limits to workspace writes.

The stable launcher and recovery artifacts are always read-only. The rest
of Autolith's tracked source is writable only when the workspace itself is
inside the source root, meaning the user deliberately runs Autolith as a
development agent on its own repository. From any other workspace Autolith
never reaches into its own source, and live self-modification persists
through private image commits instead."
  (let* ((configuration (tool-context-configuration context))
         (source-root (configuration-source-root configuration)))
    (cond
      ((not (uiop:subpathp path source-root))
       nil)
      ((or (uiop:subpathp path (merge-pathnames "bin/" source-root))
           (uiop:subpathp path (merge-pathnames "recovery/" source-root))
           (string= (enough-namestring path source-root)
                    "script/build-recovery"))
       t)
      ((uiop:subpathp (configuration-working-directory configuration)
                      source-root)
       nil)
      (t
       t))))

(-> workspace-tool-protection-notice (tool-context pathname) string)
(defun workspace-tool-protection-notice (context path)
  "Explain why PATH is refused by the workspace write tools."
  (let ((source-root (configuration-source-root
                      (tool-context-configuration context))))
    (if (or (uiop:subpathp path (merge-pathnames "bin/" source-root))
            (uiop:subpathp path (merge-pathnames "recovery/" source-root))
            (string= (enough-namestring path source-root)
                     "script/build-recovery"))
        (format nil "~A is a stable launcher or recovery artifact and stays ~
                     read-only."
                path)
        (format nil "~A is Autolith's own source repository. Run Autolith with that ~
                     repository as the workspace to develop it, and use ~
                     self.persist-definition for live self changes."
                path))))

(-> workspace-tool-integer-argument
    (json-object string &key (:fallback (option integer)))
    (option integer))
(defun workspace-tool-integer-argument (arguments name &key fallback)
  "Return integer argument NAME from ARGUMENTS, or FALLBACK when absent."
  (let ((value (tool-argument arguments name)))
    (cond
      ((null value)
       fallback)
      ((integerp value)
       value)
      ((and (numberp value) (= value (round value)))
       (round value))
      (t
       (error 'tool-error
              :message (format nil "Tool argument ~S must be an integer." name)
              :tool-name name)))))

(-> workspace-tool-shell-timeout (json-object) (integer 1))
(defun workspace-tool-shell-timeout (arguments)
  "Return the positive requested shell timeout with no product maximum."
  (max 1
       (or (workspace-tool-integer-argument arguments "timeout-seconds")
           *shell-default-timeout-seconds*)))


;;;; -- Tool Executions --

(defmethod tool-execute ((tool fs-view-image-tool)
                         (context tool-context)
                         (arguments hash-table))
  "Return a local image as native provider image content."
  (let* ((path (workspace-tool-path
                context
                (tool-argument arguments "path" :required t)))
         (conversation (tool-context-conversation context))
         (attachment
           (image-input-prepare
            path
            (conversation-image-artifact-root conversation))))
    (tool-success
     (format nil "Viewed ~A (~Dx~D, ~A)."
             (image-attachment-source-name attachment)
             (image-attachment-width attachment)
             (image-attachment-height attachment)
             (image-attachment-mime-type attachment))
     :image-attachments (list attachment))))

(defmethod tool-execute ((tool fs-list-tool)
                         (context tool-context)
                         (arguments hash-table))
  "Return the requested directory's entries with kinds and byte sizes."
  (let ((path (workspace-tool-path context (tool-argument arguments "path"))))
    (if (not (uiop:directory-exists-p path))
        (tool-failure (format nil "~A is not a directory." path))
        (let ((directories (sort (mapcar (lambda (directory)
                                           (first (last (pathname-directory
                                                         directory))))
                                         (uiop:subdirectories path))
                                 #'string<))
              (files (sort (uiop:directory-files path)
                           #'string<
                           :key #'namestring)))
          (tool-success
           (format nil "~A~%~{~A~%~}~{~A~%~}"
                   path
                   (loop for name in directories
                         collect (format nil "d           ~A/" name))
                   (loop for file in files
                         collect (format nil "f ~9D  ~A"
                                         (handler-case
                                             (with-open-file (stream file
                                                              :element-type
                                                              '(unsigned-byte 8))
                                               (file-length stream))
                                           (error ()
                                             0))
                                         (file-namestring file)))))))))

(defmethod tool-execute ((tool fs-write-tool)
                         (context tool-context)
                         (arguments hash-table))
  "Create or replace the requested file with the supplied content."
  (let ((path (workspace-tool-path
               context
               (tool-argument arguments "path" :required t)))
        (content (tool-argument arguments "content" :required t)))
    (unless (stringp content)
      (error 'tool-error
             :message "fs.write requires string content."
             :tool-name "fs.write"))
    (with-recursive-lock-held (*workspace-file-mutation-lock*)
      (cond
        ((workspace-tool-protected-path-p context path)
         (tool-failure (workspace-tool-protection-notice context path)))
        ((uiop:directory-exists-p path)
         (tool-failure (format nil "~A is a directory." path)))
        (t
         (let* ((existed-p (and (probe-file path) t))
                (result-content
                  (lisp-source-edit-result-content
                   (format nil
                           "~:[Created~;Replaced~] ~A with ~:D character~:P."
                           existed-p
                           path
                           (length content))
                   path
                   content)))
           (ensure-directories-exist path)
           (with-open-file (stream path
                                   :direction :output
                                   :if-exists :supersede
                                   :if-does-not-exist :create
                                   :external-format :utf-8)
             (write-string content stream))
           (tool-success result-content)))))))

(-> workspace-tool-run-shell-command
    (string pathname t (integer 1) (integer 0))
    tool-result)
(defun workspace-tool-run-shell-command
    (command directory policy timeout output-limit)
  "Run one already authorized shell COMMAND with fully resolved execution policy."
  (let* ((result
           (handler-bind
               ((sb-int:stream-decoding-error
                  (lambda (condition)
                    (let ((restart (find-restart 'use-value condition)))
                      (when restart
                        (invoke-restart restart (code-char #xFFFD)))))))
             (run-sandboxed
              "/bin/sh"
              (list "-c" command)
              :policy policy
              :working-directory directory
              :timeout timeout
              :merge-output-p t
              :output-limit output-limit
              :error-output-limit output-limit)))
         (output (sandbox-result-output result))
         (presented-output
           (if (sandbox-result-output-truncated-p result)
               (format nil
                       "~A~%[combined output truncated after ~D characters]"
                       output output-limit)
               output)))
    (if (sandbox-result-timed-out-p result)
        (tool-failure
         (format nil "The command was stopped after ~D seconds.~%~A"
                 timeout presented-output))
        (tool-success
         (format nil "exit ~D~%~A"
                 (sandbox-result-exit-code result)
                 presented-output)))))

(defmethod tool-execute ((tool shell-run-tool)
                         (context tool-context)
                         (arguments hash-table))
  "Authorize one command, then run it once directly or as an inspectable job."
  (let* ((command (tool-argument arguments "command" :required t))
         (directory (workspace-tool-path
                     context
                     (tool-argument arguments "directory")))
         (timeout (workspace-tool-shell-timeout arguments))
         (async-p
           (tool-boolean-argument
            arguments "async" :tool-name "shell.run")))
    (unless (non-empty-string-p command)
      (error 'tool-error
             :message "shell.run requires a non-empty command."
             :tool-name "shell.run"))
    (let ((authorization
            (tool-context-authorize-command context command directory)))
      (if (eq authorization ':deny)
          (tool-failure "The user denied this command.")
          (let* ((configuration (tool-context-configuration context))
                 (policy
                   (ecase authorization
                     (:sandboxed
                      (workspace-write-sandbox-policy
                       :workspace-roots
                       (list
                        (configuration-working-directory configuration))))
                     (:full-access
                      (external-sandbox-policy))))
                 (output-limit *shell-maximum-output-characters*))
            (tool-execution-invoke
             (tool-context-execution-runtime context)
             (tool-context-agent context)
             :tool-name "shell.run"
             :summary (format nil "~A in ~A" command directory)
             :operation-function
             (lambda ()
               (workspace-tool-run-shell-command
                command directory policy timeout output-limit))
             :async-p async-p
             :parent-call-id (tool-context-call-id context)))))))

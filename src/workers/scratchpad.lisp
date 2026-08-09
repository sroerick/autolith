(in-package #:autolith)

;;;; -- Lisp Scratchpad Tools --

(defclass lisp-scratchpad-tool (lisp-tool)
  ()
  (:documentation
   "A tool operating on the current conversation's disposable Lisp files."))

(defclass lisp-scratchpad-list-tool (lisp-scratchpad-tool)
  ()
  (:documentation "List the current conversation's scratchpad folder."))

(defclass lisp-scratchpad-read-tool (lisp-scratchpad-tool)
  ()
  (:documentation "Read one file from the current conversation's scratchpad."))

(defclass lisp-scratchpad-write-tool (lisp-scratchpad-tool)
  ()
  (:documentation "Create or replace one file in the conversation scratchpad."))

(defclass lisp-scratchpad-edit-tool (lisp-scratchpad-tool)
  ()
  (:documentation "Replace exact text inside one conversation scratchpad file."))

(defclass lisp-scratchpad-run-tool (lisp-scratchpad-tool)
  ()
  (:documentation "Load one conversation scratchpad file into a named Lisp REPL."))

(defclass lisp-scratchpad-delete-tool (lisp-scratchpad-tool)
  ()
  (:documentation "Delete one scratchpad path or clear the conversation folder."))

(defmethod tool-compact-result-visible-p ((tool lisp-scratchpad-write-tool))
  "Keep successful scratchpad writes visible in compact presentation."
  t)

(defmethod tool-compact-result-visible-p ((tool lisp-scratchpad-edit-tool))
  "Keep successful scratchpad edits visible in compact presentation."
  t)

(defmethod tool-compact-result-visible-p ((tool lisp-scratchpad-run-tool))
  "Keep arbitrary scratchpad execution visible in compact presentation."
  t)

(defmethod tool-compact-result-visible-p ((tool lisp-scratchpad-delete-tool))
  "Keep destructive scratchpad operations visible in compact presentation."
  t)


;;;; -- Session Paths --

(-> lisp-scratchpad--identifier-fragment (string) string)
(defun lisp-scratchpad--identifier-fragment (identifier)
  "Return a stable filesystem-safe fragment for conversation IDENTIFIER."
  (or (conversation-identifier-path-fragment identifier)
      (let ((fragment
              (map 'string
                   (lambda (character)
                     (if (or (alphanumericp character)
                             (member character '(#\- #\_)))
                         character
                         #\_))
                   identifier)))
        (if (non-empty-string-p fragment)
            fragment
            "conversation"))))

(-> lisp-scratchpad-root (tool-context) pathname)
(defun lisp-scratchpad-root (context)
  "Return CONTEXT's conversation-scoped disposable scratchpad folder."
  (let* ((configuration (tool-context-configuration context))
         (conversation (tool-context-conversation context))
         (fragment
           (lisp-scratchpad--identifier-fragment
            (conversation-identifier conversation))))
    (merge-pathnames
     (format nil "scratchpads/~A/" fragment)
     (configuration-cache-root configuration))))

(-> lisp-scratchpad-path (tool-context string) pathname)
(defun lisp-scratchpad-path (context relative-path)
  "Resolve RELATIVE-PATH inside CONTEXT's scratchpad or signal TOOL-ERROR."
  (unless (non-empty-string-p relative-path)
    (error 'tool-error
           :message "A scratchpad path must be a non-empty relative pathname."
           :tool-name "lisp.scratchpad"))
  (let* ((root (lisp-scratchpad-root context))
         (relative (pathname relative-path))
         (directory (pathname-directory relative)))
    (when (or (uiop:absolute-pathname-p relative)
              (wild-pathname-p relative)
              (some (lambda (component)
                      (member component '(:up :back :wild :wild-inferiors)))
                    directory))
      (error 'tool-error
             :message "Scratchpad paths must stay inside the session folder."
             :tool-name "lisp.scratchpad"))
    (let ((resolved (merge-pathnames relative root)))
      (unless (uiop:subpathp resolved root)
        (error 'tool-error
               :message "Scratchpad paths must stay inside the session folder."
               :tool-name "lisp.scratchpad"))
      resolved)))

(-> lisp-scratchpad--file-size (pathname) (integer 0))
(defun lisp-scratchpad--file-size (pathname)
  "Return PATHNAME's byte length, or zero when it cannot be measured."
  (handler-case
      (with-open-file (stream pathname :element-type '(unsigned-byte 8))
        (file-length stream))
    (error ()
      0)))

(-> lisp-scratchpad--list-entries (pathname pathname) list)
(defun lisp-scratchpad--list-entries (root directory)
  "Return recursive, sorted entry descriptions beneath DIRECTORY relative to ROOT."
  (let ((directories (sort (uiop:subdirectories directory)
                           #'string<
                           :key #'namestring))
        (files (sort (uiop:directory-files directory)
                     #'string<
                     :key #'namestring)))
    (append
     (loop for child in directories
           append (cons (format nil "d           ~A"
                                (enough-namestring child root))
                        (lisp-scratchpad--list-entries root child)))
     (loop for file in files
           collect (format nil "f ~9D  ~A"
                           (lisp-scratchpad--file-size file)
                           (enough-namestring file root))))))


;;;; -- Tool Executions --

(defmethod tool-execute ((tool lisp-scratchpad-list-tool)
                         (context tool-context)
                         (arguments hash-table))
  "List one directory within the current conversation's scratchpad."
  (declare (ignore tool))
  (let* ((root (lisp-scratchpad-root context))
         (relative-path (tool-argument arguments "path"))
         (directory (if relative-path
                        (uiop:ensure-directory-pathname
                         (lisp-scratchpad-path context relative-path))
                        root)))
    (uiop:ensure-all-directories-exist (list root))
    (if (not (uiop:directory-exists-p directory))
        (tool-failure (format nil "~A is not a scratchpad directory." directory))
        (tool-success
         (format nil "~A~%~{~A~%~}"
                 directory
                 (lisp-scratchpad--list-entries root directory))))))

(defmethod tool-execute ((tool lisp-scratchpad-read-tool)
                         (context tool-context)
                         (arguments hash-table))
  "Read a numbered window from one scratchpad file."
  (declare (ignore tool))
  (let* ((path
           (lisp-scratchpad-path
            context
            (tool-argument arguments "path" :required t)))
         (start-line (max 1 (or (workspace-tool-integer-argument
                                 arguments "start-line")
                                1)))
         (line-count (max 1 (or (workspace-tool-integer-argument
                                 arguments "line-count")
                                *fs-read-default-line-count*))))
    (cond
      ((uiop:directory-exists-p path)
       (tool-failure (format nil "~A is a directory." path)))
      ((not (probe-file path))
       (tool-failure (format nil "~A does not exist." path)))
      (t
       (multiple-value-bind (body total body-truncated-p)
           (workspace--read-file-window path start-line line-count)
         (let* ((window-start (min (1- start-line) total))
                (window-end (min (+ window-start line-count) total)))
           (tool-success
            (workspace--fs-read-result-content
             path
             body
             :first-line (1+ window-start)
             :last-line window-end
             :total-lines total
             :body-truncated-p body-truncated-p))))))))

(defmethod tool-execute ((tool lisp-scratchpad-write-tool)
                         (context tool-context)
                         (arguments hash-table))
  "Create or replace one scratchpad file."
  (declare (ignore tool))
  (let ((path
          (lisp-scratchpad-path
           context
           (tool-argument arguments "path" :required t)))
        (content (tool-argument arguments "content" :required t)))
    (unless (stringp content)
      (error 'tool-error
             :message "lisp.scratchpad-write requires string content."
             :tool-name "lisp.scratchpad-write"))
    (if (uiop:directory-exists-p path)
        (tool-failure (format nil "~A is a directory." path))
        (let* ((existed-p (and (probe-file path) t))
               (result-content
                 (lisp-source-edit-result-content
                  (format nil
                          "~:[Created~;Replaced~] scratchpad file ~A with ~:D character~:P."
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
          (tool-success result-content)))))

(defmethod tool-execute ((tool lisp-scratchpad-edit-tool)
                         (context tool-context)
                         (arguments hash-table))
  "Replace exact text occurrences inside one scratchpad file."
  (declare (ignore tool))
  (let ((path
          (lisp-scratchpad-path
           context
           (tool-argument arguments "path" :required t)))
        (old-text (tool-argument arguments "old-text" :required t))
        (new-text (tool-argument arguments "new-text" :required t))
        (replace-all (tool-argument arguments "replace-all")))
    (unless (and (stringp old-text) (stringp new-text))
      (error 'tool-error
             :message "lisp.scratchpad-edit requires string old-text and new-text."
             :tool-name "lisp.scratchpad-edit"))
    (cond
      ((zerop (length old-text))
       (tool-failure "lisp.scratchpad-edit requires non-empty old-text."))
      ((or (uiop:directory-exists-p path) (not (probe-file path)))
       (tool-failure (format nil "~A is not an existing scratchpad file." path)))
      (t
       (let* ((text (uiop:read-file-string path))
              (occurrences (workspace--count-occurrences old-text text)))
         (cond
           ((zerop occurrences)
            (tool-failure
             (format nil "The old-text was not found in ~A." path)))
           ((and (> occurrences 1) (not replace-all))
            (tool-failure
             (format nil "The old-text matches ~D times in ~A; include more context or set replace-all."
                     occurrences
                     path)))
           (t
            (let* ((replacement
                     (workspace--replace-occurrences
                      old-text
                      new-text
                      text
                      :all (and replace-all t)))
                   (result-content
                     (lisp-source-edit-result-content
                      (format nil
                              "Replaced ~D occurrence~:P in scratchpad file ~A."
                              (if replace-all occurrences 1)
                              path)
                      path
                      replacement)))
              (with-open-file (stream path
                                      :direction :output
                                      :if-exists :supersede
                                      :external-format :utf-8)
                (write-string replacement stream))
              (tool-success result-content)))))))))

(defmethod tool-execute ((tool lisp-scratchpad-run-tool)
                         (context tool-context)
                         (arguments hash-table))
  "Load one scratchpad file once into the selected named Lisp REPL."
  (declare (ignore tool))
  (let* ((path
           (lisp-scratchpad-path
            context
            (tool-argument arguments "path" :required t)))
         (repl (lisp-tool-repl-name arguments))
         (form
           (format nil
                   "(load ~A :verbose nil :print nil :external-format :utf-8)"
                   (prin1-to-string path))))
    (cond
      ((uiop:directory-exists-p path)
       (tool-failure (format nil "~A is a directory." path)))
      ((not (probe-file path))
       (tool-failure (format nil "~A does not exist." path)))
      (t
       (lisp-tool-invoke-execution
        context arguments
        :tool-name "lisp.scratchpad-run"
        :summary (format nil "Load scratchpad ~A" path)
        :operation-function
        (lambda (worker)
          (let ((result
                  (worker-response-tool-result
                   (lisp-worker-request worker :eval (list :form form)))))
            (if (tool-result-success-p result)
                (tool-success
                 (format nil "Loaded scratchpad file ~A into Lisp REPL ~A.~%~A"
                         path
                         repl
                         (tool-result-content result)))
                result))))))))

(defmethod tool-execute ((tool lisp-scratchpad-delete-tool)
                         (context tool-context)
                         (arguments hash-table))
  "Delete one scratchpad path, or clear the whole conversation folder."
  (declare (ignore tool))
  (let* ((root (lisp-scratchpad-root context))
         (relative-path (tool-argument arguments "path"))
         (path (if relative-path
                   (lisp-scratchpad-path context relative-path)
                   root)))
    (cond
      ((uiop:directory-exists-p path)
       (uiop:delete-directory-tree path
                                   :validate t
                                   :if-does-not-exist :ignore)
       (tool-success
        (if relative-path
            (format nil "Deleted scratchpad directory ~A." path)
            (format nil "Cleared scratchpad folder ~A." root))))
      ((probe-file path)
       (delete-file path)
       (tool-success (format nil "Deleted scratchpad file ~A." path)))
      (t
       (tool-failure (format nil "~A does not exist." path))))))

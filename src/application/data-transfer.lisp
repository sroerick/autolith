(in-package #:autolith)

;;;; -- Data Transfer Commands --

(-> data-transfer--command-path (t configuration) pathname)
(defun data-transfer--command-path (value configuration)
  "Resolve a user-supplied file or directory against the session workspace."
  (unless (or (pathnamep value) (non-empty-string-p value))
    (error 'configuration-error :message "Data transfer requires a pathname."))
  (merge-pathnames (if (pathnamep value)
                      value
                      (uiop:parse-native-namestring value))
                   (configuration-working-directory configuration)))

(-> data-transfer--command-workspace (list configuration &key (:export-p boolean))
    (option pathname))
(defun data-transfer--command-workspace (options configuration &key export-p)
  "Parse the scope or remapping options shared by callable and slash commands."
  (cond
    ((null options)
     nil)
    ((and export-p (= (length options) 1)
          (member (first options) '(:all "--all") :test #'equal))
     nil)
    ((and (= (length options) 2)
          (member (first options) '(:workspace "--workspace") :test #'equal))
     (data-transfer--command-path (second options) configuration))
    (t
     (error 'configuration-error
            :message (if export-p
                         "Use :all or :workspace DIRECTORY after the archive pathname."
                         "Use :workspace DIRECTORY after the archive pathname to relocate a workspace.")))))

(-> data-transfer-render-report (string list) string)
(defun data-transfer-render-report (action report)
  "Render a compact transfer receipt for terminal and command-line callers."
  (format nil "~A ~A~%~D conversations, ~D memories, ~D agendas, ~D plans, ~D papercuts, ~D files.~%~A"
          action (uiop:native-namestring (pathname (getf report :pathname)))
          (getf report :conversations) (getf report :memories)
          (getf report :agendas) (getf report :plans)
          (getf report :papercuts) (getf report :files)
          (if (getf report :workspace)
              (format nil "Workspace: ~A"
                      (uiop:native-namestring (pathname (getf report :workspace))))
              (format nil "All data across ~D workspaces."
                      (length (getf report :workspaces))))))

(define-application-command application--builtin-data-export-command
    (:name "/data.export"
     :argument "FILE [:all | :workspace DIRECTORY]"
     :description "export all portable user data or one workspace"
     :tip "writes a private archive of sessions, memories, agendas, and their assets."
     :busy-behavior :hold
     :terminal-behavior :shared
     :callable t)
    (application pathname &rest options)
  (let* ((configuration (application-configuration application))
         (workspace (data-transfer--command-workspace options configuration :export-p t))
         (report (data-export (data-transfer--command-path pathname configuration)
                              :workspace workspace :configuration configuration)))
    (application-present application (data-transfer-render-report "Exported" report)))
  ':continue)

(define-application-command application--builtin-data-import-command
    (:name "/data.import"
     :argument "FILE [:workspace DIRECTORY]"
     :description "import portable user data, optionally relocating a workspace"
     :tip "merges an archive without overwriting conflicting local data."
     :busy-behavior :hold
     :terminal-behavior :shared
     :callable t)
    (application pathname &rest options)
  (let* ((configuration (application-configuration application))
         (workspace (data-transfer--command-workspace options configuration))
         (report (data-import (data-transfer--command-path pathname configuration)
                              :workspace workspace :configuration configuration)))
    (application-present application (data-transfer-render-report "Imported" report)))
  ':continue)

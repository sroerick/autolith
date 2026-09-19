(in-package #:autolith)

;;;; -- Workspace Language Servers --

(defparameter *lsp-maximum-document-bytes* (* 2 1024 1024)
  "The largest file synchronized with a language server.")

(defparameter *lsp-maximum-open-documents* 64
  "The maximum synchronized documents per language server.")

(defparameter *lsp-maximum-clients* 16
  "The maximum live project/server pairs in one tool registry.")

(defparameter *lsp-maximum-diagnostics* 200
  "The maximum retained diagnostics for one open document.")

(defclass lsp-diagnostic-report ()
  ((items :initform #() :accessor lsp-diagnostic-report-items
          :documentation "Bounded items from one diagnostic source.")
   (received-p :initform nil :accessor lsp-diagnostic-report-received-p
               :documentation "Whether this source has reported for the current version.")
   (versioned-p :initform nil :accessor lsp-diagnostic-report-versioned-p
                :documentation "Whether the report explicitly identifies the document version.")
   (received-at :initform 0 :accessor lsp-diagnostic-report-received-at
                :documentation "Monotonic arrival time for diagnostic settling.")
   (truncated-p :initform nil :accessor lsp-diagnostic-report-truncated-p
                :documentation "Whether this source exceeded the retained item limit."))
  (:documentation "The latest push or pull report, guarded by the client lock."))

(defclass lsp-document ()
  ((path :initarg :path :reader lsp-document-path
         :documentation "Canonical local pathname.")
   (text :initarg :text :accessor lsp-document-text
         :documentation "Last synchronized complete text.")
   (version :initform 1 :accessor lsp-document-version
            :documentation "Monotonically increasing document version.")
   (push-report :initform (make-instance 'lsp-diagnostic-report)
                :accessor lsp-document-push-report
                :documentation "Latest published diagnostics for this version.")
   (pull-report :initform (make-instance 'lsp-diagnostic-report)
                :accessor lsp-document-pull-report
                :documentation "Latest pulled diagnostics for this version."))
  (:documentation "One synchronized file with independent push and pull diagnostic state."))

(defclass lsp-client ()
  ((configuration :initarg :configuration :reader lsp-client-configuration
                  :documentation "Trusted server launch configuration.")
   (root :initarg :root :reader lsp-client-root
         :documentation "Canonical project root inside the workspace.")
   (transport :initform nil :accessor lsp-client-transport
              :documentation "The external JSON-RPC transport, when started.")
   (capabilities :initform (json-object) :accessor lsp-client-capabilities
                 :documentation "Static capabilities returned by initialize.")
   (documents :initform (make-hash-table :test #'equal) :reader lsp-client-documents
              :documentation "File URIs mapped to synchronized documents.")
   (lock :initform (make-lock "LSP document state") :reader lsp-client-lock
         :documentation "Protects state shared with the notification thread."))
  (:documentation "One persistent language server for a project root."))

(defclass lsp-manager ()
  ((clients :initform (make-hash-table :test #'equal) :reader lsp-manager-clients
            :documentation "Project/server identities mapped to clients.")
   (configurations :initform nil :accessor lsp-manager-configurations
                   :documentation "Validated user configuration, loaded lazily.")
   (loaded-p :initform nil :accessor lsp-manager-loaded-p
             :documentation "Whether configuration has been loaded.")
   (lock :initform (make-recursive-lock "LSP operations") :reader lsp-manager-lock
         :documentation "Serializes synchronization, queries, and lifecycle changes."))
  (:documentation "Registry-owned lazy language server pool; contains no persistent process state."))

(-> lsp-path-uri (pathname) string)
(defun lsp-path-uri (path)
  "Encode an absolute local pathname as a file URI, including drive and UNC paths."
  (platform-file-uri *platform* path))

(-> lsp--seconds () real)
(defun lsp--seconds ()
  "Return monotonic time in seconds."
  (/ (get-internal-real-time) internal-time-units-per-second))

(-> lsp--position (string) json-object)
(defun lsp--position (text)
  "Return the zero-based UTF-16 position at the end of TEXT."
  (let ((line 0) (character 0))
    (loop for index below (length text)
          for value = (char text index)
          do (cond
               ((char= value #\Newline)
                (incf line) (setf character 0))
               ((char= value #\Return)
                (unless (and (< (1+ index) (length text))
                             (char= (char text (1+ index)) #\Newline))
                  (incf line) (setf character 0)))
               (t
                (incf character (if (> (char-code value) #xffff) 2 1)))))
    (json-object "line" line "character" character)))

(-> lsp--settings-section (json-object t) t)
(defun lsp--settings-section (settings section)
  "Return SECTION from SETTINGS, interpreting dotted configuration section names."
  (if (and (stringp section) (plusp (length section)))
      (or (json-get settings section)
          (reduce (lambda (value key)
                    (and (hash-table-p value) (json-get value key)))
                  (uiop:split-string section :separator ".")
                  :initial-value settings))
      settings))

(-> lsp-client--server-request (lsp-client string t) t)
(defun lsp-client--server-request (client method params)
  "Answer the small advertised client protocol without permitting server-driven edits."
  (cond
    ((string= method "workspace/configuration")
     (map 'vector
          (lambda (item)
            (lsp--settings-section
             (lsp-server-configuration-settings (lsp-client-configuration client))
             (and (hash-table-p item) (json-get item "section"))))
          (if (hash-table-p params) (or (json-get params "items") #()) #())))
    ((string= method "workspace/workspaceFolders")
     (vector (json-object "uri" (lsp-path-uri (lsp-client-root client))
                          "name" (file-namestring
                                  (directory-namestring (lsp-client-root client))))))
    ((string= method "workspace/applyEdit")
     (json-object "applied" yason:false
                  "failureReason" "Use revision-gated resource.edit for workspace changes."))
    ((member method '("window/workDoneProgress/create" "window/showMessageRequest")
             :test #'string=)
     nil)
    (t
     (error 'lsp-rpc-error :code -32601
            :message (format nil "Unsupported language server request: ~A" method)))))

(-> lsp-client--publish-diagnostics
    (lsp-client json-object &key (:source (member :push :pull))) null)
(defun lsp-client--publish-diagnostics (client params &key (source ':push))
  "Replace only SOURCE's report for an open file, rejecting explicitly stale versions."
  (let ((uri (json-get params "uri"))
        (version (json-get params "version"))
        (items (json-get params "diagnostics")))
    (when (and (stringp uri) (vectorp items) (not (stringp items)))
      (with-lock-held ((lsp-client-lock client))
        (let ((document (gethash uri (lsp-client-documents client))))
          (when (and document
                     (or (null version)
                         (eql version (lsp-document-version document))))
            (let ((report (ecase source
                            (:push (lsp-document-push-report document))
                            (:pull (lsp-document-pull-report document)))))
              (setf (lsp-diagnostic-report-items report)
                    (subseq items 0 (min (length items) *lsp-maximum-diagnostics*))
                    (lsp-diagnostic-report-truncated-p report)
                    (> (length items) *lsp-maximum-diagnostics*)
                    (lsp-diagnostic-report-received-p report) t
                    (lsp-diagnostic-report-versioned-p report) (integerp version)
                    (lsp-diagnostic-report-received-at report) (lsp--seconds))))))))
  nil)

(-> lsp-client--notification (lsp-client string t) null)
(defun lsp-client--notification (client method params)
  "Handle bounded diagnostics; progress and window messages need no terminal output."
  (when (and (string= method "textDocument/publishDiagnostics") (hash-table-p params))
    (lsp-client--publish-diagnostics client params))
  nil)

(-> lsp-client-start (lsp-server-configuration pathname) lsp-client)
(defun lsp-client-start (configuration root)
  "Start and initialize one configured server, closing partial startup on failure."
  (let ((client (make-instance 'lsp-client :configuration configuration :root root))
        (completed-p nil))
    (unwind-protect
         (progn
           (setf (lsp-client-transport client)
                 (lsp-transport-open
                  :command (lsp-server-configuration-command configuration)
                  :arguments (lsp-server-configuration-arguments configuration)
                  :directory root
                  :request-handler (lambda (method params)
                                     (lsp-client--server-request client method params))
                  :notification-handler (lambda (method params)
                                          (lsp-client--notification client method params))))
           (let* ((reply
                    (lsp-transport-request
                     (lsp-client-transport client) "initialize"
                     (json-object
                      "processId" nil
                      "clientInfo" (json-object "name" "Autolith" "version" *autolith-version*)
                      "rootUri" (lsp-path-uri root)
                      "workspaceFolders" (vector (json-object "uri" (lsp-path-uri root)
                                                               "name" (namestring root)))
                      "capabilities"
                      (json-object
                       "general" (json-object "positionEncodings" (vector "utf-16"))
                       "workspace" (json-object "configuration" t "workspaceFolders" t
                                                "applyEdit" yason:false)
                       "textDocument"
                       (json-object
                        "synchronization" (json-object "didSave" t)
                        "publishDiagnostics" (json-object "versionSupport" t)
                        "diagnostic" (json-object "dynamicRegistration" yason:false)
                        "hover" (json-object "contentFormat" (vector "markdown" "plaintext"))))
                      "initializationOptions"
                      (lsp-server-configuration-initialization-options configuration))
                     :timeout (lsp-server-configuration-timeout-seconds configuration)))
                  (capabilities (and (hash-table-p reply) (json-get reply "capabilities"))))
             (unless (hash-table-p capabilities)
               (error 'lsp-error :message "Language server returned no initialize capabilities."))
             (unless (string= (json-get capabilities "positionEncoding" "utf-16") "utf-16")
               (error 'lsp-error :message "Language server did not negotiate UTF-16 positions."))
             (setf (lsp-client-capabilities client) capabilities))
           (lsp-transport-notify (lsp-client-transport client) "initialized" (json-object))
           (lsp-transport-notify
            (lsp-client-transport client) "workspace/didChangeConfiguration"
            (json-object "settings" (lsp-server-configuration-settings configuration)))
           (setf completed-p t)
           client)
      (unless completed-p
        (when (lsp-client-transport client)
          (lsp-transport-close (lsp-client-transport client)))))))

(-> lsp-client-close (lsp-client &key (:detach-p boolean)) null)
(defun lsp-client-close (client &key detach-p)
  "Shut down CLIENT or detach only its inherited descriptors in a saver child."
  (let ((transport (lsp-client-transport client)))
    (when transport
      (if detach-p
          (lsp-transport-detach transport)
          (unwind-protect
               (when (lsp-transport-live-p transport)
                 (ignore-errors (lsp-transport-request transport "shutdown" nil :timeout 1))
                 (ignore-errors (lsp-transport-notify transport "exit" nil)))
            (lsp-transport-close transport)))
      (setf (lsp-client-transport client) nil)))
  (clrhash (lsp-client-documents client))
  nil)

(-> lsp-manager-close (lsp-manager &key (:detach-p boolean)) null)
(defun lsp-manager-close (manager &key detach-p)
  "Release all clients; detachment deliberately avoids inherited locks."
  (flet ((close-clients ()
           (maphash (lambda (key client)
                      (declare (ignore key))
                      (lsp-client-close client :detach-p detach-p))
                    (lsp-manager-clients manager))
           (clrhash (lsp-manager-clients manager))
           (setf (lsp-manager-loaded-p manager) nil
                 (lsp-manager-configurations manager) nil)))
    (if detach-p
        (close-clients)
        (with-recursive-lock-held ((lsp-manager-lock manager)) (close-clients))))
  nil)

(-> lsp-manager-configure (lsp-manager configuration) list)
(defun lsp-manager-configure (manager configuration)
  "Load trusted server definitions once until explicit refresh or runtime retirement."
  (unless (lsp-manager-loaded-p manager)
    (setf (lsp-manager-configurations manager) (lsp-load-configurations configuration)
          (lsp-manager-loaded-p manager) t))
  (lsp-manager-configurations manager))

(-> lsp-manager-client (lsp-manager lsp-server-configuration pathname) lsp-client)
(defun lsp-manager-client (manager configuration root)
  "Return a live client for CONFIGURATION and ROOT, restarting a dead process once."
  (let* ((key (list (lsp-server-configuration-name configuration) (namestring root)))
         (existing (gethash key (lsp-manager-clients manager))))
    (when existing
      (if (and (lsp-client-transport existing)
               (lsp-transport-live-p (lsp-client-transport existing)))
          (return-from lsp-manager-client existing)
          (progn (lsp-client-close existing) (remhash key (lsp-manager-clients manager)))))
    (when (>= (hash-table-count (lsp-manager-clients manager)) *lsp-maximum-clients*)
      (error 'lsp-error :message "Language server limit reached; use lsp.restart to release clients."))
    (setf (gethash key (lsp-manager-clients manager)) (lsp-client-start configuration root))))

(-> lsp--read-document (pathname) string)
(defun lsp--read-document (path)
  "Read a bounded UTF-8 source file for synchronization."
  (with-open-file (stream path :element-type '(unsigned-byte 8))
    (when (> (file-length stream) *lsp-maximum-document-bytes*)
      (error 'lsp-error :message "File exceeds the language server document size limit.")))
  (let ((text (uiop:read-file-string path :external-format ':utf-8)))
    (when (or (> (length text) *lsp-maximum-document-bytes*) (find #\Null text))
      (error 'lsp-error :message "Language server input must be a bounded UTF-8 text file."))
    text))

(-> lsp-client--sync-options (lsp-client) (values integer boolean t))
(defun lsp-client--sync-options (client)
  "Return change kind, open/close support, and save options advertised by CLIENT."
  (let ((sync (json-get (lsp-client-capabilities client) "textDocumentSync")))
    (cond
      ((integerp sync)
       (values sync (plusp sync) nil))
      ((hash-table-p sync)
       (values (json-get sync "change" 0) (not (null (json-get sync "openClose")))
               (json-get sync "save")))
      (t
       (values 0 nil nil)))))

(-> lsp-client-sync (lsp-client pathname) lsp-document)
(defun lsp-client-sync (client path)
  "Synchronize PATH, invalidating diagnostics before sending changed content."
  (let* ((uri (lsp-path-uri path))
         (text (lsp--read-document path))
         (document nil) (old-text nil) (changed-p nil) (opened-p nil)
         (transport (lsp-client-transport client)))
    (with-lock-held ((lsp-client-lock client))
      (setf document (gethash uri (lsp-client-documents client)))
      (unless document
        (when (>= (hash-table-count (lsp-client-documents client)) *lsp-maximum-open-documents*)
          (error 'lsp-error :message "Open document limit reached; use lsp.restart to release documents."))
        (setf document (make-instance 'lsp-document :path path :text text)
              (gethash uri (lsp-client-documents client)) document
              opened-p t))
      (unless (string= text (lsp-document-text document))
        (setf old-text (lsp-document-text document)
              changed-p t
              (lsp-document-text document) text
              (lsp-document-push-report document) (make-instance 'lsp-diagnostic-report)
              (lsp-document-pull-report document) (make-instance 'lsp-diagnostic-report))
        (incf (lsp-document-version document))))
    (multiple-value-bind (kind open-close-p save) (lsp-client--sync-options client)
      (when (and opened-p open-close-p)
        (lsp-transport-notify
         transport "textDocument/didOpen"
         (json-object "textDocument"
                      (json-object "uri" uri "languageId"
                                   (lsp-server-configuration-language-id (lsp-client-configuration client))
                                   "version" (lsp-document-version document) "text" text))))
      (when (and changed-p (plusp kind))
        (let ((change (json-object "text" text)))
          (when (= kind 2)
            (setf (gethash "range" change)
                  (json-object "start" (json-object "line" 0 "character" 0)
                               "end" (lsp--position old-text))))
          (lsp-transport-notify
           transport "textDocument/didChange"
           (json-object "textDocument" (json-object "uri" uri "version" (lsp-document-version document))
                        "contentChanges" (vector change)))))
      (when (and (or opened-p changed-p) save)
        (let ((params (json-object "textDocument" (json-object "uri" uri))))
          (when (and (hash-table-p save) (json-get save "includeText"))
            (setf (gethash "text" params) text))
          (lsp-transport-notify transport "textDocument/didSave" params))))
    document))

(-> lsp-client-resync (lsp-client) null)
(defun lsp-client-resync (client)
  "Refresh open files before each query, including changes made through the shell."
  (let ((documents nil))
    (with-lock-held ((lsp-client-lock client))
      (maphash (lambda (uri document) (push (cons uri document) documents))
               (lsp-client-documents client)))
    (dolist (entry documents)
      (let ((path (lsp-document-path (rest entry))))
        (if (probe-file path)
            (lsp-client-sync client path)
            (progn
              (multiple-value-bind (kind open-close-p) (lsp-client--sync-options client)
                (declare (ignore kind))
                (when open-close-p
                  (lsp-transport-notify (lsp-client-transport client) "textDocument/didClose"
                                        (json-object "textDocument" (json-object "uri" (first entry))))))
              (with-lock-held ((lsp-client-lock client))
                (remhash (first entry) (lsp-client-documents client))))))))
  nil)

(-> lsp-diagnostic-report--state (lsp-diagnostic-report) string)
(defun lsp-diagnostic-report--state (report)
  "Describe freshness for one diagnostic source."
  (cond
    ((not (lsp-diagnostic-report-received-p report))
     "pending")
    ((lsp-diagnostic-report-versioned-p report)
     "received")
    (t
     "unversioned")))

(-> lsp-document-diagnostics (lsp-document) (values vector boolean))
(defun lsp-document-diagnostics (document)
  "Return the bounded union and truncation flag; the caller holds the client lock."
  (let* ((reports (list (lsp-document-push-report document) (lsp-document-pull-report document)))
         (items (remove-duplicates
                 (concatenate 'vector (lsp-diagnostic-report-items (first reports))
                                      (lsp-diagnostic-report-items (second reports)))
                 :test #'equalp :from-end t)))
    (values (subseq items 0 (min (length items) *lsp-maximum-diagnostics*))
            (not (null (or (> (length items) *lsp-maximum-diagnostics*)
                           (some #'lsp-diagnostic-report-truncated-p reports)))))))

(-> lsp-document--diagnostic-snapshot (lsp-document boolean) json-object)
(defun lsp-document--diagnostic-snapshot (document pull-p)
  "Combine independent reports without treating an empty source as clearing another."
  (let* ((push-report (lsp-document-push-report document))
         (pull-report (lsp-document-pull-report document))
         (reports (remove-if-not #'lsp-diagnostic-report-received-p
                                 (list push-report pull-report)))
         (versioned-p (and reports (every #'lsp-diagnostic-report-versioned-p reports))))
    (multiple-value-bind (items truncated-p) (lsp-document-diagnostics document)
      (json-object "uri" (lsp-path-uri (lsp-document-path document))
                   "version" (lsp-document-version document)
                   "state" (cond
                             ((null reports)
                              "pending")
                             (versioned-p
                              "received")
                             (t
                              "unversioned"))
                   "sources" (json-object "push" (lsp-diagnostic-report--state push-report)
                                          "pull" (if pull-p
                                                     (lsp-diagnostic-report--state pull-report)
                                                     "unsupported"))
                   "versioned" (if versioned-p t yason:false)
                   "truncated" (if truncated-p t yason:false)
                   "items" items))))

(-> lsp-client-diagnostics (lsp-client lsp-document &key (:wait-seconds real)) json-object)
(defun lsp-client-diagnostics (client document &key (wait-seconds 2))
  "Pull one report and wait for quiet push reports, retaining both diagnostic sources."
  (let ((pull-p (not (null (json-get (lsp-client-capabilities client) "diagnosticProvider")))))
    (when pull-p
      (let ((reply (lsp-transport-request
                    (lsp-client-transport client) "textDocument/diagnostic"
                    (json-object "textDocument" (json-object "uri" (lsp-path-uri (lsp-document-path document))))
                    :timeout (lsp-server-configuration-timeout-seconds (lsp-client-configuration client)))))
        ;; No previousResultId is sent: a full report is required for this snapshot.
        (unless (and (hash-table-p reply) (json-string= (json-get reply "kind") "full")
                     (vectorp (json-get reply "items")) (not (stringp (json-get reply "items"))))
          (error 'lsp-error :message "Language server returned no full diagnostic report."))
        (lsp-client--publish-diagnostics
         client (json-object "uri" (lsp-path-uri (lsp-document-path document))
                             "version" (lsp-document-version document)
                             "diagnostics" (json-get reply "items"))
         :source ':pull)))
    (let ((deadline (+ (lsp--seconds) wait-seconds)))
      (loop
        (with-lock-held ((lsp-client-lock client))
          (let ((reports (remove-if-not #'lsp-diagnostic-report-received-p
                                       (list (lsp-document-push-report document)
                                             (lsp-document-pull-report document)))))
            (when (or (>= (lsp--seconds) deadline)
                      (and reports
                           (every (lambda (report)
                                    (>= (- (lsp--seconds) (lsp-diagnostic-report-received-at report)) 0.15))
                                  reports)))
              (return (lsp-document--diagnostic-snapshot document pull-p)))))
        (unless (lsp-transport-live-p (lsp-client-transport client))
          (error 'lsp-error :message "Language server exited while waiting for diagnostics."))
        (sleep 0.02)))))

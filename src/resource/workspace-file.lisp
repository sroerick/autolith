(in-package #:autolith)

;;;; -- Workspace File Resources --

(defparameter *workspace-file-resource-maximum-observations* 16
  "The transient resource observations retained by one conversation.")

(defparameter *workspace-file-resource-maximum-bytes* (* 4 1024 1024)
  "The largest exact workspace-file snapshot retained for revision-gated editing.")

(defparameter *workspace-file-resource-default-line-count* 400
  "The lines requested by resource.read when no explicit count is supplied.")

(defparameter *workspace-file-resource-maximum-line-count* 1000
  "The largest line window accepted by one resource.read call.")

(defparameter *workspace-file-resource-maximum-result-characters* 7600
  "The maximum characters constructed for one resource tool result.")

(defparameter *workspace-file-resource-publish-function*
  #'uiop:rename-file-overwriting-target
  "The function atomically publishing a prepared workspace-file replacement.")

(defvar *workspace-file-resource-digest-key* (random-data 16)
  "The process-local key used to identify exact workspace-file snapshots.")

(defclass workspace-file-resource (resource)
  ((pathname
    :initarg :pathname
    :reader workspace-file-resource-pathname
    :type pathname
    :documentation "The authority-checked pathname represented by this resource."))
  (:documentation "An existing or potential file addressed through the workspace boundary."))

(defclass workspace-file-resolver (resource-resolver)
  ()
  (:documentation "Resolve workspace: URIs through the ordinary workspace path boundary."))

(defclass workspace-file-observation (resource-observation)
  ((lines
    :initarg :lines
    :reader workspace-file-observation-lines
    :type vector
    :documentation "The complete logical lines in the exact observed snapshot.")
   (line-ending
    :initarg :line-ending
    :reader workspace-file-observation-line-ending
    :type string
    :documentation "The LF or CRLF sequence used when reconstructing edited content.")
   (final-newline-p
    :initarg :final-newline-p
    :reader workspace-file-observation-final-newline-p
    :type boolean
    :documentation "Whether the exact observed snapshot ended in a line ending."))
  (:documentation "A complete transient UTF-8 workspace-file snapshot."))

(defclass workspace-file-observation-state (resource-observation-state)
  ((visible-ranges
    :initarg :visible-ranges
    :accessor workspace-file-observation-state-visible-ranges
    :type list
    :documentation "Inclusive original line ranges fully shown to the model."))
  (:documentation "One conversation-local model observation of a workspace file."))


;;;; -- URI Resolution --

(defmethod resource-resolver-child-safe-p
    ((resolver workspace-file-resolver) context)
  "Permit child agents to resolve files through the existing workspace boundary."
  (declare (ignore resolver context))
  t)

(-> workspace-file--uri-safe-octet-p ((unsigned-byte 8)) boolean)
(defun workspace-file--uri-safe-octet-p (octet)
  "Return true when OCTET may appear literally in a workspace URI identifier."
  (and (or (and (>= octet (char-code #\a)) (<= octet (char-code #\z)))
           (and (>= octet (char-code #\A)) (<= octet (char-code #\Z)))
           (and (>= octet (char-code #\0)) (<= octet (char-code #\9)))
           (find octet
                 (map 'list #'char-code "-._~/")
                 :test #'=))
       t))

(-> workspace-file--encode-identifier (string) string)
(defun workspace-file--encode-identifier (identifier)
  "Percent-encode IDENTIFIER while retaining readable path separators."
  (with-output-to-string (stream)
    (loop for octet across (sb-ext:string-to-octets identifier
                                                    :external-format :utf-8)
          do
             (if (workspace-file--uri-safe-octet-p octet)
                 (write-char (code-char octet) stream)
                 (format stream "%~2,'0X" octet)))))

(-> workspace-file--canonical-uri (tool-context pathname) string)
(defun workspace-file--canonical-uri (context path)
  "Return PATH's stable canonical workspace URI under CONTEXT."
  (let* ((working-directory
           (or (ignore-errors
                 (truename
                  (configuration-working-directory
                   (tool-context-configuration context))))
               (configuration-working-directory
                (tool-context-configuration context))))
         (canonical-path (or (ignore-errors (truename path)) path))
         (identifier
           (if (uiop:subpathp canonical-path working-directory)
               (enough-namestring canonical-path working-directory)
               (namestring canonical-path))))
    (format nil "workspace:~A"
            (workspace-file--encode-identifier identifier))))

(defmethod resource-resolver-resolve
    ((resolver workspace-file-resolver) identifier context)
  "Resolve IDENTIFIER through WORKSPACE-TOOL-PATH without granting authority."
  (declare (ignore resolver))
  (let ((path (workspace-tool-path context (url-decode identifier))))
    (make-instance 'workspace-file-resource
                   :uri      (workspace-file--canonical-uri context path)
                   :pathname path)))

(defmethod resource-capabilities
    ((resource workspace-file-resource) (context tool-context))
  "Return workspace file operations allowed by CONTEXT for RESOURCE."
  (if (workspace-tool-protected-path-p
       context (workspace-file-resource-pathname resource))
      '(:read)
      '(:read :edit)))


;;;; -- Snapshot Observation --

(-> workspace-file--digest (string) string)
(defun workspace-file--digest (content)
  "Return a full process-local digest for exact UTF-8 CONTENT."
  (let ((mac (make-mac ':siphash
                       *workspace-file-resource-digest-key*
                       :digest-length 16)))
    (update-mac mac
                (sb-ext:string-to-octets content :external-format :utf-8))
    (with-output-to-string (stream)
      (loop for octet across (produce-mac mac)
            do (format stream "~2,'0X" octet)))))

(-> workspace-file--validate-file-stat (pathname t) null)
(defun workspace-file--validate-file-stat (path stat)
  "Reject a non-regular or oversized PATH described by STAT."
  (unless (sb-posix:s-isreg (sb-posix:stat-mode stat))
    (error 'tool-error
           :message (format nil "Workspace resource ~A is not a regular file."
                            (namestring path))
           :tool-name "resource.read"))
  (when (> (sb-posix:stat-size stat) *workspace-file-resource-maximum-bytes*)
    (error 'tool-error
           :message
           (format nil "Workspace resource ~A is ~:D bytes; resource.read retains exact snapshots only up to ~:D bytes. Use fs.read for bounded inspection."
                   (namestring path)
                   (sb-posix:stat-size stat)
                   *workspace-file-resource-maximum-bytes*)
           :tool-name "resource.read"))
  nil)

(-> workspace-file--same-file-stat-p (t t) boolean)
(defun workspace-file--same-file-stat-p (left right)
  "Return true when LEFT and RIGHT identify the same opened filesystem object."
  (and (= (sb-posix:stat-dev left) (sb-posix:stat-dev right))
       (= (sb-posix:stat-ino left) (sb-posix:stat-ino right))))

(-> workspace-file--stable-file-stat-p (t t) boolean)
(defun workspace-file--stable-file-stat-p (before after)
  "Return true when an opened file stayed unchanged between two observations."
  (and (workspace-file--same-file-stat-p before after)
       (= (sb-posix:stat-size before) (sb-posix:stat-size after))
       (= (sb-posix:stat-mtime before) (sb-posix:stat-mtime after))
       (= (sb-posix:stat-ctime before) (sb-posix:stat-ctime after))))

(-> workspace-file--read-content
    (pathname &optional (option tool-context))
    string)
(defun workspace-file--read-content (path &optional context)
  "Read a bounded regular PATH descriptor as exact UTF-8 text."
  (let ((file-descriptor nil)
        (stream nil))
    (unwind-protect
         (handler-case
             (progn
               (setf file-descriptor
                     (sb-posix:open (namestring path)
                                    (logior sb-posix:o-rdonly
                                            sb-posix:o-nonblock
                                            sb-posix:o-nofollow)))
               (let ((stat (sb-posix:fstat file-descriptor)))
                 (workspace-file--validate-file-stat path stat)
                 (when context
                   (workspace-tool-path context (namestring path))
                   (let ((current-stat (sb-posix:stat (namestring path))))
                     (unless (workspace-file--same-file-stat-p stat current-stat)
                       (error 'tool-error
                              :message
                              (format nil "Workspace resource ~A changed while its authority boundary was being checked. Reread it and retry."
                                      (namestring path))
                              :tool-name "resource.read"))))
                 (let* ((length (sb-posix:stat-size stat))
                        (octets (make-array length
                                            :element-type '(unsigned-byte 8))))
                   (setf stream
                         (sb-sys:make-fd-stream
                          file-descriptor
                          :input t
                          :element-type '(unsigned-byte 8)
                          :buffering :none
                          :auto-close nil))
                   (unless (= (read-sequence octets stream) length)
                     (error 'tool-error
                            :message
                            (format nil "Workspace resource ~A changed while it was being observed. Reread it and retry."
                                    (namestring path))
                            :tool-name "resource.read"))
                    (unless (workspace-file--stable-file-stat-p
                             stat
                             (sb-posix:fstat file-descriptor))
                      (error 'tool-error
                             :message
                             (format nil "Workspace resource ~A changed while it was being observed. Reread it and retry."
                                     (namestring path))
                             :tool-name "resource.read"))
                   (handler-case
                       (sb-ext:octets-to-string octets :external-format :utf-8)
                     (error ()
                       (error 'tool-error
                              :message
                              (format nil "Workspace resource ~A is not valid UTF-8 text. Use fs.read or another binary-aware tool."
                                      (namestring path))
                              :tool-name "resource.read"))))))
           (sb-posix:syscall-error (condition)
             (error 'tool-error
                    :message
                    (format nil "Could not open workspace resource ~A as an exact regular file: ~A"
                            (namestring path) condition)
                    :tool-name "resource.read")))
      (when stream
        (close stream))
      (when file-descriptor
        (ignore-errors (sb-posix:close file-descriptor))))))

(-> workspace-file--line-ending (string) string)
(defun workspace-file--line-ending (content)
  "Return the line-ending style to preserve for CONTENT."
  (if (search (format nil "~C~C" #\Return #\Newline) content)
      (format nil "~C~C" #\Return #\Newline)
      (string #\Newline)))

(-> workspace-file--mixed-line-endings-p (string) boolean)
(defun workspace-file--mixed-line-endings-p (content)
  "Return true when CONTENT contains both LF and CRLF line endings."
  (let ((crlf-p nil)
        (lf-p nil))
    (loop for position = (position #\Newline content)
            then (position #\Newline content :start (1+ position))
          while position
          do (if (and (plusp position)
                      (char= (char content (1- position)) #\Return))
                 (setf crlf-p t)
                 (setf lf-p t)))
    (and crlf-p lf-p)))

(-> workspace-file--split-lines (string) vector)
(defun workspace-file--split-lines (content)
  "Return CONTENT's logical lines without their LF or CRLF delimiters."
  (if (zerop (length content))
      #()
      (let ((lines nil)
            (start 0)
            (length (length content)))
        (loop
          for newline = (position #\Newline content :start start)
          for end = (or newline length)
          for logical-end = (if (and (> end start)
                                     (char= (char content (1- end)) #\Return))
                                (1- end)
                                end)
          do (push (subseq content start logical-end) lines)
          if newline
            do (setf start (1+ newline))
          else
            do (return)
          when (= start length)
            do (return))
        (coerce (nreverse lines) 'vector))))

(-> workspace-file--final-newline-p (string) boolean)
(defun workspace-file--final-newline-p (content)
  "Return true when CONTENT ends in LF, including CRLF."
  (and (plusp (length content))
       (char= (char content (1- (length content))) #\Newline)))

(-> workspace-file--observe-path
    (workspace-file-resource tool-context)
    workspace-file-observation)
(defun workspace-file--observe-path (resource context)
  "Return a complete exact observation of RESOURCE's current file content."
  (let ((path (workspace-file-resource-pathname resource)))
    (unless (uiop:file-exists-p path)
      (error 'tool-error
             :message (format nil "Resource ~A does not name an existing file."
                              (resource-uri resource))
             :tool-name "resource.read"))
    (when (uiop:directory-exists-p path)
      (error 'tool-error
             :message (format nil "Resource ~A names a directory, not a file."
                              (resource-uri resource))
             :tool-name "resource.read"))
    (let ((content (workspace-file--read-content path context)))
      (make-instance 'workspace-file-observation
                     :uri             (resource-uri resource)
                     :revision        (workspace-file--digest content)
                     :content         content
                     :metadata        (list ':pathname path)
                     :lines           (workspace-file--split-lines content)
                     :line-ending     (workspace-file--line-ending content)
                     :final-newline-p (workspace-file--final-newline-p content)))))

(defmethod resource-observe
    ((resource workspace-file-resource) (context tool-context))
  "Observe RESOURCE as a complete exact UTF-8 snapshot under CONTEXT."
  (workspace-file--observe-path resource context))


;;;; -- Conversation Observation State --

(-> workspace-file--merge-visible-ranges (list list) list)
(defun workspace-file--merge-visible-ranges (left right)
  "Return sorted inclusive LEFT and RIGHT ranges with overlaps merged."
  (let ((ranges (sort (append (copy-tree left) (copy-tree right))
                      #'< :key #'first))
        (result nil))
    (dolist (range ranges)
      (let ((previous (first result)))
        (if (and previous (<= (first range) (1+ (second previous))))
            (setf (second previous) (max (second previous) (second range)))
            (push (copy-list range) result))))
    (nreverse result)))

(-> workspace-file--observation-state-count (hash-table) (integer 0))
(defun workspace-file--observation-state-count (states)
  "Return the number of workspace file observation STATES."
  (loop for state being the hash-values of states
        count (typep state 'workspace-file-observation-state)))

(-> workspace-file--expire-oldest-observation-state
    (conversation hash-table)
    boolean)
(defun workspace-file--expire-oldest-observation-state (conversation states)
  "Expire the oldest workspace file state without disturbing other resource states."
  (loop for alias in (conversation-resource-observation-order conversation)
        for state = (resource-observation-state-find
                     states alias 'workspace-file-observation-state)
        when state
          do
             (setf (conversation-resource-observation-order conversation)
                   (remove alias
                           (conversation-resource-observation-order conversation)
                           :test #'string=
                           :count 1))
             (remhash alias states)
             (return t)
        finally (return nil)))

(-> workspace-file--observation-state-for-snapshot
    (conversation workspace-file-observation list)
    workspace-file-observation-state)
(defun workspace-file--observation-state-for-snapshot
    (conversation observation visible-ranges)
  "Return or create CONVERSATION's state for OBSERVATION and VISIBLE-RANGES."
  (with-recursive-lock-held ((conversation-resource-observation-lock conversation))
    (let* ((states (conversation-resource-observations conversation))
           (matching
             (loop for state being the hash-values of states
                   when (typep state 'workspace-file-observation-state)
                     do
                        (let ((existing
                                (resource-observation-state-observation state)))
                          (when (and
                                 (string= (resource-observation-uri existing)
                                          (resource-observation-uri observation))
                                 (string= (resource-observation-revision existing)
                                          (resource-observation-revision observation))
                                 (string= (resource-observation-content existing)
                                          (resource-observation-content observation)))
                            (return state))))))
      (when matching
        (setf (workspace-file-observation-state-visible-ranges matching)
              (workspace-file--merge-visible-ranges
               (workspace-file-observation-state-visible-ranges matching)
               visible-ranges))
        (return-from workspace-file--observation-state-for-snapshot matching))
      (let* ((alias (resource-observation-state-new-alias states))
             (state (make-instance 'workspace-file-observation-state
                                   :alias          alias
                                   :observation    observation
                                   :visible-ranges visible-ranges)))
        (setf (gethash alias states) state
              (conversation-resource-observation-order conversation)
              (append (conversation-resource-observation-order conversation)
                      (list alias)))
        (loop while (> (workspace-file--observation-state-count states)
                       *workspace-file-resource-maximum-observations*)
              while (workspace-file--expire-oldest-observation-state
                     conversation states))
        state))))

(-> workspace-file--find-observation-state
    (conversation non-empty-string non-empty-string)
    workspace-file-observation-state)
(defun workspace-file--find-observation-state (conversation uri alias)
  "Return CONVERSATION's exact URI observation ALIAS or signal stale revision."
  (let ((state
          (resource-observation-state-find
           (conversation-resource-observations conversation)
           alias
           'workspace-file-observation-state)))
    (unless (and state
                 (string= uri
                          (resource-observation-uri
                           (resource-observation-state-observation state))))
      (error 'resource-revision-stale
             :uri               uri
             :expected-revision alias
             :actual-revision   nil))
    state))

(-> workspace-file--line-visible-p
    (workspace-file-observation-state (integer 1))
    boolean)
(defun workspace-file--line-visible-p (state line)
  "Return true when LINE was fully visible under STATE's exact observation."
  (and (some (lambda (range)
               (<= (first range) line (second range)))
             (workspace-file-observation-state-visible-ranges state))
       t))

(-> workspace-file--range-visible-p
    (workspace-file-observation-state (integer 1) (integer 1))
    boolean)
(defun workspace-file--range-visible-p (state start-line end-line)
  "Return true when every line from START-LINE through END-LINE was visible."
  (and (some (lambda (range)
               (and (<= (first range) start-line)
                    (<= end-line (second range))))
             (workspace-file-observation-state-visible-ranges state))
       t))


;;;; -- Bounded Observation Rendering --

(-> workspace-file--window-ranges
    (workspace-file-observation (integer 1) (integer 1))
    (values string list (integer 0) boolean))
(defun workspace-file--window-ranges (observation start-line line-count)
  "Render a bounded numbered window and return body, visible ranges, last line, truncation."
  (let* ((lines (workspace-file-observation-lines observation))
         (total-lines (length lines))
         (last-requested (min total-lines (+ start-line line-count -1)))
         (body (make-array *workspace-file-resource-maximum-result-characters*
                           :element-type 'character
                           :fill-pointer 0))
         (visible-start nil)
         (visible-end nil)
         (truncated-p nil))
    (loop for line from start-line to last-requested
          for text = (aref lines (1- line))
          for rendered = (format nil "~6D  ~A" line text)
          for separator-length = (if visible-start 1 0)
          do
             (if (> (+ (fill-pointer body)
                       separator-length
                       (length rendered))
                    (array-total-size body))
                 (progn
                   (setf truncated-p t)
                   (return))
                 (progn
                   (when visible-start
                     (vector-push #\Newline body))
                   (loop for character across rendered
                         do (vector-push character body))
                   (unless visible-start
                     (setf visible-start line))
                   (setf visible-end line))))
    (values (coerce body 'string)
            (if visible-start (list (list visible-start visible-end)) nil)
            (or visible-end 0)
            truncated-p)))

(-> workspace-file--format-ranges (list) string)
(defun workspace-file--format-ranges (ranges)
  "Return RANGES in concise model-visible inclusive form."
  (if ranges
      (format nil "~{~A~^, ~}"
              (mapcar (lambda (range)
                        (if (= (first range) (second range))
                            (format nil "~D" (first range))
                            (format nil "~D-~D" (first range) (second range))))
                      ranges))
      "none"))

(-> workspace-file--elisions ((integer 0) list boolean) list)
(defun workspace-file--elisions (total-lines ranges truncated-p)
  "Return explicit gaps omitted from the cumulative visible RANGES."
  (let ((cursor 1)
        (elisions nil))
    (dolist (range ranges)
      (when (< cursor (first range))
        (push (format nil "~D-~D~:[ between visible ranges~; before~]"
                      cursor (1- (first range)) (= cursor 1))
              elisions))
      (setf cursor (1+ (second range))))
    (when (<= cursor total-lines)
      (push (format nil "~D-~D after" cursor total-lines) elisions))
    (when truncated-p
      (push "current result truncated" elisions))
    (nreverse elisions)))

(-> workspace-file--read-result
    (workspace-file-observation-state string (integer 0) boolean)
    string)
(defun workspace-file--read-result (state body total-lines truncated-p)
  "Return one explicit model-facing resource observation result."
  (let* ((observation (resource-observation-state-observation state))
         (ranges (workspace-file-observation-state-visible-ranges state))
         (elisions (workspace-file--elisions total-lines ranges truncated-p)))
    (format nil "URI: ~A~%Revision: ~A~%Visible lines: ~A of ~D~%Elided: ~A~%Content:~%~A"
            (resource-observation-uri observation)
            (resource-observation-state-alias state)
            (workspace-file--format-ranges ranges)
            total-lines
            (if elisions (format nil "~{~A~^; ~}" elisions) "none")
            body)))


;;;; -- Structured Operations --

(-> workspace-file--operation-name (json-object) string)
(defun workspace-file--operation-name (operation)
  "Return and validate OPERATION's operation name."
  (let ((name (tool-argument operation "op" :required t)))
    (unless (and (stringp name)
                 (member name
                         '("replace-lines" "insert-before" "insert-after"
                           "delete-lines" "replace-empty")
                         :test #'string=))
      (error 'tool-error
             :message (format nil "Unknown resource edit operation ~S." name)
             :tool-name "resource.edit"))
    name))

(-> workspace-file--required-positive-line (json-object string) (integer 1))
(defun workspace-file--required-positive-line (operation name)
  "Return required positive integer line NAME from OPERATION."
  (let ((value (tool-argument operation name :required t)))
    (unless (and (integerp value) (plusp value))
      (error 'tool-error
             :message (format nil "Resource edit field ~S must be a positive integer."
                              name)
             :tool-name "resource.edit"))
    value))

(-> workspace-file--operation-content (json-object string boolean) string)
(defun workspace-file--operation-content (operation name non-empty-p)
  "Return string content NAME from OPERATION, optionally requiring non-empty text."
  (let ((value (tool-argument operation name :required t)))
    (unless (and (stringp value)
                 (or (not non-empty-p) (plusp (length value))))
      (error 'tool-error
             :message (format nil "Resource edit field ~S must be ~:[a string~;a non-empty string~]."
                              name non-empty-p)
             :tool-name "resource.edit"))
    value))

(-> workspace-file--operation-extra-keys-p (json-object list) boolean)
(defun workspace-file--operation-extra-keys-p (operation allowed)
  "Return true when OPERATION contains a key outside ALLOWED."
  (loop for key being the hash-keys of operation
        thereis (not (member key allowed :test #'string=))))

(-> workspace-file--normalize-operation
    (json-object workspace-file-observation-state)
    list)
(defun workspace-file--normalize-operation (operation state)
  "Validate one JSON OPERATION against STATE and return its normalized plist."
  (unless (json-object-p operation)
    (error 'tool-error
           :message "Every resource edit operation must be a JSON object."
           :tool-name "resource.edit"))
  (let* ((name (workspace-file--operation-name operation))
         (observation (resource-observation-state-observation state)))
    (cond
      ((string= name "replace-empty")
       (when (workspace-file--operation-extra-keys-p
              operation '("op" "content"))
         (error 'tool-error
                :message "Operation replace-empty contains unsupported fields."
                :tool-name "resource.edit"))
       (unless (zerop (length (workspace-file-observation-lines observation)))
         (error 'tool-error
                :message "Operation replace-empty is valid only for an observed empty file."
                :tool-name "resource.edit"))
       (let ((content (workspace-file--operation-content operation "content" t)))
         (list :kind ':replace-empty
               :start 0
               :end 0
               :lines (coerce (workspace-file--split-lines content) 'list)
               :line-ending (workspace-file--line-ending content)
               :final-newline-p (workspace-file--final-newline-p content)
               :summary "replace-empty")))
      ((member name '("replace-lines" "delete-lines") :test #'string=)
       (let* ((start-line (workspace-file--required-positive-line
                           operation "start-line"))
              (end-line (workspace-file--required-positive-line
                         operation "end-line"))
              (replace-p (string= name "replace-lines"))
              (allowed (if replace-p
                           '("op" "start-line" "end-line" "content")
                           '("op" "start-line" "end-line"))))
         (when (workspace-file--operation-extra-keys-p operation allowed)
           (error 'tool-error
                  :message (format nil "Operation ~A contains unsupported fields." name)
                  :tool-name "resource.edit"))
         (unless (<= start-line end-line)
           (error 'tool-error
                  :message (format nil "Operation ~A has start-line after end-line." name)
                  :tool-name "resource.edit"))
         (unless (workspace-file--range-visible-p state start-line end-line)
           (error 'tool-error
                  :message (format nil "Operation ~A addresses lines ~D-~D that were not fully visible under this revision. Reread the required window."
                                   name start-line end-line)
                  :tool-name "resource.edit"))
         (list :kind (if replace-p ':replace ':delete)
               :start start-line
               :end end-line
               :lines (if replace-p
                          (coerce
                           (workspace-file--split-lines
                            (workspace-file--operation-content
                             operation "content" nil))
                           'list)
                          nil)
               :summary (format nil "~A ~D-~D" name start-line end-line))))
      (t
       (let* ((line (workspace-file--required-positive-line operation "line"))
              (content (workspace-file--operation-content operation "content" t)))
         (when (workspace-file--operation-extra-keys-p
                operation '("op" "line" "content"))
           (error 'tool-error
                  :message (format nil "Operation ~A contains unsupported fields." name)
                  :tool-name "resource.edit"))
         (unless (workspace-file--line-visible-p state line)
           (error 'tool-error
                  :message (format nil "Operation ~A anchors line ~D that was not visible under this revision. Reread the required window."
                                   name line)
                  :tool-name "resource.edit"))
         (list :kind (if (string= name "insert-before") ':insert-before ':insert-after)
               :start line
               :end line
               :lines (coerce (workspace-file--split-lines content) 'list)
               :summary (format nil "~A ~D" name line)))))))

(-> workspace-file--validate-operation-overlaps (list) null)
(defun workspace-file--validate-operation-overlaps (operations)
  "Reject operations whose original line intervals overlap or share an anchor."
  (loop for tail on operations
        for operation = (first tail)
        do
           (dolist (other (rest tail))
             (when (and (<= (getf operation :start) (getf other :end))
                        (<= (getf other :start) (getf operation :end)))
               (error 'tool-error
                      :message
                      (format nil "Resource edit operations overlap or ambiguously share original lines ~D-~D and ~D-~D."
                              (getf operation :start) (getf operation :end)
                              (getf other :start) (getf other :end))
                      :tool-name "resource.edit"))))
  nil)

(-> workspace-file--normalize-operations
    (list workspace-file-observation-state)
    list)
(defun workspace-file--normalize-operations (value state)
  "Validate operation list VALUE completely against STATE."
  (unless (and (listp value) value)
    (error 'tool-error
           :message "Resource edit operations must be a non-empty list."
           :tool-name "resource.edit"))
  (let ((operations
          (loop for operation in value
                collect (workspace-file--normalize-operation operation state))))
    (when (and (find ':replace-empty operations
                     :key (lambda (operation) (getf operation :kind)))
               (> (length operations) 1))
      (error 'tool-error
             :message "Operation replace-empty must be the only resource edit operation."
             :tool-name "resource.edit"))
    (workspace-file--validate-operation-overlaps operations)
    (sort operations #'< :key (lambda (operation) (getf operation :start)))))

(-> workspace-file--apply-normalized-operations (vector list) list)
(defun workspace-file--apply-normalized-operations (lines operations)
  "Apply normalized OPERATIONS to original LINES in one deterministic pass."
  (when (and (zerop (length lines))
             (eq (getf (first operations) :kind) ':replace-empty))
    (return-from workspace-file--apply-normalized-operations
      (copy-list (getf (first operations) :lines))))
  (let ((result nil)
        (line 1)
        (remaining operations)
        (total (length lines)))
    (loop while (<= line total)
          for operation = (first remaining)
          do
             (cond
               ((and operation (= line (getf operation :start)))
                (case (getf operation :kind)
                  (:insert-before
                   (setf result (nconc (reverse (copy-list (getf operation :lines)))
                                       result))
                   (push (aref lines (1- line)) result)
                   (incf line))
                  (:insert-after
                   (push (aref lines (1- line)) result)
                   (setf result (nconc (reverse (copy-list (getf operation :lines)))
                                       result))
                   (incf line))
                  ((:replace :delete)
                   (setf result (nconc (reverse (copy-list (getf operation :lines)))
                                       result)
                         line (1+ (getf operation :end)))))
                (pop remaining))
               (t
                (push (aref lines (1- line)) result)
                (incf line))))
    (nreverse result)))

(-> workspace-file--join-lines (list string boolean) string)
(defun workspace-file--join-lines (lines line-ending final-newline-p)
  "Return LINES joined with LINE-ENDING while preserving FINAL-NEWLINE-P."
  (with-output-to-string (stream)
    (loop for line in lines
          for first-p = t then nil
          unless first-p do (write-string line-ending stream)
          do (write-string line stream))
    (when (and final-newline-p lines)
      (write-string line-ending stream))))

(-> workspace-file--temporary-path (pathname) pathname)
(defun workspace-file--temporary-path (path)
  "Return a fresh same-directory temporary pathname for PATH."
  (merge-pathnames
   (format nil ".~A.autolith-resource-~A.tmp"
           (file-namestring path)
           (subseq (localgroup-random-token) 0 16))
   (uiop:pathname-directory-pathname path)))

(-> workspace-file--replacement-octets (string) (simple-array (unsigned-byte 8) (*)))
(defun workspace-file--replacement-octets (content)
  "Return CONTENT as UTF-8 octets after enforcing the exact replacement limit."
  (let ((octets (sb-ext:string-to-octets content :external-format :utf-8)))
    (when (> (length octets) *workspace-file-resource-maximum-bytes*)
      (error 'tool-error
             :message
             (format nil "Prospective workspace resource replacement is ~:D UTF-8 bytes; resource.edit permits at most ~:D bytes."
                     (length octets)
                     *workspace-file-resource-maximum-bytes*)
             :tool-name "resource.edit"))
    octets))

(-> workspace-file--write-temporary
    (pathname pathname (simple-array (unsigned-byte 8) (*)))
    null)
(defun workspace-file--write-temporary (temporary target octets)
  "Write replacement OCTETS to TEMPORARY and preserve TARGET permissions."
  (with-open-file (stream temporary
                          :direction :output
                          :if-exists :error
                          :if-does-not-exist :create
                          :element-type '(unsigned-byte 8))
    (write-sequence octets stream)
    (finish-output stream))
  (ignore-errors
    (sb-posix:chmod (namestring temporary)
                    (logand #o7777
                            (sb-posix:stat-mode
                             (sb-posix:stat (namestring target))))))
  nil)

(-> workspace-file--publish
    (workspace-file-resource tool-context workspace-file-observation string)
    workspace-file-observation)
(defun workspace-file--publish (resource context base-observation content)
  "Atomically publish CONTENT after an immediate exact BASE-OBSERVATION check."
  (let ((path (workspace-file-resource-pathname resource)))
    (when (workspace-tool-protected-path-p context path)
      (error 'tool-error
             :message (workspace-tool-protection-notice context path)
             :tool-name "resource.edit"))
    (let ((octets (workspace-file--replacement-octets content))
          (temporary (workspace-file--temporary-path path)))
      (unwind-protect
           (progn
             (workspace-file--write-temporary temporary path octets)
             (unless (uiop:file-exists-p path)
               (error 'resource-revision-stale
                      :uri (resource-uri resource)
                      :expected-revision
                      (resource-observation-revision base-observation)
                      :actual-revision nil))
             (let ((current (workspace-file--observe-path resource context)))
               (unless (and
                        (string= (resource-observation-revision current)
                                 (resource-observation-revision base-observation))
                        (string= (resource-observation-content current)
                                 (resource-observation-content base-observation)))
                 (error 'resource-revision-stale
                        :uri (resource-uri resource)
                        :expected-revision
                        (resource-observation-revision base-observation)
                        :actual-revision
                        (resource-observation-revision current))))
             (funcall *workspace-file-resource-publish-function* temporary path)
             (let ((published (workspace-file--observe-path resource context)))
               (unless (string= content
                                (resource-observation-content published))
                 (error 'tool-error
                        :message
                        "Atomic workspace resource publication did not produce the exact requested content."
                        :tool-name "resource.edit"))
               (setf temporary nil)
               published))
        (when (and temporary (probe-file temporary))
          (delete-file temporary))))))

(-> workspace-file--operation-window (list (integer 0))
    (values (integer 1) (integer 1)))
(defun workspace-file--operation-window (operations total-lines)
  "Return a bounded nearby window covering OPERATIONS in the resulting file."
  (let* ((first-line (reduce #'min operations :key (lambda (op) (getf op :start))))
         (last-line (reduce #'max operations :key (lambda (op) (getf op :end))))
         (start (max 1 (- first-line 2)))
         (count (max 1 (min 120 (+ (- last-line start) 6)))))
    (values (if (zerop total-lines) 1 (min start total-lines)) count)))

(defmethod resource-apply-operations
    ((resource workspace-file-resource) (context tool-context)
     &key base-revision operations)
  "Apply structured original-line OPERATIONS to RESOURCE at BASE-REVISION."
  (let ((conversation (tool-context-conversation context)))
    (with-recursive-lock-held (*workspace-file-mutation-lock*)
      (with-recursive-lock-held
          ((conversation-resource-observation-lock conversation))
        (let* ((state (workspace-file--find-observation-state
                       conversation (resource-uri resource) base-revision))
               (base-observation
                 (resource-observation-state-observation state)))
          (when (workspace-file--mixed-line-endings-p
                 (resource-observation-content base-observation))
            (error 'tool-error
                   :message "resource.edit does not rewrite files with mixed LF and CRLF line endings because doing so could change untouched lines. Normalize the file deliberately or use fs.edit for an exact replacement."
                   :tool-name "resource.edit"))
          (unless (uiop:file-exists-p (workspace-file-resource-pathname resource))
            (error 'resource-revision-stale
                   :uri (resource-uri resource)
                   :expected-revision base-revision
                   :actual-revision nil))
          (let ((current (workspace-file--observe-path resource context)))
            (unless (and
                     (string= (resource-observation-revision current)
                              (resource-observation-revision base-observation))
                     (string= (resource-observation-content current)
                              (resource-observation-content base-observation)))
              (error 'resource-revision-stale
                     :uri (resource-uri resource)
                     :expected-revision base-revision
                     :actual-revision (resource-observation-revision current)))
            (let* ((normalized
                     (workspace-file--normalize-operations operations state))
                   (new-lines
                     (workspace-file--apply-normalized-operations
                      (workspace-file-observation-lines base-observation)
                      normalized))
                   (replace-empty
                     (find ':replace-empty normalized
                           :key (lambda (operation) (getf operation :kind))))
                   (content
                     (workspace-file--join-lines
                      new-lines
                      (if replace-empty
                          (getf replace-empty :line-ending)
                          (workspace-file-observation-line-ending base-observation))
                      (if replace-empty
                          (getf replace-empty :final-newline-p)
                          (workspace-file-observation-final-newline-p
                           base-observation)))))
              (values
               (workspace-file--publish resource context base-observation content)
               normalized))))))))


;;;; -- Resource Tool Methods --

(defmethod resource-tool-read
    ((resource workspace-file-resource) (tool resource-read-tool)
     (context tool-context) (arguments hash-table))
  "Read a bounded workspace resource window and establish its edit observation."
  (declare (ignore tool))
  (let ((start-line
          (max 1 (or (workspace-tool-integer-argument
                      arguments "start-line" :fallback 1)
                     1)))
        (line-count
          (min *workspace-file-resource-maximum-line-count*
               (max 1 (or (workspace-tool-integer-argument
                           arguments "line-count"
                           :fallback *workspace-file-resource-default-line-count*)
                          *workspace-file-resource-default-line-count*)))))
    (with-recursive-lock-held (*workspace-file-mutation-lock*)
      (let* ((observation (resource-observe resource context))
             (total-lines
               (length (workspace-file-observation-lines observation))))
        (when (and (plusp total-lines) (> start-line total-lines))
          (error 'tool-error
                 :message (format nil "Start line ~D is beyond the resource's ~D lines. Request a valid window."
                                  start-line total-lines)
                 :tool-name "resource.read"))
        (multiple-value-bind (body visible-ranges last-line truncated-p)
            (workspace-file--window-ranges observation start-line line-count)
          (declare (ignore last-line))
          (when (and (plusp total-lines) (null visible-ranges))
            (error 'tool-error
                   :message (format nil "Line ~D exceeds the resource.read result limit and was not observed. Use fs.read or another bounded inspection method."
                                    start-line)
                   :tool-name "resource.read"))
          (let ((state
                  (workspace-file--observation-state-for-snapshot
                   (tool-context-conversation context) observation visible-ranges)))
            (tool-success
             (workspace-file--read-result state body total-lines truncated-p))))))))

(defmethod resource-tool-edit
    ((resource workspace-file-resource) (tool resource-edit-tool)
     (context tool-context) (arguments hash-table))
  "Apply structured revision-gated operations and return a refreshed observation."
  (declare (ignore tool))
  (let* ((uri (tool-argument arguments "uri" :required t))
         (base-revision (tool-argument arguments "base-revision" :required t))
         (operation-array (tool-argument arguments "operations" :required t)))
    (unless (non-empty-string-p base-revision)
      (error 'tool-error
             :message "Resource edit base-revision must be a non-empty string."
             :tool-name "resource.edit"))
    (unless (and (vectorp operation-array) (plusp (length operation-array)))
      (error 'tool-error
             :message "Resource edit operations must be a non-empty JSON array."
             :tool-name "resource.edit"))
    (let ((operations (coerce operation-array 'list)))
      (handler-case
        (multiple-value-bind (observation normalized)
            (resource-apply-operations resource context
                                       :base-revision base-revision
                                       :operations operations)
          (multiple-value-bind (start-line line-count)
              (workspace-file--operation-window
               normalized (length (workspace-file-observation-lines observation)))
            (multiple-value-bind (body visible-ranges last-line truncated-p)
                (workspace-file--window-ranges observation start-line line-count)
              (declare (ignore last-line))
              (let* ((state
                       (workspace-file--observation-state-for-snapshot
                        (tool-context-conversation context)
                        observation visible-ranges))
                     (result-content
                       (format nil "Applied ~{~A~^; ~}.~%~A"
                               (mapcar
                                (lambda (operation) (getf operation :summary))
                                normalized)
                               (workspace-file--read-result
                                state body
                                (length
                                 (workspace-file-observation-lines observation))
                                truncated-p))))
                (tool-success
                 (lisp-source-edit-result-content
                  result-content
                  (workspace-file-resource-pathname resource)
                  (resource-observation-content observation)))))))
      (resource-revision-stale ()
        (tool-failure
         (format nil "Resource revision ~A is stale, expired, or was not observed in this conversation. Reread ~A with resource.read and retry against the returned revision."
                 base-revision uri)))))))

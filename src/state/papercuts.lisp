(in-package #:autolith)

;;;; -- Persistent Papercuts --

(defparameter *papercut-format-version* 1
  "The readable persistent papercut format version.")

(defparameter *papercut-title-limit* 200
  "The maximum characters in one papercut title.")

(defparameter *papercut-content-limit* 8000
  "The maximum characters in one papercut report.")

(defparameter *papercut-resolution-limit* 1000
  "The maximum characters in one papercut closure resolution.")

(defparameter *papercut-short-identifier-length* 8
  "The characters shown when a papercut identifier is abbreviated.")

(defvar *papercut-lock* (make-lock "Autolith persistent papercuts")
  "The process-local lock serializing papercut reads and appends.")

(defclass papercut ()
  ((identifier
    :initarg :identifier
    :reader papercut-identifier
    :type non-empty-string
    :documentation "The stable identifier of this papercut report.")
   (reported-at
    :initarg :reported-at
    :reader papercut-reported-at
    :type timestamp
    :documentation "The time at which the report was recorded.")
   (workspace
    :initarg :workspace
    :reader papercut-workspace
    :type non-empty-string
    :documentation "The workspace in which the problem was observed.")
   (title
    :initarg :title
    :reader papercut-title
    :type non-empty-string
    :documentation "A short description of the problem.")
   (content
    :initarg :content
    :reader papercut-content
    :type non-empty-string
    :documentation "The complete user-visible problem report.")
   (source-conversation
    :initarg :source-conversation
    :reader papercut-source-conversation
    :type (option string)
    :documentation "The conversation that most recently reported the papercut."))
  (:documentation "One persistent user-visible report of an Autolith problem."))


;;;; -- Validation and Records --

(-> papercut--validate-text (t string integer) string)
(defun papercut--validate-text (value field limit)
  "Return non-empty string VALUE after validating FIELD and LIMIT."
  (unless (non-empty-string-p value)
    (error 'papercut-error
           :message (format nil "Papercut ~A must be a non-empty string." field)
           :pathname #P"papercuts.sexp"
           :identifier nil))
  (when (> (length value) limit)
    (error 'papercut-error
           :message (format nil "Papercut ~A exceeds the ~:D-character limit."
                            field
                            limit)
           :pathname #P"papercuts.sexp"
           :identifier nil))
  value)

(-> papercut--record (papercut) list)
(defun papercut--record (papercut)
  "Return the complete portable record for PAPERCUT."
  (list :papercut
        :version *papercut-format-version*
        :id (papercut-identifier papercut)
        :reported-at (papercut-reported-at papercut)
        :workspace (papercut-workspace papercut)
        :title (papercut-title papercut)
        :content (papercut-content papercut)
        :source-conversation (papercut-source-conversation papercut)))

(-> papercut--closed-record (string string timestamp) list)
(defun papercut--closed-record (identifier resolution closed-at)
  "Return one portable closure record for IDENTIFIER and RESOLUTION."
  (list :papercut-closed
        :version *papercut-format-version*
        :id identifier
        :closed-at closed-at
        :resolution resolution))

(-> papercut--validate-closed-record (pathname list) string)
(defun papercut--validate-closed-record (pathname record)
  "Validate closure RECORD from PATHNAME and return its identifier."
  (let ((version (getf (rest record) :version))
        (identifier (getf (rest record) :id))
        (closed-at (getf (rest record) :closed-at))
        (resolution (getf (rest record) :resolution)))
    (unless (and (eql version *papercut-format-version*)
                 (non-empty-string-p identifier)
                 (typep closed-at 'timestamp))
      (error 'papercut-error
             :message "A papercut closure record has invalid metadata."
             :pathname pathname
             :identifier (and (stringp identifier) identifier)))
    (handler-case
        (papercut--validate-text
         resolution "closure resolution" *papercut-resolution-limit*)
      (papercut-error (condition)
        (error 'papercut-error
               :message (autolith-error-message condition)
               :pathname pathname
               :identifier identifier)))
    identifier))

(-> papercut--record->papercut (pathname list) papercut)
(defun papercut--record->papercut (pathname record)
  "Validate and convert one portable papercut RECORD from PATHNAME."
  (let ((version (getf (rest record) :version))
        (identifier (getf (rest record) :id))
        (reported-at (getf (rest record) :reported-at))
        (workspace (getf (rest record) :workspace))
        (title (getf (rest record) :title))
        (content (getf (rest record) :content))
        (source-conversation (getf (rest record) :source-conversation)))
    (unless (and (eql version *papercut-format-version*)
                 (non-empty-string-p identifier)
                 (typep reported-at 'timestamp)
                 (non-empty-string-p workspace)
                 (or (null source-conversation)
                     (non-empty-string-p source-conversation)))
      (error 'papercut-error
             :message "A persistent papercut record has invalid metadata."
             :pathname pathname
             :identifier (and (stringp identifier) identifier)))
    (handler-case
        (make-instance 'papercut
                       :identifier identifier
                       :reported-at reported-at
                       :workspace workspace
                       :title (papercut--validate-text
                               title "title" *papercut-title-limit*)
                       :content (papercut--validate-text
                                 content "content" *papercut-content-limit*)
                       :source-conversation source-conversation)
      (papercut-error (condition)
        (error 'papercut-error
               :message (autolith-error-message condition)
               :pathname pathname
               :identifier identifier)))))


;;;; -- Readable Log --

(-> papercut--append-record (configuration list) null)
(defun papercut--append-record (configuration record)
  "Append one complete papercut RECORD, atomically creating the log if absent."
  (let ((pathname (configuration-papercut-path configuration)))
    (handler-case
        (log-append
         pathname
         record
         :initial-forms
         (list (list :papercuts :version *papercut-format-version*)))
      (error (cause)
        (error 'papercut-error
               :message (format nil "Could not append papercut: ~A" cause)
               :pathname pathname
               :identifier nil))))
  nil)

(-> papercut--read-forms (pathname) (values list boolean))
(defun papercut--read-forms (pathname)
  "Read complete papercut forms and report an incomplete final form."
  (handler-case
      (log-read pathname)
    (error (cause)
      (error 'papercut-error
             :message (format nil "Malformed persistent papercut data: ~A"
                              cause)
             :pathname pathname
             :identifier nil))))

(-> papercut--replay-unlocked (configuration) list)
(defun papercut--replay-unlocked (configuration)
  "Replay the readable log and return all papercuts."
  (let ((pathname (configuration-papercut-path configuration)))
    (multiple-value-bind (records incomplete-final-form-p)
        (papercut--read-forms pathname)
      (declare (ignore incomplete-final-form-p))
      (when (and (probe-file pathname) (null records))
        (error 'papercut-error
               :message "The persistent papercut file has no complete header."
               :pathname pathname
               :identifier nil))
      (when records
        (let ((header (first records)))
          (unless (and (listp header)
                       (eq (first header) :papercuts)
                       (eql (getf (rest header) :version)
                            *papercut-format-version*))
            (error 'papercut-error
                   :message "The persistent papercut header is missing or unsupported."
                   :pathname pathname
                   :identifier nil))))
      (let ((seen (make-hash-table :test #'equal))
            (active (make-hash-table :test #'equal)))
        (dolist (record (rest records))
          (unless (and (listp record) (keywordp (first record)))
            (error 'papercut-error
                   :message "A persistent papercut record is not a keyword list."
                   :pathname pathname
                   :identifier nil))
          (case (first record)
            (:papercut
             (let* ((papercut (papercut--record->papercut pathname record))
                    (identifier (papercut-identifier papercut)))
               (when (gethash identifier seen)
                 (error 'papercut-error
                        :message (format nil
                                         "Persistent papercut identifier ~A occurs more than once."
                                         identifier)
                        :pathname pathname
                        :identifier identifier))
               (setf (gethash identifier seen) t
                     (gethash identifier active) papercut)))
            (:papercut-closed
             (let ((identifier
                     (papercut--validate-closed-record pathname record)))
               (unless (gethash identifier seen)
                 (error 'papercut-error
                        :message (format nil
                                         "Papercut closure references unknown identifier ~A."
                                         identifier)
                        :pathname pathname
                        :identifier identifier))
               (unless (gethash identifier active)
                 (error 'papercut-error
                        :message (format nil
                                         "Papercut identifier ~A is closed more than once."
                                         identifier)
                        :pathname pathname
                        :identifier identifier))
               (remhash identifier active)))
            (otherwise
             (error 'papercut-error
                    :message (format nil "Unsupported persistent papercut record ~S."
                                     (first record))
                    :pathname pathname
                    :identifier nil))))
        (sort (loop for papercut being the hash-values of active
                    collect papercut)
              (lambda (left right)
                (or (> (papercut-reported-at left)
                       (papercut-reported-at right))
                    (and (= (papercut-reported-at left)
                            (papercut-reported-at right))
                         (string< (papercut-identifier left)
                                  (papercut-identifier right))))))))))

(-> papercut--load-unlocked (configuration) list)
(defun papercut--load-unlocked (configuration)
  "Return papercuts, translating malformed data into PAPERCUT-ERROR."
  (handler-case
      (papercut--replay-unlocked configuration)
    (papercut-error (condition)
      (error condition))
    (error (condition)
      (error 'papercut-error
             :message (format nil "Malformed persistent papercut data: ~A"
                              condition)
             :pathname (configuration-papercut-path configuration)
             :identifier nil))))


;;;; -- Selection and Mutation --

(-> papercut--workspace (configuration) non-empty-string)
(defun papercut--workspace (configuration)
  "Return CONFIGURATION's current workspace identity used by papercut records."
  (namestring (configuration-working-directory configuration)))

(-> papercut--list-unlocked (configuration) list)
(defun papercut--list-unlocked (configuration)
  "Return current-workspace papercuts while the caller holds the papercut lock."
  (let ((workspace (papercut--workspace configuration)))
    (remove-if-not
     (lambda (papercut)
       (string= workspace (papercut-workspace papercut)))
     (papercut--load-unlocked configuration))))

(-> papercut-list (configuration) list)
(defun papercut-list (configuration)
  "Return papercuts reported in CONFIGURATION's current workspace, newest first."
  (with-lock-held (*papercut-lock*)
    (papercut--list-unlocked configuration)))

(-> papercut-find (configuration string) (option papercut))
(defun papercut-find (configuration identifier)
  "Return the exact active papercut IDENTIFIER in CONFIGURATION's workspace."
  (find identifier
        (papercut-list configuration)
        :test #'string=
        :key #'papercut-identifier))

(-> papercut-resolve
    (configuration string)
    (values (option papercut) (member :missing :ambiguous :found) list))
(defun papercut-resolve (configuration identifier)
  "Resolve an exact or unique identifier prefix in the current workspace.

The second value is :FOUND, :MISSING, or :AMBIGUOUS. The third value contains
matching reports for :AMBIGUOUS."
  (if (non-empty-string-p identifier)
      (let* ((papercuts (papercut-list configuration))
             (matches (remove-if-not
                       (lambda (papercut)
                         (uiop:string-prefix-p
                          identifier
                          (papercut-identifier papercut)))
                       papercuts)))
        (cond
          ((null matches)
           (values nil ':missing nil))
          ((null (rest matches))
           (values (first matches) ':found nil))
          (t
           (values nil ':ambiguous matches))))
      (values nil ':missing nil)))

(-> papercut--report-unlocked
    (configuration non-empty-string non-empty-string (option string))
    papercut)
(defun papercut--report-unlocked (configuration title content source-conversation)
  "Append one validated report while the caller holds the papercut lock."
  (let ((papercut
          (make-instance
           'papercut
           :identifier (make-identifier)
           :reported-at (get-universal-time)
           :workspace (papercut--workspace configuration)
           :title title
           :content content
           :source-conversation source-conversation)))
    (papercut--append-record configuration (papercut--record papercut))
    papercut))

(-> papercut-report
    (configuration &key (:title string) (:content string)
                   (:source-conversation (option string)))
    papercut)
(defun papercut-report (configuration &key title content source-conversation)
  "Record one new user-visible report about a problem in the current workspace."
  (let ((validated-title
          (papercut--validate-text title "title" *papercut-title-limit*))
        (validated-content
          (papercut--validate-text content "content" *papercut-content-limit*)))
    (unless (or (null source-conversation)
                (non-empty-string-p source-conversation))
      (error 'papercut-error
             :message "Papercut source conversation must be a non-empty string."
             :pathname (configuration-papercut-path configuration)
             :identifier nil))
    (with-lock-held (*papercut-lock*)
      (papercut--report-unlocked
       configuration validated-title validated-content source-conversation))))

(-> papercut--mark-closed-unlocked
    (configuration non-empty-string non-empty-string)
    papercut)
(defun papercut--mark-closed-unlocked (configuration identifier resolution)
  "Close one validated active report while the caller holds the papercut lock."
  (let* ((workspace (papercut--workspace configuration))
         (papercut
           (find-if
            (lambda (candidate)
              (and (string= identifier (papercut-identifier candidate))
                   (string= workspace (papercut-workspace candidate))))
            (papercut--load-unlocked configuration))))
    (unless papercut
      (error 'papercut-error
             :message (format nil
                              "No active papercut ~A exists in this workspace."
                              identifier)
             :pathname (configuration-papercut-path configuration)
             :identifier identifier))
    (papercut--append-record
     configuration
     (papercut--closed-record identifier resolution (get-universal-time)))
    papercut))

(-> papercut-mark-closed (configuration string &key (:resolution string)) papercut)
(defun papercut-mark-closed (configuration identifier &key resolution)
  "Close active papercut IDENTIFIER with a durable RESOLUTION and return it."
  (unless (non-empty-string-p identifier)
    (error 'papercut-error
           :message "Papercut identifier must be a non-empty string."
           :pathname (configuration-papercut-path configuration)
           :identifier nil))
  (let ((validated-resolution
          (papercut--validate-text
           resolution "closure resolution" *papercut-resolution-limit*)))
    (with-lock-held (*papercut-lock*)
      (papercut--mark-closed-unlocked
       configuration identifier validated-resolution))))


;;;; -- Presentation Values --

(-> papercut-timestamp-string (timestamp) string)
(defun papercut-timestamp-string (timestamp)
  "Return TIMESTAMP as an ISO-8601 UTC string."
  (multiple-value-bind (second minute hour date month year)
      (decode-universal-time timestamp 0)
    (format nil "~4,'0D-~2,'0D-~2,'0DT~2,'0D:~2,'0D:~2,'0DZ"
            year month date hour minute second)))

(-> papercut-short-identifier (papercut) string)
(defun papercut-short-identifier (papercut)
  "Return the stable abbreviated identifier shown in the terminal."
  (subseq (papercut-identifier papercut)
          0
          (min *papercut-short-identifier-length*
               (length (papercut-identifier papercut)))))

(-> papercut-call-source (papercut) string)
(defun papercut-call-source (papercut)
  "Return the canonical Lisp call that opens PAPERCUT."
  (format nil "(papercut ~S)" (papercut-short-identifier papercut)))

(-> papercut-excerpt (string integer) string)
(defun papercut-excerpt (content limit)
  "Return a single-line prefix of CONTENT no longer than LIMIT characters."
  (let* ((single-line
           (with-output-to-string (stream)
             (loop with spacing-p = nil
                   for character across content
                   if (find character '(#\Space #\Tab #\Newline #\Return))
                     do (unless spacing-p
                          (write-char #\Space stream)
                          (setf spacing-p t))
                   else
                     do (write-char character stream)
                        (setf spacing-p nil))))
         (trimmed (string-trim '(#\Space) single-line)))
    (if (<= (length trimmed) limit)
        trimmed
        (format nil "~A..." (subseq trimmed 0 (max 0 (- limit 3)))))))

(in-package #:autolith)

;;;; -- Persistent Papercuts --

(defparameter *papercut-format-version* 1
  "The readable persistent papercut format version.")

(defparameter *papercut-title-limit* 200
  "The maximum characters in one papercut title.")

(defparameter *papercut-content-limit* 8000
  "The maximum characters in one papercut report.")

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
            (papercuts nil))
        (dolist (record (rest records))
          (unless (and (listp record) (keywordp (first record)))
            (error 'papercut-error
                   :message "A persistent papercut record is not a keyword list."
                   :pathname pathname
                   :identifier nil))
          (unless (eq (first record) :papercut)
            (error 'papercut-error
                   :message (format nil "Unsupported persistent papercut record ~S."
                                    (first record))
                   :pathname pathname
                   :identifier nil))
          (let ((papercut (papercut--record->papercut pathname record)))
            (when (gethash (papercut-identifier papercut) seen)
              (error 'papercut-error
                     :message (format nil
                                      "Persistent papercut identifier ~A occurs more than once."
                                      (papercut-identifier papercut))
                     :pathname pathname
                     :identifier (papercut-identifier papercut)))
            (setf (gethash (papercut-identifier papercut) seen) t)
            (push papercut papercuts)))
        (sort papercuts
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

(-> papercut-list (configuration) list)
(defun papercut-list (configuration)
  "Return papercuts reported in CONFIGURATION's current workspace, newest first."
  (let ((workspace (namestring (configuration-working-directory configuration))))
    (with-lock-held (*papercut-lock*)
      (remove-if-not
       (lambda (papercut)
         (string= workspace (papercut-workspace papercut)))
       (papercut--load-unlocked configuration)))))

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
      (let ((papercut
              (make-instance
               'papercut
               :identifier (make-identifier)
               :reported-at (get-universal-time)
               :workspace (namestring (configuration-working-directory configuration))
               :title validated-title
               :content validated-content
               :source-conversation source-conversation)))
        (papercut--append-record configuration (papercut--record papercut))
        papercut))))


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

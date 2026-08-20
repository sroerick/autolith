(in-package #:autolith)

;;;; -- Persistent Deferred Inputs --

(defparameter *later-version* 2
  "The readable deferred-input state format version.")

(defparameter *later-supported-versions* (list 1 2)
  "Readable deferred-input state versions this image can restore.")

(defclass later-entry ()
  ((identifier
    :initarg :identifier
    :reader later-entry-identifier
    :type non-empty-string
    :documentation "The stable identifier of this deferred input.")
   (input
    :initarg :input
    :reader later-entry-input
    :type non-empty-string
    :documentation "The ordinary user input dispatched when this entry is due.")
   (directory
    :initarg :directory
    :reader later-entry-directory
    :type non-empty-string
    :documentation "The canonical workspace in which this input was scheduled.")
   (due-at
    :initarg :due-at
    :accessor later-entry-due-at
    :type timestamp
    :documentation "The universal time at which this input becomes runnable.")
   (created-at
    :initarg :created-at
    :reader later-entry-created-at
    :type timestamp
    :documentation "The universal time at which this input was scheduled.")
   (window
    :initarg :window
    :accessor later-entry-window
    :type non-empty-string
    :documentation "The rate-limit window or estimate governing DUE-AT.")
   (conversation
    :initarg :conversation
    :initform nil
    :reader later-entry-conversation
    :type (option non-empty-string)
    :documentation
    "The conversation that scheduled this input. Version 1 entries carry NIL
and stay runnable from any conversation, because their origin was never
recorded."))
  (:documentation "One durable input waiting for a rate-limit reset."))

(defclass later-state ()
  ((queue
    :initarg :queue
    :initform (later--make-queue)
    :accessor later-state-queue
    :type priority-queue
    :documentation "Deferred entries indexed and ordered for in-process scheduling.")
   (active-entry
    :initform nil
    :accessor later-state-active-entry
    :type (option later-entry)
    :documentation "The deferred entry dispatched but not yet completed."))
  (:documentation "Validated deferred inputs restored across Autolith processes."))

(-> later--entry-form-p (t) boolean)
(defun later--entry-form-p (form)
  "Return true when FORM is one complete portable deferred entry."
  (handler-case
      (and (consp form)
           (eq (first form) ':entry)
           (let ((properties (rest form)))
             (and (member (length properties) '(12 14))
                  (every (lambda (property)
                           (readable-state-property-present-p properties
                                                              property))
                         '(:id :input :directory :due-at :created-at :window))
                  (non-empty-string-p (getf properties :id))
                  (non-empty-string-p (getf properties :input))
                  (non-empty-string-p (getf properties :directory))
                  (typep (getf properties :due-at) 'timestamp)
                  (typep (getf properties :created-at) 'timestamp)
                  (non-empty-string-p (getf properties :window))
                  ;; Version 1 entries predate origin scoping and omit the
                  ;; property entirely.
                  (or (= (length properties) 12)
                      (and (readable-state-property-present-p properties
                                                              ':conversation)
                           (typep (getf properties :conversation)
                                  '(or null non-empty-string)))))))
    (error ()
      nil)))

(-> later--form-p (t) boolean)
(defun later--form-p (form)
  "Return true when FORM is one supported deferred-input state."
  (handler-case
      (and (listp form)
           (= (length form) 5)
           (eq (first form) ':later)
           (eq (second form) ':version)
           (member (third form) *later-supported-versions*)
           (eq (fourth form) ':entries)
           (listp (fifth form))
           (every #'later--entry-form-p (fifth form))
           (= (length (remove-duplicates
                       (mapcar (lambda (entry)
                                 (getf (rest entry) :id))
                               (fifth form))
                       :test #'string=))
              (length (fifth form))))
    (error ()
      nil)))

(-> later--entry-form->entry (list) later-entry)
(defun later--entry-form->entry (form)
  "Return the deferred entry represented by validated FORM."
  (let ((properties (rest form)))
    (make-instance 'later-entry
                   :identifier (copy-seq (getf properties :id))
                   :input (copy-seq (getf properties :input))
                   :directory (copy-seq (getf properties :directory))
                   :due-at (getf properties :due-at)
                   :created-at (getf properties :created-at)
                   :window (copy-seq (getf properties :window))
                   :conversation
                   (let ((conversation (getf properties :conversation)))
                     (and (non-empty-string-p conversation)
                          (copy-seq conversation))))))

(-> later--entry< (later-entry later-entry) boolean)
(defun later--entry< (left right)
  "Return true when LEFT's deadline and creation time precede RIGHT's."
  (or (< (later-entry-due-at left) (later-entry-due-at right))
      (and (= (later-entry-due-at left) (later-entry-due-at right))
           (< (later-entry-created-at left) (later-entry-created-at right)))))

(-> later--make-queue (&optional list) priority-queue)
(defun later--make-queue (&optional entries)
  "Return an indexed stable priority queue containing ENTRIES."
  (let ((queue (make-priority-queue :lessp #'later--entry<
                                     :key-function #'later-entry-identifier
                                     :key-test 'equal)))
    (dolist (entry entries queue)
      (priority-queue-push queue entry entry))))

(-> later-state-entries (later-state) list)
(defun later-state-entries (state)
  "Return STATE's deferred entries as a detached ordered list."
  (priority-queue->list (later-state-queue state)))

(-> later--read (configuration) later-state)
(defun later--read (configuration)
  "Read CONFIGURATION's deferred inputs or return an empty state."
  (block nil
    (let ((pathname (configuration-later-path configuration)))
      (unless (probe-file pathname)
        (return (make-instance 'later-state)))
      (handler-case
          (multiple-value-bind (form sole-form-p)
              (snapshot-read pathname)
            (unless (and sole-form-p (later--form-p form))
              (error 'later-error
                     :message (format nil
                                      "Deferred inputs at ~A are malformed or unsupported."
                                      pathname)
                     :pathname pathname
                     :operation ':read
                     :cause nil))
             (make-instance
              'later-state
              :queue
              (later--make-queue
               (mapcar #'later--entry-form->entry (fifth form)))))
        (later-error (condition)
          (error condition))
        (error (cause)
          (error 'later-error
                 :message (format nil "Could not read deferred inputs at ~A: ~A"
                                  pathname cause)
                 :pathname pathname
                 :operation ':read
                 :cause cause))))))

(-> later-load (configuration) later-state)
(defun later-load (configuration)
  "Return deferred inputs, warning and using an empty queue after corruption."
  (handler-case
      (later--read configuration)
    (later-error (condition)
      (warn 'later-load-warning
            :pathname (later-error-pathname condition)
            :cause condition)
      (make-instance 'later-state))))

(-> later--entry->form (later-entry) list)
(defun later--entry->form (entry)
  "Return ENTRY as one portable readable form."
  (list :entry
        :id (later-entry-identifier entry)
        :input (later-entry-input entry)
        :directory (later-entry-directory entry)
        :due-at (later-entry-due-at entry)
        :created-at (later-entry-created-at entry)
        :window (later-entry-window entry)
        :conversation (later-entry-conversation entry)))

(-> later--state-form (later-state &optional list) list)
(defun later--state-form (state &optional (entries (later-state-entries state)))
  "Return STATE as one portable readable form."
  (list :later :version *later-version* :entries
        (mapcar #'later--entry->form entries)))

(-> later--write (configuration later-state &optional list) null)
(defun later--write (configuration state &optional (entries nil entries-p))
  "Atomically persist deferred input STATE with private file permissions."
  (let ((pathname (configuration-later-path configuration)))
    (handler-case
        (snapshot-write pathname
                        (later--state-form state (if entries-p entries
                                                    (later-state-entries state))))
      (error (cause)
        (error 'later-error
               :message (format nil "Could not persist deferred inputs at ~A: ~A"
                                pathname cause)
               :pathname pathname
               :operation ':write
               :cause cause))))
  nil)

(-> later-schedule
    (&key (:configuration configuration) (:state later-state)
          (:input string) (:directory pathname) (:due-at timestamp)
          (:window string) (:conversation (option string))
          (:created-at timestamp))
    later-entry)
(defun later-schedule
    (&key configuration state input directory due-at window conversation
          (created-at (get-universal-time)))
  "Persist INPUT in DIRECTORY for DUE-AT and return its new deferred entry.

CONVERSATION records the origin so the input runs only there. Omitting it
leaves the entry runnable from any conversation, which only version 1 state
relies on."
  (unless (and (non-empty-string-p input)
               (non-empty-string-p window))
    (error 'later-error
           :message "A deferred input and rate-limit window are required."
           :pathname (configuration-later-path configuration)
           :operation ':validate
           :cause nil))
  (let ((existing-directory (uiop:directory-exists-p directory)))
    (unless existing-directory
      (error 'later-error
             :message (format nil "Deferred-input directory ~A does not exist."
                              directory)
             :pathname (configuration-later-path configuration)
             :operation ':validate
             :cause nil))
    (let ((entry
            (make-instance 'later-entry
                           :identifier (make-identifier)
                           :input (copy-seq input)
                           :directory
                           (namestring
                            (uiop:ensure-directory-pathname
                             (truename existing-directory)))
                           :due-at due-at
                           :created-at created-at
                           :window (copy-seq window)
                           :conversation
                           (and (non-empty-string-p conversation)
                                (copy-seq conversation)))))
      (priority-queue-push (later-state-queue state) entry entry)
      (handler-case
          (progn (later--write configuration state) entry)
        (error (condition)
          (priority-queue-cancel (later-state-queue state)
                                 (later-entry-identifier entry))
          (error condition))))))

(-> later-cancel (configuration later-state string) boolean)
(defun later-cancel (configuration state identifier)
  "Remove IDENTIFIER from STATE durably and report whether it existed."
  (let* ((entries (later-state-entries state))
         (replacement
           (remove identifier entries :key #'later-entry-identifier :test #'string=)))
    (unless (= (length entries) (length replacement))
      (later--write configuration state replacement)
      (priority-queue-cancel (later-state-queue state) identifier)
      (when (and (later-state-active-entry state)
                 (string= identifier
                          (later-entry-identifier (later-state-active-entry state))))
        (setf (later-state-active-entry state) nil))
      t)))

(-> later-entry-runnable-p (later-entry (option string)) boolean)
(defun later-entry-runnable-p (entry conversation)
  "Return true when CONVERSATION may run ENTRY.

An entry runs only in the conversation that scheduled it. Version 1 entries
recorded no origin and stay runnable anywhere."
  (let ((origin (later-entry-conversation entry)))
    (or (null origin)
        (and (non-empty-string-p conversation)
             (string= origin conversation)))))

(-> later-next-entry (later-state (option string)) (option later-entry))
(defun later-next-entry (state conversation)
  "Return STATE's earliest entry CONVERSATION may run, due or not.

The scheduler waits on this entry rather than the queue head, so a deadline
belonging to another conversation never wakes this one."
  (find-if (lambda (entry) (later-entry-runnable-p entry conversation))
           (later-state-entries state)))

(-> later-pop-due (later-state timestamp (option string)) (option later-entry))
(defun later-pop-due (state now conversation)
  "Mark STATE's next entry for CONVERSATION active when it is due at NOW."
  (unless (later-state-active-entry state)
    (let ((entry (later-next-entry state conversation)))
      (when (and entry (<= (later-entry-due-at entry) now))
        (setf (later-state-active-entry state) entry)))))
(-> later--window-exhausted-p ((option list)) boolean)
(defun later--window-exhausted-p (window)
  "Return true when WINDOW reports all of its allowance used."
  (let ((used (and window (getf window :used-percent))))
    (and (realp used) (>= used 100))))

(-> later--window-label ((option list) string) string)
(defun later--window-label (window fallback)
  "Return a compact label for rate-limit WINDOW or FALLBACK."
  (let ((minutes (and window (getf window :window-minutes))))
    (cond
      ((and (integerp minutes) (<= 285 minutes 315))
       "5h")
      ((and (integerp minutes) (<= 9576 minutes 10584))
       "weekly")
      ((integerp minutes)
       (format nil "~D minute~:P" minutes))
      (t
       fallback))))

(-> later-reset-deadline
    ((option list) &key (:now timestamp))
    (values (option timestamp) (option string)))
(defun later-reset-deadline (snapshot &key (now (get-universal-time)))
  "Return the reset deadline and window label selected from SNAPSHOT.

An exhausted secondary window governs because a primary reset cannot unblock
it. Otherwise the reported primary reset is used. When the primary window is
temporarily absent, estimate five hours without crossing a reported secondary
reset. Five seconds of margin avoids dispatching on the reset boundary."
  (let* ((primary (and snapshot (getf snapshot :primary)))
         (secondary (and snapshot (getf snapshot :secondary)))
         (primary-reset (and primary (getf primary :resets-at)))
         (secondary-reset (and secondary (getf secondary :resets-at))))
    (cond
      ((later--window-exhausted-p secondary)
       (if (typep secondary-reset 'timestamp)
           (values (max now (+ secondary-reset 5))
                   (later--window-label secondary "secondary"))
           (values nil nil)))
      ((typep primary-reset 'timestamp)
       (values (max now (+ primary-reset 5))
               (later--window-label primary "primary")))
      (secondary
       (let ((estimate (+ now (* 5 60 60))))
         (values (if (typep secondary-reset 'timestamp)
                     (min estimate (+ secondary-reset 5))
                     estimate)
                 "estimated 5h")))
      (t
       (values nil nil)))))

(-> later-reschedule
    (&key (:configuration configuration) (:state later-state)
          (:entry later-entry) (:due-at timestamp) (:window string))
    later-entry)
(defun later-reschedule (&key configuration state entry due-at window)
  "Persist active ENTRY with a replacement DUE-AT and WINDOW."
  (unless (eq entry (later-state-active-entry state))
    (error 'later-error
           :message (format nil "Deferred input ~A is not active."
                            (later-entry-identifier entry))
           :pathname (configuration-later-path configuration)
           :operation ':reschedule
           :cause nil))
  (let ((old-due-at (later-entry-due-at entry))
        (old-window (later-entry-window entry)))
    (setf (later-entry-due-at entry) due-at
          (later-entry-window entry) (copy-seq window))
    (priority-queue-change-priority
     (later-state-queue state) (later-entry-identifier entry) entry)
    (handler-case
        (progn
          (later--write configuration state)
          (setf (later-state-active-entry state) nil)
          entry)
      (error (condition)
        (setf (later-entry-due-at entry) old-due-at
              (later-entry-window entry) old-window)
        (priority-queue-change-priority
         (later-state-queue state) (later-entry-identifier entry) entry)
        (error condition)))))

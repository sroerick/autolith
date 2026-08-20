(in-package #:autolith)

;;;; -- Deferred Input Tests --

(-> test-later-persistence () null)
(defun test-later-persistence ()
  "Test deferred inputs persist in order and cancel atomically."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration)))
    (unwind-protect
         (let* ((state (later-load configuration))
                (directory (configuration-working-directory configuration))
                 (last-entry
                   (later-schedule :configuration configuration :state state :input "last"
                                   :directory directory :due-at 200 :window "weekly" :created-at 20))
                 (tie-entry
                   (later-schedule :configuration configuration :state state :input "second"
                                   :directory directory :due-at 100 :window "5h" :created-at 10))
                 (earlier-entry
                   (later-schedule :configuration configuration :state state :input "first"
                                   :directory directory :due-at 90 :window "5h" :created-at 10))
                 (created-earlier-entry
                   (later-schedule :configuration configuration :state state :input "created earlier"
                                   :directory directory :due-at 100 :window "5h" :created-at 5))
                 (loaded (later-load configuration)))
             (later-reschedule :configuration configuration :state loaded
                               :entry (later-pop-due loaded 90 nil)
                               :due-at 100 :window "5h")
             (test-call-with-function-replacements
              (list (list 'later--write (lambda (&rest arguments)
                                         (declare (ignore arguments))
                                         (error "forced write failure"))))
              (lambda ()
                 (ignore-errors (later-cancel configuration loaded
                                              (later-entry-identifier tie-entry)))))
             (test-assert
              (equal (mapcar #'later-entry-input (later-state-entries loaded))
                     '("created earlier" "first" "second" "last"))
              "rescheduling and failed cancellation preserve exact-tie FIFO")
           (test-assert
            (and (later-cancel configuration loaded
                               (later-entry-identifier earlier-entry))
                 (not (later-cancel configuration loaded "missing")))
            "deferred cancellation reports exact identifiers")
            (test-assert
             (equal (mapcar #'later-entry-identifier
                            (later-state-entries (later-load configuration)))
                    (mapcar #'later-entry-identifier
                            (list created-earlier-entry tie-entry last-entry)))
             "deferred cancellation persists without disturbing other entries")
           (test-assert (= (logand (sb-posix:stat-mode
                                    (sb-posix:stat
                                     (namestring
                                      (configuration-later-path configuration))))
                                   #o777)
                           #o600)
                        "deferred state is private to the current user"))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-later-conversation-scope () null)
(defun test-later-conversation-scope ()
  "Test deferred inputs run only in the conversation that scheduled them."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (pathname (configuration-later-path configuration)))
    (unwind-protect
         (let* ((state (later-load configuration))
                (directory (configuration-working-directory configuration))
                (entry
                  (later-schedule :configuration configuration :state state
                                  :input "then make a release"
                                  :directory directory :due-at 100
                                  :window "weekly" :created-at 10
                                  :conversation "z-2x75cm")))
           (test-assert
            (null (later-pop-due state 200 "fresh-session"))
            "a due entry stays queued outside its own conversation")
           (test-assert
            (null (later-pop-due state 200 nil))
            "a due entry stays queued when no conversation is active")
           (test-assert
            (null (later-next-entry state "fresh-session"))
            "the scheduler never waits on another conversation's deadline")
           (test-assert
            (eq (later-pop-due state 200 "z-2x75cm") entry)
            "a due entry runs in the conversation that scheduled it")
           (setf (later-state-active-entry state) nil)
           (test-assert
            (string= (later-entry-conversation
                      (first (later-state-entries (later-load configuration))))
                     "z-2x75cm")
            "the scheduling conversation survives a reload")
           ;; Version 1 entries recorded no origin, so they must stay runnable
           ;; instead of stranding in the queue forever.
           (snapshot-write
            pathname
            (list :later :version 1 :entries
                  (list (list :entry
                              :id "legacy-entry"
                              :input "legacy input"
                              :directory (namestring directory)
                              :due-at 100
                              :created-at 10
                              :window "weekly"))))
           (let ((legacy (later-load configuration)))
             (test-assert
              (null (later-entry-conversation
                     (first (later-state-entries legacy))))
              "version 1 entries load without an origin conversation")
             (test-assert
              (and (later-pop-due legacy 200 "any-conversation") t)
              "version 1 entries remain runnable from any conversation")))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-later-malformed-state () null)
(defun test-later-malformed-state ()
  "Test malformed deferred state cannot evaluate reader forms."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (pathname (configuration-later-path configuration))
         (*later-reader-evaluated-p* nil))
    (declare (special *later-reader-evaluated-p*))
    (unwind-protect
         (progn
           (ensure-directories-exist pathname)
           (with-open-file (stream pathname
                                   :direction ':output
                                   :if-exists ':supersede
                                   :if-does-not-exist ':create)
             (write-string
              "#.(setf autolith::*later-reader-evaluated-p* t)"
              stream))
           (handler-bind ((later-load-warning #'muffle-warning))
             (test-assert (null (later-state-entries
                                 (later-load configuration)))
                          "malformed deferred state loads as an empty queue"))
           (test-assert (null *later-reader-evaluated-p*)
                        "deferred state disables reader evaluation"))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-later-reset-selection () null)
(defun test-later-reset-selection ()
  "Test primary, secondary, and estimated reset selection."
  (multiple-value-bind (due-at window)
      (later-reset-deadline
       '(:primary (:used-percent 100 :window-minutes 300 :resets-at 2000)
         :secondary (:used-percent 50 :window-minutes 10080 :resets-at 9000))
       :now 1000)
    (test-assert (and (= due-at 2005) (string= window "5h"))
                 "a usable primary reset schedules the five-hour window"))
  (multiple-value-bind (due-at window)
      (later-reset-deadline
       '(:primary (:used-percent 100 :window-minutes 300 :resets-at 2000)
         :secondary (:used-percent 100 :window-minutes 10080 :resets-at 9000))
       :now 1000)
    (test-assert (and (= due-at 9005) (string= window "weekly"))
                 "an exhausted secondary window overrides the primary reset"))
  (multiple-value-bind (due-at window)
      (later-reset-deadline
       '(:secondary (:used-percent 50 :window-minutes 10080 :resets-at 50000))
       :now 1000)
    (test-assert (and (= due-at 19000) (string= window "estimated 5h"))
                 "a missing primary window uses a bounded five-hour estimate"))
  (multiple-value-bind (due-at window)
      (later-reset-deadline
       '(:secondary (:used-percent 100 :window-minutes 10080))
       :now 1000)
    (test-assert (and (null due-at) (null window))
                 "an exhausted window without a reset refuses to guess"))
  nil)

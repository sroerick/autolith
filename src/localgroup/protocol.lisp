(in-package #:autolith)

;;;; -- Localgroup Protocol Bridge --

;;; The wire protocol lives in the image-daemon library. This file keeps
;;; Autolith's condition bridge, startup handoff state, and the small
;;; process helpers the localgroup runtime shares.

(defvar *localgroup-startup-record* nil
  "The validated detached-process handoff record active during startup.")

(-> localgroup-startup-detached-p () boolean)
(defun localgroup-startup-detached-p ()
  "Return true while startup is reconstructing a detached localgroup process."
  (not (null *localgroup-startup-record*)))

(defparameter *localgroup-thread-stop-timeout-seconds* 1
  "The maximum seconds spent waiting for one transport thread to stop.")

(-> localgroup-stop-thread (t) null)
(defun localgroup-stop-thread (thread)
  "Boundedly stop and reap THREAD when it is live and not the caller."
  (when (and thread
             (not (eq thread (current-thread)))
             (thread-alive-p thread))
    (handler-case
        (sb-ext:with-timeout *localgroup-thread-stop-timeout-seconds*
          (join-thread thread))
      (sb-ext:timeout ()
        (when (thread-alive-p thread)
          (ignore-errors (sb-thread:terminate-thread thread)))
        (ignore-errors (join-thread thread)))))
  nil)

;; These runtime functions load after the responsive input implementation.
(-> application-localgroup-paused-p (t) boolean)
(-> application-localgroup-resume (t) boolean)
(-> application-localgroup-request-handoff (t keyword) list)
(-> application-localgroup-handoff-pending-p (t) boolean)
(-> application-localgroup-take-ready-handoff (t) (option keyword))
(-> application-localgroup-run-handoff (t keyword t) null)
(-> localgroup-handoff-assert-startup-active () null)
(-> localgroup-handoff-finish-startup (t) null)

(define-condition localgroup-error (image-daemon:daemon-error autolith-error)
  ()
  (:documentation
   "A localgroup failure joined to Autolith's condition hierarchy."))

(-> localgroup--proper-list-p (t) boolean)
(defun localgroup--proper-list-p (value)
  "Return true when VALUE is a proper list."
  (and (listp value)
       (handler-case
           (not (null (list-length value)))
         (type-error ()
           nil))))


;;;; -- image-daemon Host Wiring --

(setf image-daemon:*daemon-error-class* 'localgroup-error)

(in-package #:autolith)

;;;; -- Localgroup Checkpoint Quiescence --

(-> application-call-with-localgroup-quiesced (application function) t)
(defun application-call-with-localgroup-quiesced (application function)
  "Call FUNCTION without localgroup threads, preserving the process session identity."
  (let ((session (application-localgroup-session application)))
    (unless session
      (return-from application-call-with-localgroup-quiesced
        (funcall function)))
    (let ((identifier (localgroup-session-identifier session))
          (token (localgroup-session-token session))
          (created-at (localgroup-session-created-at session)))
      (localgroup-stop application)
      (unwind-protect
           (funcall function)
        (unless (application-localgroup-session application)
          (localgroup-start application
                            :identifier identifier
                            :token token
                            :created-at created-at))))))

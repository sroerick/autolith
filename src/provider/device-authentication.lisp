(in-package #:autolith)

;;;; -- RFC 8628 Conditions --

(define-condition device-authentication-error
    (cl-rfc8628:device-authentication-error authentication-error)
  ()
  (:documentation
   "A device authentication failure joined to Autolith's condition hierarchy."))


;;;; -- cl-rfc8628 Host Wiring --

(setf cl-rfc8628:*user-agent-function* #'authentication-user-agent
      cl-rfc8628:*device-authentication-error-class* 'device-authentication-error)

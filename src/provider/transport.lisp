(in-package #:autolith)

;;;; -- Provider Response Deadlines --

(defparameter *provider-error-body-deadline-seconds* 2
  "Maximum seconds spent reading an optional provider error body.")

(-> provider-call-with-response-deadline (real function) t)
(defun provider-call-with-response-deadline (seconds function)
  "Call FUNCTION under an SBCL response deadline of SECONDS, preserving all values."
  (sb-sys:with-deadline (:seconds seconds)
    (funcall function)))

(-> provider--close-response-stream (stream) null)
(defun provider--close-response-stream (stream)
  "Abortively close STREAM without allowing cleanup failure to escape."
  (handler-case
      (when (open-stream-p stream)
        (close stream :abort t))
    (error ()
      nil))
  nil)

(-> provider--drain-error-body (stream) (option string))
(defun provider--drain-error-body (stream)
  "Read a bounded optional error body from STREAM under a short deadline.

Both decoded character streams and undecoded byte streams are accepted because
Dexador chooses the element type from response headers. Read and deadline
failures are diagnostic-only, and STREAM is always abort-closed."
  (unwind-protect
       (handler-case
           (provider-call-with-response-deadline
            *provider-error-body-deadline-seconds*
            (lambda ()
              (if (subtypep (stream-element-type stream) 'character)
                  (let* ((buffer (make-string 4000))
                         (end (read-character-sequence buffer stream)))
                    (and (plusp end) (subseq buffer 0 end)))
                  (let* ((buffer (make-array 4000
                                             :element-type '(unsigned-byte 8)))
                         (end (read-sequence buffer stream)))
                    (and (plusp end)
                         (sb-ext:octets-to-string
                          (subseq buffer 0 end)
                          :external-format ':utf-8))))))
         (sb-sys:deadline-timeout ()
           nil)
         (error ()
           nil))
    (provider--close-response-stream stream)))

(-> provider--error-body-text (t) (option string))
(defun provider--error-body-text (content)
  "Return dependency error CONTENT as displayable text, when it carries any.

Strings pass through, streams are drained and closed under the diagnostic body
deadline, and octet vectors are decoded as UTF-8."
  (handler-case
      (cond
        ((stringp content)
         (and (plusp (length content)) content))
        ((streamp content)
         (provider--drain-error-body content))
        ((and (vectorp content) (plusp (length content)))
         (sb-ext:octets-to-string (coerce content '(vector (unsigned-byte 8)))
                                  :external-format ':utf-8))
        (t
         nil))
    (error ()
      nil)))

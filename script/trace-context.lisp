;;;; Trace continuity shared by the standalone launcher and pristine recovery.

(defun autolith-script-ensure-trace-context ()
  "Create one nonzero W3C context and first attempt only when not inherited."
  (unless (uiop:getenv "AUTOLITH_TRACEPARENT")
    (let ((state (make-random-state t)))
      (autolith-script-setenv
       "AUTOLITH_TRACEPARENT"
       (string-downcase
        (format nil "00-~32,'0X-~16,'0X-01"
                (1+ (random (1- (expt 2 128)) state))
                (1+ (random (1- (expt 2 64)) state)))))))
  (unless (uiop:getenv "AUTOLITH_RUN_ATTEMPT")
    (autolith-script-setenv "AUTOLITH_RUN_ATTEMPT" "1"))
  nil)

(defun autolith-script-advance-run-attempt ()
  "Retain the launcher trace and advance the bounded attempt for a recovered child."
  (autolith-script-ensure-trace-context)
  (let* ((text (uiop:getenv "AUTOLITH_RUN_ATTEMPT"))
         (valid-p (and text (<= 1 (length text) 5)
                       (every (lambda (character) (find character "0123456789")) text)
                       (<= 1 (parse-integer text) 65535)))
         (attempt (if valid-p (parse-integer text) 1)))
    (unless valid-p
      (format *error-output* "[telemetry warning] Invalid AUTOLITH_RUN_ATTEMPT; using attempt 1.~%"))
    (when (= attempt 65535)
      (format *error-output* "[telemetry warning] Run attempt limit reached; retaining attempt 65535.~%"))
    (autolith-script-setenv "AUTOLITH_RUN_ATTEMPT" (write-to-string (min 65535 (1+ attempt)))))
  nil)

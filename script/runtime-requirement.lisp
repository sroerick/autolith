;;;; Minimum SBCL runtime enforcement shared by standalone build scripts.

(defun autolith-version-components (version)
  "Return three numeric components for exact release VERSION, or nil."
  (let ((fields (uiop:split-string version :separator ".")))
    (when (and (= (length fields) 3)
               (every (lambda (field)
                        (and (plusp (length field))
                             (every #'digit-char-p field)))
                      fields))
      (mapcar #'parse-integer fields))))

(defun autolith-version-at-least-p (candidate minimum)
  "Return whether release CANDIDATE is at least release MINIMUM."
  (let ((candidate-components (autolith-version-components candidate))
        (minimum-components (autolith-version-components minimum)))
    (and candidate-components
         minimum-components
         (loop for candidate-component in candidate-components
               for minimum-component in minimum-components
               when (> candidate-component minimum-component) return t
               when (< candidate-component minimum-component) return nil
               finally (return t)))))

(defun autolith-require-minimum-runtime (version-pathname)
  "Signal an error unless this process satisfies the version at VERSION-PATHNAME."
  (let ((minimum (string-trim '(#\Space #\Tab #\Newline #\Return)
                              (uiop:read-file-string version-pathname))))
    (unless (autolith-version-components minimum)
      (error "Autolith's tracked minimum SBCL version is malformed: ~S."
             minimum))
    (unless (autolith-version-at-least-p (lisp-implementation-version) minimum)
      (error "Autolith needs release SBCL ~A or newer, but this process is SBCL ~A. Set AUTOLITH_SBCL to a suitable release executable."
             minimum
             (lisp-implementation-version)))))

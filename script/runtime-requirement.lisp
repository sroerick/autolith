;;;; Minimum SBCL runtime enforcement shared by standalone build scripts.

(defun autolith-version-components (version &key allow-suffix-p)
  "Return three numeric version components, optionally accepting an SBCL build suffix.
Tracked release versions use the strict default. Host versions may append a
nonempty dot, hyphen, or plus suffix containing letters, digits, dots, underscores,
hyphens, and plus signs. The complete host identity is preserved by callers."
  (block invalid
    (let ((components nil)
          (start 0)
          (length (length version)))
      (dotimes (index 3)
        (let ((end (or (position-if-not #'digit-char-p version :start start) length)))
          (when (= end start)
            (return-from invalid nil))
          (push (parse-integer version :start start :end end) components)
          (if (< index 2)
              (progn
                (unless (and (< end length) (char= (char version end) #\.))
                  (return-from invalid nil))
                (setf start (1+ end)))
              (unless (or (= end length)
                          (and allow-suffix-p (< (1+ end) length)
                               (find (char version end) ".-+")
                               (alphanumericp (char version (1+ end)))
                               (every (lambda (character)
                                        (or (alphanumericp character)
                                            (find character ".-+_")))
                                      (subseq version (1+ end)))))
                (return-from invalid nil)))))
      (nreverse components))))

(defun autolith-version-at-least-p (candidate minimum)
  "Compare a host SBCL CANDIDATE's numeric version with the strict release MINIMUM."
  (let ((candidate-components (autolith-version-components candidate :allow-suffix-p t))
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
      (error "Autolith needs SBCL ~A or newer, but this process is SBCL ~A. Set AUTOLITH_SBCL to a suitable executable."
             minimum
             (lisp-implementation-version)))))

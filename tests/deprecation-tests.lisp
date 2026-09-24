(in-package #:autolith)

;;;; -- Legacy Configuration Deprecation Tests --

(define-deprecated-function deprecation-tests--legacy-double (value)
    "(deprecation-tests--double value)"
  "Return VALUE doubled through a legacy entry point."
  (* value 2))

(-> test-version-comparison () null)
(defun test-version-comparison ()
  "Test release version parsing and ordering."
  (dolist (case '(("0.50.3" (0 50 3))
                  ("0.50.3-dev.5" (0 50 3))
                  ("1.0" (1 0))
                  ("2" (2))))
    (destructuring-bind (version components) case
      (test-assert (equal (version-components version) components)
                   (format nil "~A parses into its integer components" version))))
  (dolist (case '(("0.51.0" "0.52.0" t)
                  ("0.52.0" "0.51.0" nil)
                  ("0.52.0" "0.52.0" nil)
                  ("0.52" "0.52.0" nil)
                  ("0.9.9" "0.10.0" t)
                  ("0.53.7-dev.2" "0.54.0" t)
                  ("1.0.0" "0.54.0" nil)))
    (destructuring-bind (left right expected) case
      (test-assert (eq (version< left right) expected)
                   (format nil "~A < ~A is ~A" left right expected))))
  nil)

(-> test-legacy-configuration-phases () null)
(defun test-legacy-configuration-phases ()
  "Test the legacy configuration API is silent, then warns, then is due."
  (dolist (case '(("0.51.0" :silent)
                  ("0.51.9" :silent)
                  ("0.52.0" :warn)
                  ("0.53.7" :warn)
                  ("0.54.0" :due)
                  ("1.0.0" :due)))
    (destructuring-bind (version phase) case
      (let ((*autolith-version* version))
        (test-assert (eq (legacy-configuration-phase) phase)
                     (format nil "release ~A is in the ~(~A~) phase" version phase)))))
  (let ((*autolith-version* "0.51.0"))
    (test-assert
     (= (handler-case (deprecation-tests--legacy-double 2)
          (warning () ':warned))
        4)
     "a deprecated function is silent during the silent phase"))
  (let ((*autolith-version* "0.52.0"))
    (test-assert
     (eq (handler-case (deprecation-tests--legacy-double 2)
           (deprecation-warning (warning)
             (and (eq (deprecation-warning-name warning)
                      'deprecation-tests--legacy-double)
                  (search "deprecation-tests--double"
                          (princ-to-string warning))
                  ':warned)))
         ':warned)
     "a deprecated function warns with its replacement during the warning phase")
    (test-assert
     (= (handler-bind ((deprecation-warning #'muffle-warning))
          (deprecation-tests--legacy-double 2))
        4)
     "a muffled deprecation warning still returns the legacy result"))
  nil)

(-> test-legacy-configuration-removal-due () null)
(defun test-legacy-configuration-removal-due ()
  "Fail from release 0.54.0 until the legacy configuration API is deleted.

This case is a reminder, not a defect: it starts failing in the release that
must delete the versions 1 to 7 preference reader, the migration, and every
deprecated configuration and preference shim."
  (test-assert (not (eq (legacy-configuration-phase) ':due))
               (legacy-configuration-removal-reminder))
  nil)

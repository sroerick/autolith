(in-package #:autolith)

;;;; -- Release Versions --

(-> version-components (string) list)
(defun version-components (version)
  "Return the leading integer components of VERSION, ignoring any suffix.

Development builds such as 0.50.3-dev.5 compare as their base release."
  (let ((components nil)
        (start 0))
    (loop
      (let* ((end (or (position-if-not #'digit-char-p version :start start)
                      (length version)))
             (component (and (> end start)
                             (parse-integer version :start start :end end))))
        (unless component
          (return))
        (push component components)
        (if (and (< end (length version))
                 (char= (char version end) #\.))
            (setf start (1+ end))
            (return))))
    (nreverse components)))

(-> version< (string string) boolean)
(defun version< (left right)
  "Return true when release LEFT precedes release RIGHT."
  (let ((left-components (version-components left))
        (right-components (version-components right)))
    (loop
      (let ((left-component (or (pop left-components) 0))
            (right-component (or (pop right-components) 0)))
        (cond
          ((< left-component right-component)
           (return t))
          ((> left-component right-component)
           (return nil))
          ((and (null left-components) (null right-components))
           (return nil)))))))

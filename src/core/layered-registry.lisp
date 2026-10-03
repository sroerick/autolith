(in-package #:autolith)

;;;; -- Layered Registry Mechanics --

(-> layered-registry-replace
    (list t &key (:key-function function) (:source-function function)
              (:key-test function) (:source keyword))
    list)
(defun layered-registry-replace
    (registrations replacement &key key-function source-function key-test source)
  "Return REGISTRATIONS with REPLACEMENT replacing its source/key layer.
The replacement keeps the existing position; otherwise it is appended."
  (let ((position
          (position-if
           (lambda (registration)
             (and (funcall key-test
                           (funcall key-function registration)
                           (funcall key-function replacement))
                  (eq source (funcall source-function registration))))
           registrations)))
    (if position
        (append (subseq registrations 0 position)
                (list replacement)
                (nthcdr (1+ position) registrations))
        (append registrations (list replacement)))))

(-> layered-registry-remove
    (list t &key (:key-function function) (:source-function function)
              (:key-test function) (:source keyword))
    list)
(defun layered-registry-remove
    (registrations key &key key-function source-function key-test source)
  "Return REGISTRATIONS without the SOURCE layer identified by KEY."
  (remove-if
   (lambda (registration)
     (and (funcall key-test key (funcall key-function registration))
          (eq source (funcall source-function registration))))
   registrations))

(-> layered-registry-remove-source (list keyword function) list)
(defun layered-registry-remove-source (registrations source source-function)
  "Return REGISTRATIONS without any layer attributed to SOURCE."
  (remove source registrations :key source-function :test #'eq))

(-> layered-registry-effective (list function function) list)
(defun layered-registry-effective (registrations key-function source-rank-function)
  "Return effective layers, preserving first-key order and highest rank.
Later layers win when their source ranks tie."
  (let ((order nil)
        (seen (make-hash-table :test #'equal))
        (winners (make-hash-table :test #'equal)))
    (dolist (registration registrations)
      (let ((key (funcall key-function registration)))
        (unless (gethash key seen)
          (setf (gethash key seen) t)
          (push key order))
        (let ((winner (gethash key winners)))
          (when (or (null winner)
                    (>= (funcall source-rank-function registration)
                        (funcall source-rank-function winner)))
            (setf (gethash key winners) registration)))))
    (mapcar (lambda (key) (gethash key winners)) (nreverse order))))

(-> layered-registry-restore-position
    (list t t &key (:key-function function) (:key-test function)
              (:copy-function function))
    list)
(defun layered-registry-restore-position
    (registrations key snapshot &key key-function key-test copy-function)
  "Restore KEY from SNAPSHOT at its recorded position, or remove it.
COPY-FUNCTION detaches the restored registration as required by the caller."
  (let ((remaining
          (remove key registrations :key key-function :test key-test)))
    (if snapshot
        (let* ((position (min (getf snapshot :position) (length remaining)))
               (registration
                 (funcall copy-function (getf snapshot :registration))))
          (append (subseq remaining 0 position)
                  (list registration)
                  (nthcdr position remaining)))
        remaining)))

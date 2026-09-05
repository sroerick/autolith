(in-package #:autolith)

;;;; -- JSON Construction --

(defparameter *json-decoded-false* ':json-false
  "The portable internal marker preserving decoded JSON false across encoding.")

(-> json-object (&rest t) json-object)
(defun json-object (&rest key-values)
  "Return a string-keyed JSON object built from alternating KEY-VALUES."
  (unless (evenp (length key-values))
    (error 'configuration-error
           :message "JSON objects require an even number of key and value arguments."))
  (let ((object (make-hash-table :test #'equal)))
    (loop for (key value) on key-values by #'cddr
          do (unless (stringp key)
               (error 'configuration-error
                      :message (format nil "JSON object key ~S is not a string." key)))
             (setf (gethash key object) value))
    object))

(-> json-array (&rest t) vector)
(defun json-array (&rest elements)
  "Return a JSON array containing ELEMENTS."
  (coerce elements 'vector))

(-> json-object-copy (json-object) json-object)
(defun json-object-copy (object)
  "Return a detached shallow copy of JSON OBJECT."
  (let ((copy (make-hash-table :test (hash-table-test object)
                               :size (max 1 (hash-table-count object)))))
    (maphash (lambda (key value)
               (setf (gethash key copy) value))
             object)
    copy))

(-> json-get-present (json-object string) (values t boolean))
(defun json-get-present (object key)
  "Return KEY from JSON OBJECT and whether the key is present.

Decoded JSON false is presented as NIL while its internal marker remains in
OBJECT so that re-encoding preserves the distinction from JSON null."
  (multiple-value-bind (value present-p)
      (gethash key object)
    (values (if (eq value *json-decoded-false*) nil value)
            present-p)))

(-> json-get (json-object string &optional t) t)
(defun json-get (object key &optional default)
  "Return KEY from JSON OBJECT, or DEFAULT when the key is absent.

Decoded JSON false is presented as NIL while its internal marker remains in
OBJECT so that re-encoding preserves the distinction from JSON null."
  (multiple-value-bind (value present-p)
      (json-get-present object key)
    (if present-p value default)))

(-> json-string= (t string) boolean)
(defun json-string= (value expected)
  "Return true when VALUE is the JSON string EXPECTED."
  (and (stringp value) (string= value expected)))

(-> json-string-member-p (t list) boolean)
(defun json-string-member-p (value expected)
  "Return true when VALUE is a JSON string in EXPECTED."
  (not (null (and (stringp value)
                  (member value expected :test #'string=)))))

(-> json--encoding-value (json-value) json-value)
(defun json--encoding-value (value)
  "Return VALUE with decoded-false markers translated for Yason encoding."
  (cond
    ((eq value *json-decoded-false*)
     yason:false)
    ((json-object-p value)
     (let ((copy (make-hash-table :test (hash-table-test value)
                                  :size (max 1 (hash-table-count value)))))
       (maphash (lambda (key child)
                  (setf (gethash key copy) (json--encoding-value child)))
                value)
       copy))
    ((stringp value)
     value)
    ((vectorp value)
     (map 'vector #'json--encoding-value value))
    ((consp value)
     (mapcar #'json--encoding-value value))
    (t
     value)))

(-> json-encode (json-value) string)
(defun json-encode (value)
  "Encode VALUE as a compact JSON string."
  (with-output-to-string (stream)
    (yason:encode (json--encoding-value value) stream)))

(-> json-encode-utf8 (json-value) (vector (unsigned-byte 8)))
(defun json-encode-utf8 (value)
  "Encode VALUE directly as compact UTF-8 JSON octets."
  (let* ((octet-stream (make-in-memory-output-stream))
         (character-stream
           (make-flexi-stream octet-stream :external-format ':utf-8)))
    (unwind-protect
         (progn
           (yason:encode (json--encoding-value value) character-stream)
           (finish-output character-stream)
           (get-output-stream-sequence octet-stream))
      (close character-stream))))

(-> json-decode (string) json-value)
(defun json-decode (source)
  "Decode one JSON value from SOURCE without conflating false and null."
  (let ((yason:*parse-json-arrays-as-vectors* t)
        (yason:*parse-json-booleans-as-symbols* t)
        (yason:true t)
        (yason:false *json-decoded-false*))
    (yason:parse source)))

(-> json-object-source-p (t) boolean)
(defun json-object-source-p (source)
  "Return true when SOURCE contains exactly one JSON object and whitespace."
  (and
   (stringp source)
   (handler-case
        (with-input-from-string (stream source)
          (let ((yason:*parse-json-arrays-as-vectors* t)
                (yason:*parse-json-booleans-as-symbols* t)
                (yason:true t)
                (yason:false *json-decoded-false*))
            (let ((value (yason:parse stream)))
              (loop for character = (peek-char nil stream nil nil)
                    while (and character
                               (member character
                                       '(#\Space #\Tab #\Newline #\Return)))
                    do (read-char stream))
              (and (json-object-p value)
                   (null (peek-char nil stream nil nil))))))
     (error ()
       nil))))


;;;; -- Bounded Presentation --

(-> bounded-string
    (t &key (:limit integer) (:overflow-uri-function (option function)))
    string)
(defun bounded-string (value &key (limit 8000) overflow-uri-function)
  "Render VALUE as a string no longer than LIMIT characters.

OVERFLOW-URI-FUNCTION, when supplied, receives the complete text of an
oversized VALUE and may return a URI where that text stays readable;
the truncation notice then tells the reader where to continue instead
of discarding the tail."
  (let ((text (if (stringp value)
                  value
                  (write-to-string value
                                   :circle t
                                   :level 8
                                   :length 80
                                   :readably nil))))
    (if (<= (length text) limit)
        text
        (let ((uri (and overflow-uri-function
                        (funcall overflow-uri-function text))))
          (if uri
              (format nil "~A~%... ~:D characters omitted; read the complete result at ~A"
                      (subseq text 0 limit)
                      (- (length text) limit)
                      uri)
              (format nil "~A~%... ~:D characters omitted"
                      (subseq text 0 limit)
                      (- (length text) limit)))))))

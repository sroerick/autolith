(in-package #:autolith)

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

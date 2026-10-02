(in-package #:autolith)

;;;; -- URL Detection --

(defparameter *text-url-schemes* '("https://" "http://")
  "The URL prefixes recognized in transcript text, longest first.")

(defparameter *text-url-trailing-punctuation* ".,;:!?'\""
  "Characters that end a sentence rather than a URL when they trail one.")

(-> text-url--scheme-at (string integer) (option integer))
(defun text-url--scheme-at (text start)
  "Return the end of the URL scheme beginning at START in TEXT, or NIL."
  (loop for scheme in *text-url-schemes*
        for end = (+ start (length scheme))
        when (and (<= end (length text))
                  (string-equal scheme text :start2 start :end2 end))
          return end))

(-> text-url--boundary-p (character) boolean)
(defun text-url--boundary-p (character)
  "Return true when CHARACTER can never belong to a URL."
  (let ((code (char-code character)))
    (not (null (or (member character '(#\Space #\Tab #\Newline #\Return #\Page
                                       #\< #\> #\" #\` #\| #\{ #\})
                           :test #'char=)
                   (< code 32)
                   (member code '(#x7f #xa0 #x3000)))))))

(-> text-url--trim-end (string integer integer) integer)
(defun text-url--trim-end (text start end)
  "Return END moved back over trailing punctuation and unbalanced closers."
  (loop
    (when (<= end (1+ start))
      (return end))
    (let ((last (char text (1- end))))
      (cond
        ((find last *text-url-trailing-punctuation*)
         (decf end))
        ((and (char= last #\))
              (> (count #\) text :start start :end end)
                 (count #\( text :start start :end end)))
         (decf end))
        ((and (char= last #\])
              (> (count #\] text :start start :end end)
                 (count #\[ text :start start :end end)))
         (decf end))
        (t
         (return end))))))

(-> text-url-ranges (string) list)
(defun text-url-ranges (text)
  "Return the (START . END) character ranges of the web URLs in TEXT.

A URL begins with a recognized scheme and runs to whitespace or a bracketing
character, then sheds trailing sentence punctuation and closing brackets that
have no opening partner inside it. A scheme alone is not a URL."
  (let ((ranges nil)
        (index 0)
        (length (length text)))
    (loop
      (when (>= index length)
        (return (nreverse ranges)))
      (let ((scheme-end (text-url--scheme-at text index)))
        (if scheme-end
            (let ((end (or (position-if #'text-url--boundary-p text :start scheme-end)
                           length)))
              (setf end (text-url--trim-end text index end))
              (if (> end scheme-end)
                  (progn
                    (push (cons index end) ranges)
                    (setf index end))
                  (setf index scheme-end)))
            (incf index))))))

(-> text-url-at (string integer) (option string))
(defun text-url-at (text offset)
  "Return the URL in TEXT covering character OFFSET, or NIL."
  (loop for (start . end) in (text-url-ranges text)
        when (and (<= start offset) (< offset end))
          return (subseq text start end)))

(-> web-url-p (t) boolean)
(defun web-url-p (value)
  "Return true when VALUE is one complete http or https URL."
  (and (stringp value)
       (let ((ranges (text-url-ranges value)))
         (and (= (length ranges) 1)
              (zerop (car (first ranges)))
              (= (cdr (first ranges)) (length value))))))


;;;; -- OSC 8 Hyperlinks --

(-> text-url--linkable-p (string) boolean)
(defun text-url--linkable-p (url)
  "Return true when URL fits an OSC 8 payload, meaning printable ASCII only."
  (every (lambda (character)
           (<= 33 (char-code character) 126))
         url))

(-> terminal--hyperlink-open-sequence (string) string)
(defun terminal--hyperlink-open-sequence (url)
  "Return the OSC 8 control starting a hyperlink to URL."
  (format nil "~C]8;;~A~C\\" #\Escape url #\Escape))

(-> terminal--hyperlink-close-sequence () string)
(defun terminal--hyperlink-close-sequence ()
  "Return the OSC 8 control ending the current hyperlink."
  (format nil "~C]8;;~C\\" #\Escape #\Escape))

(-> terminal--hyperlinked-text (string) string)
(defun terminal--hyperlinked-text (text)
  "Return TEXT with every ASCII web URL wrapped in an OSC 8 hyperlink.

The visible characters are unchanged, so the result strips back to TEXT and
terminals that know OSC 8 offer the URL on hover or modifier-click."
  (let ((ranges (text-url-ranges text)))
    (if (null ranges)
        text
        (with-output-to-string (output)
          (let ((position 0))
            (loop for (start . end) in ranges
                  for url = (subseq text start end)
                  do (write-string text output :start position :end start)
                     (if (text-url--linkable-p url)
                         (progn
                           (write-string (terminal--hyperlink-open-sequence url) output)
                           (write-string url output)
                           (write-string (terminal--hyperlink-close-sequence) output))
                         (write-string url output))
                     (setf position end))
            (write-string text output :start position))))))

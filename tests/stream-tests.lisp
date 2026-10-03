(in-package #:autolith)

;;;; -- Bounded Character Read Tests --

(defclass test-short-character-input-stream
    (trivial-gray-streams:fundamental-character-input-stream)
  ((content
    :initarg :content
    :reader test-short-character-input-stream-content
    :documentation "The characters returned by the test stream.")
   (position
    :initform 0
    :accessor test-short-character-input-stream-position
    :documentation "The next unread character in CONTENT.")
   (maximum-read-size
    :initarg :maximum-read-size
    :reader test-short-character-input-stream-maximum-read-size
    :documentation "The largest short read returned before end of input.")
   (request-sizes
    :initform nil
    :accessor test-short-character-input-stream-request-sizes
    :documentation "The character counts requested through READ-SEQUENCE."))
  (:documentation "A character stream that records and shortens sequence reads."))

(defmethod trivial-gray-streams:stream-read-sequence
    ((stream test-short-character-input-stream) sequence start end &key)
  "Read at most the test stream's configured short-read size into SEQUENCE."
  (let* ((end       (or end (length sequence)))
         (request   (- end start))
         (position  (test-short-character-input-stream-position stream))
         (available (- (length (test-short-character-input-stream-content stream))
                       position))
         (count     (min request
                         available
                         (test-short-character-input-stream-maximum-read-size
                          stream))))
    (push request (test-short-character-input-stream-request-sizes stream))
    (replace sequence
             (test-short-character-input-stream-content stream)
             :start1 start
             :end1   (+ start count)
             :start2 position
             :end2   (+ position count))
    (incf (test-short-character-input-stream-position stream) count)
    (+ start count)))

(-> test-bounded-character-reads () null)
(defun test-bounded-character-reads ()
  "Exercise bounded character reads across full, short, and empty inputs."
  (let* ((*character-read-sequence-window* 3)
         (content "příliš žluťoučký kůň")
         (buffer (make-string (length content))))
    (with-input-from-string (stream content)
      (test-assert
       (= (read-character-sequence buffer stream) (length content))
       "bounded character reads fill an exact-sized buffer")
      (test-assert
       (string= buffer content)
       "bounded character reads preserve multibyte character content")))
  (let* ((*character-read-sequence-window* 3)
         (content "žluť")
         (stream (make-instance 'test-short-character-input-stream
                                :content content
                                :maximum-read-size 2))
         (buffer (make-string 8 :initial-element #\?)))
    (test-assert
     (= (read-character-sequence buffer stream) (length content))
     "bounded character reads continue after short reads")
    (test-assert
     (string= (subseq buffer 0 (length content)) content)
     "short bounded character reads preserve their returned prefix")
    (test-assert
     (every (lambda (character) (char= character #\?))
            (subseq buffer (length content)))
     "short bounded character reads leave the unused buffer tail untouched")
    (test-assert
     (every (lambda (size) (<= size *character-read-sequence-window*))
            (test-short-character-input-stream-request-sizes stream))
     "bounded character reads never request more than their configured window")
    (test-assert
     (zerop (read-character-sequence buffer stream))
     "bounded character reads return zero at end of input"))
  nil)


;;;; -- Bounded Output Capture Tests --

(-> test-bounded-output-stream () null)
(defun test-bounded-output-stream ()
  "Test that bounded output keeps a prefix, reports truncation, and tracks columns."
  (let ((stream (bounded-output-stream-create 8)))
    (format stream "abc~%de")
    (test-assert (and (string= (bounded-output-stream-text stream)
                               (format nil "abc~%de"))
                      (not (bounded-output-stream-truncated-p stream)))
                 "bounded output retains text within its capacity")
    (test-assert (= (trivial-gray-streams:stream-line-column stream) 2)
                 "bounded output tracks the column after a newline")
    (write-string "fghij" stream)
    (format stream "~&tail")
    (test-assert (and (string= (bounded-output-stream-text stream)
                               (format nil "abc~%defg"))
                      (bounded-output-stream-truncated-p stream)
                      (= (bounded-output-stream-capacity stream) 8))
                 "bounded output keeps only the prefix and reports truncation")
    (test-assert (= (trivial-gray-streams:stream-line-column stream) 4)
                 "bounded output keeps following columns past its capacity"))
  (let ((stream (bounded-output-stream-create 0)))
    (write-char #\x stream)
    (test-assert (and (string= (bounded-output-stream-text stream) "")
                      (bounded-output-stream-truncated-p stream))
                 "a zero-capacity stream retains nothing and reports truncation"))
  nil)

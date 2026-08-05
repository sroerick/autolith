(in-package #:autolith)

;;;; -- Enhanced Terminal Input --

(defparameter *terminal-escape-delay-seconds* 0.002
  "The seconds allowed for bytes following one terminal Escape character.")

(defparameter *terminal-unbracketed-paste-coalesce-seconds* 0.002
  "The short delay used to collect one unbracketed terminal input burst.")

(defparameter *terminal-control-v-paste-idle-seconds* 0.05
  "The idle gap ending one unbracketed Ctrl-V paste burst.")

(defparameter *terminal-unbracketed-paste-maximum-characters* 1000000
  "The maximum characters retained from one unbracketed paste burst.")

(-> terminal--character-after-escape (stream real) (option character))
(defun terminal--character-after-escape (stream escape-delay)
  "Return the next STREAM character after Escape, allowing ESCAPE-DELAY seconds."
  (or (read-char-no-hang stream nil nil)
      (progn
        (when (plusp escape-delay)
          (sleep escape-delay))
        (read-char-no-hang stream nil nil))))

(-> terminal--read-input-burst (stream) (option string))
(defun terminal--read-input-burst (stream)
  "Return one bounded burst currently available from STREAM, or NIL at EOF."
  (let ((first (read-char stream nil nil)))
    (unless first
      (return-from terminal--read-input-burst nil))
    (when (plusp *terminal-unbracketed-paste-coalesce-seconds*)
      (sleep *terminal-unbracketed-paste-coalesce-seconds*))
    (let ((characters (list first))
          (count 1))
      (loop while (< count *terminal-unbracketed-paste-maximum-characters*)
            for character = (read-char-no-hang stream nil nil)
            while character
            do (push character characters)
               (incf count))
      (coerce (nreverse characters) 'string))))

(-> terminal--multiline-paste-burst-p (string) boolean)
(defun terminal--multiline-paste-burst-p (text)
  "Return true when TEXT is one unbracketed burst containing a line break."
  (and (> (length text) 1)
       (not (char= (char text 0) *terminal-escape-character*))
       (not (char= (char text 0) (code-char 22)))
       (or (find #\Newline text)
           (find #\Return text))
       t))

(-> terminal--read-control-v-paste (stream) t)
(defun terminal--read-control-v-paste (stream)
  "Return one bounded paste event from bytes following literal Ctrl-V."
  (let ((characters nil)
        (count 0))
    (loop while (< count *terminal-unbracketed-paste-maximum-characters*)
          for character = (or (read-char-no-hang stream nil nil)
                              (progn
                                (sleep *terminal-control-v-paste-idle-seconds*)
                                (read-char-no-hang stream nil nil)))
          while character
          do (push character characters)
             (incf count))
    (if characters
        (list ':paste (coerce (nreverse characters) 'string))
        ':ignore)))

(-> terminal--csi-final-character-p (character) boolean)
(defun terminal--csi-final-character-p (character)
  "Return true when CHARACTER terminates one terminal CSI sequence."
  (let ((code (char-code character)))
    (and (<= #x40 code) (<= code #x7e))))

(-> terminal--read-csi-body (stream) string)
(defun terminal--read-csi-body (stream)
  "Read one complete CSI body from STREAM, including its final character."
  (with-output-to-string (body)
    (loop for character = (read-char stream nil nil)
          while character
          do (write-char character body)
          when (terminal--csi-final-character-p character)
            do (return))))

(-> terminal--kitty-key-event (string) (values boolean t))
(defun terminal--kitty-key-event (body)
  "Return a semantic event for one supported Kitty CSI-u BODY."
  (cond
    ((member body '("10u" "13u") :test #'string=)
     (values t ':submit))
    ((member body '("10;2u" "10;3u" "10;4u" "10;5u"
                    "10;6u" "10;7u" "10;8u"
                    "13;2u" "13;3u" "13;4u" "13;5u"
                    "13;6u" "13;7u" "13;8u")
             :test #'string=)
     (values t ':insert-newline))
    ((string= body "9u")
     (values t ':complete))
    ((string= body "9;2u")
     (values t ':complete-previous))
    ((string= body "27u")
     (values t ':escape))
    ((string= body "127u")
     (values t ':backspace))
    (t
     (values nil nil))))

(-> terminal--read-prefixed-event (string stream) t)
(defun terminal--read-prefixed-event (prefix stream)
  "Decode PREFIX followed by STREAM through Clinedi's public event reader."
  (read-event
   :stream (make-concatenated-stream
            (make-string-input-stream prefix)
            stream)
   :escape-delay 0))

(-> terminal--read-escape-event (stream real) t)
(defun terminal--read-escape-event (stream escape-delay)
  "Read one raw or enhanced Escape event from STREAM."
  (let ((next (terminal--character-after-escape stream escape-delay)))
    (cond
      ((null next)
       ':escape)
      ((char= next #\[)
       (let* ((body (terminal--read-csi-body stream))
              (prefix (format nil "~C[~A" *terminal-escape-character* body)))
         (multiple-value-bind (recognized-p event)
             (terminal--kitty-key-event body)
           (if recognized-p
               event
               (terminal--read-prefixed-event prefix stream)))))
      (t
       (terminal--read-prefixed-event
        (format nil "~C~C" *terminal-escape-character* next)
        stream)))))

(-> terminal--decode-editing-event
    (stream &key (:escape-delay real))
    t)
(defun terminal--decode-editing-event
    (stream &key (escape-delay *terminal-escape-delay-seconds*))
  "Decode one Clinedi event while accepting bare Kitty CSI-u control reports."
  (let ((character (read-char stream nil nil)))
    (cond
      ((null character)
       ':stream-end)
      ((char= character *terminal-escape-character*)
       (terminal--read-escape-event stream escape-delay))
      ((char= character (code-char 22))
       (terminal--read-control-v-paste stream))
      (t
       (unread-char character stream)
       (read-event :stream stream :escape-delay escape-delay)))))

(-> terminal--decode-buffered-editing-event
    (stream-terminal stream &key (:escape-delay real))
    t)
(defun terminal--decode-buffered-editing-event
    (terminal prefix-stream
     &key (escape-delay *terminal-escape-delay-seconds*))
  "Decode one event from PREFIX-STREAM followed by TERMINAL's raw input."
  (let ((event
          (terminal--decode-editing-event
           (make-concatenated-stream
            prefix-stream
            (stream-terminal-input-stream terminal))
           :escape-delay escape-delay)))
    (setf (stream-terminal-pending-input-stream terminal)
          (and (listen prefix-stream) prefix-stream))
    event))

(-> terminal-read-editing-event
    (stream-terminal &key (:escape-delay real))
    t)
(defun terminal-read-editing-event
    (terminal &key (escape-delay *terminal-escape-delay-seconds*))
  "Read one semantic event, preserving unbracketed multiline paste bursts."
  (let ((pending (stream-terminal-pending-input-stream terminal)))
    (when (and pending (not (listen pending)))
      (setf (stream-terminal-pending-input-stream terminal) nil
            pending nil))
    (if pending
        (terminal--decode-buffered-editing-event
         terminal pending :escape-delay escape-delay)
        (let ((burst
                (terminal--read-input-burst
                 (stream-terminal-input-stream terminal))))
          (cond
            ((null burst)
             ':stream-end)
            ((terminal--multiline-paste-burst-p burst)
             (list ':paste (sanitize-text burst)))
            (t
             (terminal--decode-buffered-editing-event
              terminal
              (make-string-input-stream burst)
              :escape-delay escape-delay)))))))

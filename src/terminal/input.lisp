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
  "Return true when TEXT is one unbracketed burst containing a line break.

This classification takes precedence over controls later in the burst so pasted
text cannot invoke editing commands."
  (and (> (length text) 1)
       (not (char= (char text 0) *terminal-escape-character*))
       (not (char= (char text 0) (code-char 22)))
       (or (find #\Newline text)
           (find #\Return text))
       t))

(-> terminal--plain-text-burst-p (string) boolean)
(defun terminal--plain-text-burst-p (text)
  "Return true when TEXT is one multi-character burst without terminal controls."
  (and (> (length text) 1)
       (every (lambda (character)
                (let ((code (char-code character)))
                  (not (or (< code 32)
                           (<= 127 code 159)))))
              text)
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

(-> terminal--decode-editing-event
    (stream &key (:escape-delay real))
    t)
(defun terminal--decode-editing-event
    (stream &key (escape-delay *terminal-escape-delay-seconds*))
  "Decode one Clinedi event while retaining Autolith's literal Ctrl-V policy."
  (let ((character (read-char stream nil nil)))
    (cond
      ((null character)
       ':stream-end)
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
  "Read one event, batching plain input and preserving multiline paste bursts."
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
            ((terminal--plain-text-burst-p burst)
             (list ':insert burst))
            (t
             (terminal--decode-buffered-editing-event
              terminal
              (make-string-input-stream burst)
              :escape-delay escape-delay)))))))

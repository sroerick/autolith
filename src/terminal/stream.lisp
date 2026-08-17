(in-package #:autolith)

;;;; -- Terminal Methods --

(defparameter *terminal-window-size-request*
  #+linux #x5413
  #+bsd #x40087468
  #-(or linux bsd) #x5413
  "The platform TIOCGWINSZ ioctl request for reading terminal dimensions.")

#-(or linux bsd)
(warn "Autolith has no validated TIOCGWINSZ value for this platform; terminal size will fall back to tput.")

(-> terminal-file-descriptor-size
    (integer)
    (values (option integer) (option integer)))
(defun terminal-file-descriptor-size (file-descriptor)
  "Return positive terminal rows and columns for FILE-DESCRIPTOR, or NIL values."
  (handler-case
      (sb-alien:with-alien ((size (array sb-alien:unsigned-short 4)))
        (sb-posix:ioctl file-descriptor
                        *terminal-window-size-request*
                        (sb-alien:addr (sb-alien:deref size 0)))
        (let ((rows (sb-alien:deref size 0))
              (columns (sb-alien:deref size 1)))
          (values (and (plusp rows) rows)
                  (and (plusp columns) columns))))
    (sb-posix:syscall-error ()
      (values nil nil))))

(defmethod terminal--write ((terminal stream-terminal) (text string))
  "Write trusted TEXT to TERMINAL's output stream."
  (write-string text (stream-terminal-output-stream terminal))
  nil)

(defmethod terminal-flush ((terminal stream-terminal))
  "Flush TERMINAL's output stream."
  (finish-output (stream-terminal-output-stream terminal))
  nil)

(-> terminal--disable-input-protocols (stream-terminal) null)
(defun terminal--disable-input-protocols (terminal)
  "Best-effort restore ordinary keyboard reporting and paste handling."
  (let ((failure nil))
    (labels ((attempt (function)
               "Run FUNCTION, retaining only the first signaled failure."
               (handler-case
                   (funcall function)
                 (error (condition)
                   (unless failure
                     (setf failure condition))))))
      (attempt
       (lambda ()
         (terminal--write terminal
                          (terminal-bracketed-paste-disable-sequence))))
      (attempt
       (lambda ()
         (terminal--write terminal
                          (terminal-keyboard-enhancement-disable-sequence))))
      (attempt (lambda () (terminal-flush terminal))))
    (when failure
      (error failure)))
  nil)

(-> terminal--enable-input-protocols (stream-terminal) null)
(defun terminal--enable-input-protocols (terminal)
  "Enable modified keys and bracketed paste, rolling back partial output."
  (handler-case
      (progn
        (terminal--write terminal
                         (terminal-keyboard-enhancement-enable-sequence))
        (terminal--write terminal (terminal-bracketed-paste-enable-sequence))
        (terminal-flush terminal))
    (error (condition)
      (ignore-errors (terminal--disable-input-protocols terminal))
      (error condition)))
  nil)

(defmethod terminal-input-ready-p ((terminal stream-terminal))
  "Return true when TERMINAL input can be consumed without blocking."
  (let ((pending (stream-terminal-pending-input-stream terminal)))
    (when (and pending (not (listen pending)))
      (setf (stream-terminal-pending-input-stream terminal) nil
            pending nil))
    (not
     (null
      (or (not (terminal-interactive-p terminal))
          pending
          (listen (stream-terminal-input-stream terminal)))))))

(-> terminal--interactive-file-descriptor-p (integer) boolean)
(defun terminal--interactive-file-descriptor-p (file-descriptor)
  "Return true when FILE-DESCRIPTOR names an interactive terminal."
  (and (not (minusp file-descriptor))
       (let ((result (sb-unix:unix-isatty file-descriptor)))
         (and result (plusp result)))))

(-> terminal--terminal-mode-or-nil (stream-terminal) t)
(defun terminal--terminal-mode-or-nil (terminal)
  "Return TERMINAL's termios value, or NIL when its input is not interactive."
  (let ((file-descriptor (stream-terminal-input-file-descriptor terminal)))
    (unless (terminal--interactive-file-descriptor-p file-descriptor)
      (return-from terminal--terminal-mode-or-nil nil))
    (handler-case
        (sb-posix:tcgetattr file-descriptor)
      (sb-posix:syscall-error (condition)
        (error 'terminal-error
               :message "Could not inspect terminal input mode."
               :operation ':start
               :cause condition)))))

(-> terminal--configure-input-mode (sb-posix:termios) sb-posix:termios)
(defun terminal--configure-input-mode (mode)
  "Configure MODE for noncanonical, no-echo, application-managed input."
  (setf (sb-posix:termios-lflag mode)
        (logandc2 (sb-posix:termios-lflag mode)
                  (logior sb-posix:icanon
                          sb-posix:echo
                          sb-posix:isig
                          sb-posix:iexten))
        (sb-posix:termios-iflag mode)
        (logandc2 (sb-posix:termios-iflag mode) sb-posix:ixon))
  (let ((control-characters (sb-posix:termios-cc mode)))
    (setf (aref control-characters sb-posix:vmin) 1
          (aref control-characters sb-posix:vtime) 0))
  mode)

(defmethod terminal-start ((terminal stream-terminal))
  "Start TERMINAL in noncanonical mode, or select its non-TTY fallback."
  (when (terminal-started-p terminal)
    (return-from terminal-start terminal))
  (let ((saved-mode (terminal--terminal-mode-or-nil terminal)))
    (if (null saved-mode)
        (setf (terminal-interactive-p terminal) nil
              (terminal-styled-p terminal) nil
              (terminal-started-p terminal) t)
        (handler-case
            (let ((active-mode
                    (terminal--configure-input-mode
                     (sb-posix:tcgetattr
                      (stream-terminal-input-file-descriptor terminal)))))
              (sb-posix:tcsetattr
               (stream-terminal-input-file-descriptor terminal)
               sb-posix:tcsanow
               active-mode)
              (setf (stream-terminal-saved-terminal-mode terminal) saved-mode
                    (terminal-interactive-p terminal) t
                    (terminal-styled-p terminal) (terminal-environment-styling-p)
                    (terminal-started-p terminal) t)
              (terminal--enable-input-protocols terminal))
          (error (condition)
            (ignore-errors
              (sb-posix:tcsetattr
               (stream-terminal-input-file-descriptor terminal)
               sb-posix:tcsanow
               saved-mode))
            (setf (stream-terminal-saved-terminal-mode terminal) nil
                  (terminal-interactive-p terminal) nil
                  (terminal-started-p terminal) nil)
            (error 'terminal-error
                   :message "Could not enter terminal input mode."
                   :operation ':start
                   :cause condition)))))
  terminal)

(defmethod terminal-stop ((terminal stream-terminal))
  "Stop TERMINAL and restore the exact termios value captured at startup."
  (unless (terminal-started-p terminal)
    (return-from terminal-stop terminal))
  (let ((failure nil)
        (saved-mode (stream-terminal-saved-terminal-mode terminal)))
    (when (terminal-interactive-p terminal)
      (handler-case
          (progn
            (terminal--disable-input-protocols terminal))
        (error (condition)
          (setf failure condition)))
      (when saved-mode
        (handler-case
            (sb-posix:tcsetattr
             (stream-terminal-input-file-descriptor terminal)
             sb-posix:tcsanow
             saved-mode)
          (error (condition)
            (unless failure
              (setf failure condition))))))
    (setf (stream-terminal-saved-terminal-mode terminal) nil
          (terminal-interactive-p terminal) nil
          (terminal-styled-p terminal) nil
          (terminal-started-p terminal) nil)
    (when failure
      (error 'terminal-error
             :message "Could not completely restore terminal state."
             :operation ':stop
             :cause failure)))
  terminal)

(defmethod terminal-read-event ((terminal stream-terminal))
  "Read one key, escape sequence, paste, fallback line, or physical stream end."
  (if (terminal-interactive-p terminal)
      (terminal-read-editing-event terminal)
      (let ((line (read-line (stream-terminal-input-stream terminal) nil nil)))
        (if line
            (list :line line)
            :stream-end))))

;; Generic functions require broad FTYPEs so downstream terminal adapters can
;; add methods without SBCL replacing a class-restricted proclamation.
(-> terminal-start (t) *)
(-> terminal-stop (t) *)
(-> terminal-read-event (t) *)
(-> terminal--write (t t) *)
(-> terminal-flush (t) *)


;;;; -- Public Construction --

(-> stream-terminal-create
    (&key
     (:input-stream stream)
     (:output-stream stream)
     (:input-file-descriptor integer)
     (:rows integer)
     (:columns integer))
    stream-terminal)
(defun stream-terminal-create
    (&key
       (input-stream *standard-input*)
       (output-stream *standard-output*)
       (input-file-descriptor 0)
       (rows *terminal-default-rows*)
       (columns *terminal-default-columns*))
  "Create a stream terminal using INPUT-STREAM, OUTPUT-STREAM, and a POSIX descriptor."
  (make-instance 'stream-terminal
                 :input-stream input-stream
                 :output-stream output-stream
                 :input-file-descriptor input-file-descriptor
                 :rows (if (plusp rows)
                           rows
                           *terminal-default-rows*)
                 :columns (if (plusp columns)
                              columns
                              *terminal-default-columns*)))

(in-package #:autolith)

;;;; -- Localgroup Wire Protocol --

(defparameter *localgroup-protocol-version* 1
  "The local loopback control protocol version.")

(defparameter *localgroup-packet-character-limit* (* 8 1024 1024)
  "The maximum decoded characters accepted in one localgroup packet.")

(defparameter *localgroup-frame-header-character-limit* 16
  "The maximum decimal length-header characters accepted for one packet.")

(defparameter *localgroup-connect-timeout-seconds* 2
  "The maximum seconds allowed for a localgroup connect or required response.")

(defvar *localgroup-startup-record* nil
  "The validated detached-process handoff record active during startup.")

(-> localgroup-startup-detached-p () boolean)
(defun localgroup-startup-detached-p ()
  "Return true while startup is reconstructing a detached localgroup process."
  (not (null *localgroup-startup-record*)))

;; These runtime functions load after the responsive input implementation.
(-> application-localgroup-paused-p (t) boolean)
(-> application-localgroup-resume (t) boolean)
(-> application-localgroup-request-handoff (t keyword) list)
(-> application-localgroup-handoff-pending-p (t) boolean)
(-> application-localgroup-take-ready-handoff (t) (option keyword))
(-> application-localgroup-run-handoff (t keyword t) null)
(-> localgroup-handoff-assert-startup-active () null)
(-> localgroup-handoff-finish-startup (t) null)

(define-condition localgroup-error (autolith-error)
  ((operation
    :initarg :operation
    :reader localgroup-error-operation
    :type keyword
    :documentation "The localgroup operation that failed.")
   (session-id
    :initarg :session-id
    :initform nil
    :reader localgroup-error-session-id
    :type (option string)
    :documentation "The requested localgroup session identifier, when known.")
   (cause
    :initarg :cause
    :initform nil
    :reader localgroup-error-cause
    :type t
    :documentation "The underlying failure, when one exists."))
  (:documentation "A local session discovery, transport, or control failure."))

(-> localgroup--proper-list-p (t) boolean)
(defun localgroup--proper-list-p (value)
  "Return true when VALUE is a finite proper list."
  (not
   (null
    (handler-case
        (let ((length (list-length value)))
          (and length t))
      (type-error () nil)))))

(-> localgroup--hex-string ((simple-array (unsigned-byte 8) (*))) string)
(defun localgroup--hex-string (bytes)
  "Return lowercase hexadecimal text for BYTES."
  (with-output-to-string (stream)
    (loop for byte across bytes
          do (format stream "~2,'0x" byte))))

(-> localgroup-random-token () string)
(defun localgroup-random-token ()
  "Return a fresh unguessable capability token."
  (localgroup--hex-string (random-data 32)))

(-> localgroup-random-session-id () string)
(defun localgroup-random-session-id ()
  "Return a fresh hexadecimal nonce for private handoff pathnames."
  (subseq (localgroup--hex-string (random-data 8)) 0 12))

(-> localgroup--legacy-session-identifier-p (t) boolean)
(defun localgroup--legacy-session-identifier-p (value)
  "Return true when VALUE is one former twelve-character hexadecimal session ID."
  (not
   (null
    (and (stringp value)
         (= (length value) 12)
         (every (lambda (character) (digit-char-p character 16)) value)))))

(-> localgroup-session-identifier-normalize (t) string)
(defun localgroup-session-identifier-normalize (value)
  "Return VALUE's canonical localgroup session identifier.

Canonical idsmall identifiers accept their visual hyphen. Former hexadecimal
session identifiers remain accepted for discovery and detached handoff."
  (handler-case
      (identifier-normalize value)
    (identifier-error ()
      (if (localgroup--legacy-session-identifier-p value)
          (string-downcase value)
          (error 'localgroup-error
                 :message
                 "A localgroup session identifier must be a seven-character Bitcoin Base58 identifier, with an optional hyphen after the first, or a legacy twelve-character hexadecimal identifier."
                 :operation ':arguments
                 :session-id (and (stringp value) value))))))

(-> localgroup-session-identifier-display (string) string)
(defun localgroup-session-identifier-display (identifier)
  "Return IDENTIFIER in readable canonical form, retaining legacy IDs plainly."
  (handler-case
      (identifier-display identifier)
    (identifier-error ()
      (if (localgroup--legacy-session-identifier-p identifier)
          (string-downcase identifier)
          identifier))))

(-> localgroup-session-identifier-timestamp (string) (option timestamp))
(defun localgroup-session-identifier-timestamp (identifier)
  "Return the timestamp encoded by canonical IDENTIFIER, or NIL for legacy IDs."
  (handler-case
      (idsmall:identifier-timestamp identifier)
    (identifier-error () nil)))

(-> localgroup--packet-payload (list) string)
(defun localgroup--packet-payload (packet)
  "Return PACKET as readable text without reader evaluation syntax."
  (with-standard-io-syntax
    (let ((*print-circle* nil)
          (*print-readably* t)
          (*print-pretty* nil))
      (with-output-to-string (stream)
        (write packet :stream stream)))))

(-> localgroup--packet-string (list) string)
(defun localgroup--packet-string (packet)
  "Return PACKET as one bounded decimal-length-prefixed wire frame."
  (let ((payload (localgroup--packet-payload packet)))
    (when (> (length payload) *localgroup-packet-character-limit*)
      (error 'localgroup-error
             :message "A localgroup packet exceeded the configured limit."
             :operation ':write-packet))
    (format nil "~D~%~A" (length payload) payload)))

(-> localgroup-write-packet (stream list) null)
(defun localgroup-write-packet (stream packet)
  "Write and flush one bounded localgroup PACKET frame to STREAM."
  (write-string (localgroup--packet-string packet) stream)
  (finish-output stream)
  nil)

(-> localgroup--read-frame-header (stream) (option string))
(defun localgroup--read-frame-header (stream)
  "Read one bounded decimal frame header, returning NIL only at clean end of file."
  (let ((characters (make-array 16
                                :element-type 'character
                                :adjustable t
                                :fill-pointer 0)))
    (loop for character = (read-char stream nil nil)
          do (cond
               ((null character)
                (if (zerop (length characters))
                    (return nil)
                    (error 'localgroup-error
                           :message "A localgroup frame header ended early."
                           :operation ':read-packet)))
               ((char= character #\Newline)
                (return (coerce characters 'string)))
               ((>= (length characters)
                    *localgroup-frame-header-character-limit*)
                (error 'localgroup-error
                       :message "A localgroup frame header exceeded the configured limit."
                       :operation ':read-packet))
               (t
                (vector-push-extend character characters))))))

(-> localgroup--read-frame-length (stream) (option integer))
(defun localgroup--read-frame-length (stream)
  "Read and validate one packet character count from STREAM."
  (let ((header (localgroup--read-frame-header stream)))
    (unless header
      (return-from localgroup--read-frame-length nil))
    (unless (and (plusp (length header))
                 (every #'digit-char-p header))
      (error 'localgroup-error
             :message "A localgroup frame header is malformed."
             :operation ':read-packet))
    (let ((length (parse-integer header)))
      (unless (<= 1 length *localgroup-packet-character-limit*)
        (error 'localgroup-error
               :message "A localgroup packet exceeded the configured limit."
               :operation ':read-packet))
      length)))

(-> localgroup--read-frame-payload (stream integer) string)
(defun localgroup--read-frame-payload (stream length)
  "Read exactly LENGTH decoded packet characters from STREAM."
  (let ((payload (make-string length)))
    (loop for index below length
          for character = (read-char stream nil nil)
          do (unless character
               (error 'localgroup-error
                      :message "A localgroup packet ended before its declared length."
                      :operation ':read-packet))
             (setf (char payload index) character))
    payload))

(-> localgroup-read-packet (stream) (option list))
(defun localgroup-read-packet (stream)
  "Read one safe, proper-list localgroup packet frame from STREAM."
  (let ((length (localgroup--read-frame-length stream)))
    (unless length
      (return-from localgroup-read-packet nil))
    (let ((payload (localgroup--read-frame-payload stream length)))
      (handler-case
          (with-standard-io-syntax
            (let ((*read-eval* nil)
                  (*readtable* (copy-readtable nil)))
              (multiple-value-bind (packet position)
                  (read-from-string payload nil nil)
                (unless (and packet
                             (localgroup--proper-list-p packet)
                             (every (lambda (character)
                                      (find character '(#\Space #\Tab #\Return
                                                        #\Newline)))
                                    (subseq payload position)))
                  (error 'localgroup-error
                         :message "A localgroup packet is malformed."
                         :operation ':read-packet))
                packet)))
        (localgroup-error (condition)
          (error condition))
        (error (condition)
          (error 'localgroup-error
                 :message "A localgroup packet could not be read safely."
                 :operation ':read-packet
                 :cause condition))))))

(-> localgroup-read-response (stream keyword) list)
(defun localgroup-read-response (stream operation)
  "Read one required response within the localgroup transport deadline."
  (handler-case
      (sb-sys:with-deadline (:seconds *localgroup-connect-timeout-seconds*)
        (or (localgroup-read-packet stream)
            (error 'localgroup-error
                   :message "The localgroup endpoint closed without a response."
                   :operation operation)))
    (sb-sys:deadline-timeout (condition)
      (error 'localgroup-error
             :message "The localgroup endpoint did not respond in time."
             :operation operation
             :cause condition))))

(-> localgroup--socket-stream (sb-bsd-sockets:socket) stream)
(defun localgroup--socket-stream (socket)
  "Return a buffered UTF-8 character stream for SOCKET."
  (sb-bsd-sockets:socket-make-stream
   socket
   :input t
   :output t
   :element-type 'character
   :external-format ':utf-8
   :buffering ':full))

(-> localgroup-connect (integer) (values sb-bsd-sockets:socket stream))
(defun localgroup-connect (port)
  "Connect to the loopback localgroup endpoint at PORT."
  (let ((socket (make-instance 'sb-bsd-sockets:inet-socket
                               :type ':stream
                               :protocol ':tcp))
        (stream nil))
    (handler-case
        (progn
          (setf (sb-bsd-sockets:sockopt-tcp-nodelay socket) t)
          (sb-sys:with-deadline (:seconds *localgroup-connect-timeout-seconds*)
            (sb-bsd-sockets:socket-connect
             socket
             (sb-bsd-sockets:make-inet-address "127.0.0.1")
             port))
          (setf stream (localgroup--socket-stream socket))
          (values socket stream))
      (error (condition)
        (if stream
            (ignore-errors (close stream))
            (ignore-errors (sb-bsd-sockets:socket-close socket)))
        (error 'localgroup-error
               :message (format nil "Could not connect to localgroup port ~D." port)
               :operation ':connect
               :cause condition)))))

(-> localgroup-call
    (integer string keyword &optional list)
    list)
(defun localgroup-call (port token operation &optional arguments)
  "Perform one authenticated localgroup OPERATION and return its response."
  (multiple-value-bind (socket stream)
      (localgroup-connect port)
    (declare (ignore socket))
    (unwind-protect
         (progn
           (localgroup-write-packet
            stream
            (list :localgroup-request
                  :version *localgroup-protocol-version*
                  :token token
                  :operation operation
                  :arguments arguments))
           (localgroup-read-response stream operation))
      (ignore-errors (close stream)))))

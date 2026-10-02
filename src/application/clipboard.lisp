(in-package #:autolith)

;;;; -- Transcript Actions --

(defparameter *application-transcript-action-notice-seconds* 4
  "How long clipboard and browser notices stay on the status row.")

(defparameter *application-opened-url-notice-cells* 60
  "The longest URL shown whole in an opening notice before it is abbreviated.")

(-> application--transcript-action-notice (application string) null)
(defun application--transcript-action-notice (application text)
  "Show TEXT transiently on APPLICATION's status row."
  (terminal-ui-set-notice (application-ui application)
                          text
                          :duration-seconds
                          *application-transcript-action-notice-seconds*)
  nil)

(-> application--host-clipboard-copy (string) (values boolean (option string)))
(defun application--host-clipboard-copy (text)
  "Place TEXT on the host clipboard, returning success and any failure reason."
  (handler-case
      (progn
        (setf (clipboard-text) text)
        (values t nil))
    (sophisticated-clipboard-error (condition)
      (values nil (princ-to-string condition)))))

(-> application-copy-text (application string) boolean)
(defun application-copy-text (application text)
  "Copy TEXT for the user and report how, returning whether any route accepted it.

The host clipboard is the certain route and is tried first. The terminal
also receives an OSC 52 request, which is what reaches the user's own
machine over SSH or a localgroup relay; a terminal that ignores it loses
nothing. A notice names the route that worked, or the host failure."
  (let ((lines (1+ (count #\Newline (string-right-trim '(#\Newline) text)))))
    (multiple-value-bind (host-p reason)
        (application--host-clipboard-copy text)
      (let ((terminal-p (terminal-ui-copy-to-terminal (application-ui application) text)))
        (application--transcript-action-notice
         application
         (cond
           (host-p
            (format nil "Copied ~D line~:P to the clipboard." lines))
           (terminal-p
            (format nil "Sent ~D line~:P to the terminal's clipboard; ~A" lines reason))
           (t
            (format nil "Nothing copied: ~A" reason))))
        (or host-p terminal-p)))))

(-> application-open-url (application string) boolean)
(defun application-open-url (application url)
  "Open web URL in the default browser and report the outcome in a notice."
  (cond
    ((not (web-url-p url))
     (application--transcript-action-notice
      application "Only http and https links open from the transcript.")
     nil)
    ((platform-open-url *platform* url)
     (application--transcript-action-notice
      application
      (format nil "Opening ~A"
              (text-cell-prefix url *application-opened-url-notice-cells*)))
     t)
    (t
     (application--transcript-action-notice
      application "No browser launcher is available on this host.")
     nil)))

(-> application-transcript-action (application list) null)
(defun application-transcript-action (application action)
  "Perform clicked transcript ACTION, either (:copy TEXT) or (:open-url URL)."
  (case (first action)
    (:copy
     (application-copy-text application (second action)))
    (:open-url
     (application-open-url application (second action))))
  nil)

(-> application-connect-transcript-actions (application) null)
(defun application-connect-transcript-actions (application)
  "Route clicks on APPLICATION's transcript widgets and links to its actions."
  (let ((ui (and (slot-boundp application 'ui)
                 (application-ui application))))
    (when (typep ui 'terminal-ui)
      (setf (terminal-ui-action-function ui)
            (lambda (action)
              (application-transcript-action application action)))))
  nil)

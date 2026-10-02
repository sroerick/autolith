(in-package #:autolith)

;;;; -- Terminal Clipboard (OSC 52) --

(-> terminal-clipboard-sequence (string) string)
(defun terminal-clipboard-sequence (text)
  "Return the OSC 52 control asking the terminal to place TEXT on its clipboard.

The payload is the UTF-8 encoding of TEXT in base64, addressed to the
clipboard selection. Terminals that honor OSC 52, including those reached
over SSH or through a localgroup relay, copy on the machine the user sits at."
  (format nil "~C]52;c;~A~C\\"
          #\Escape
          (usb8-array-to-base64-string
           (sb-ext:string-to-octets text :external-format ':utf-8))
          #\Escape))

(-> terminal-ui-copy-to-terminal (terminal-ui string) boolean)
(defun terminal-ui-copy-to-terminal (ui text)
  "Write the OSC 52 copy request for TEXT to UI's terminal, reporting whether it was sent.

Only an interactive styled terminal receives the control; piped or plain
output never carries clipboard requests."
  (let ((terminal (terminal-ui-terminal ui)))
    (if (and (terminal-interactive-p terminal)
             (terminal-styled-p terminal))
        (with-terminal-ui-locked (ui)
          (terminal--write terminal (terminal-clipboard-sequence text))
          (terminal-flush terminal)
          t)
        nil)))

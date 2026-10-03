(in-package #:autolith)

;;;; -- Hyperlinked Transcript Rendering --

(-> test-hyperlinked-rendering () null)
(defun test-hyperlinked-rendering ()
  "Test OSC 8 wrapping of URLs in styled output and its absence elsewhere."
  (let* ((styled (make-instance 'recording-terminal :columns 40 :styled-p t))
         (plain-terminal (make-instance 'recording-terminal :columns 40))
         (spans (list (terminal-span ':plain "go to https://example.com/x now")))
         (plain (terminal--spans-text spans))
         (display (terminal--render-spans styled spans))
         (open (format nil "~C]8;;https://example.com/x~C\\" #\Escape #\Escape))
         (close (format nil "~C]8;;~C\\" #\Escape #\Escape))
         (open-at (search open display)))
    (test-assert open-at
                 "styled output opens an OSC 8 hyperlink at the URL")
    (test-assert (and open-at
                      (search close display
                              :start2 (+ open-at (length open))))
                 "the hyperlink closes after the URL")
    (test-assert (string= (clinedi:ansi-strip display) plain)
                 "hyperlinks leave the visible text unchanged")
    (test-assert (not (search (format nil "~C]8;;" #\Escape)
                              (terminal--render-spans plain-terminal spans)))
                 "unstyled output carries no OSC 8 controls"))
  nil)

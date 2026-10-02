(in-package #:autolith)

;;;; -- URL Detection and Hyperlinks --

(-> test-text-url-ranges () null)
(defun test-text-url-ranges ()
  "Test web URL detection, trailing punctuation, bracket balance, and WEB-URL-P."
  (dolist (case '(("see https://example.com/path."
                   ("https://example.com/path"))
                  ("(https://example.com/a_(b))"
                   ("https://example.com/a_(b)"))
                  ("<https://x.org>, then http://d.e/f?x=1&y=2#frag!"
                   ("https://x.org" "http://d.e/f?x=1&y=2#frag"))
                  ("HTTPS://Up.Case/Path"
                   ("HTTPS://Up.Case/Path"))
                  ("a bare http:// scheme"
                   ())
                  ("[link](https://example.com/wiki_(disambiguation))"
                   ("https://example.com/wiki_(disambiguation)"))
                  ("quoted \"https://q.example/\" text"
                   ("https://q.example/"))
                  ("no links here"
                   ())))
    (destructuring-bind (text expected) case
      (test-assert
       (equal expected
              (loop for (start . end) in (text-url-ranges text)
                    collect (subseq text start end)))
       (format nil "URL detection in ~S" text))))
  (let ((text "visit https://example.com/doc now"))
    (test-assert (string= "https://example.com/doc" (text-url-at text 10))
                 "the URL under an offset is returned")
    (test-assert (null (text-url-at text 2))
                 "offsets before a URL return nothing")
    (test-assert (null (text-url-at text (1- (length text))))
                 "trailing text is not part of the URL"))
  (test-assert (web-url-p "https://example.com/a")
               "a complete https URL is a web URL")
  (test-assert (not (web-url-p "see https://example.com/a"))
               "surrounding text disqualifies a web URL")
  (test-assert (not (web-url-p "ftp://example.com/a"))
               "other schemes are not web URLs")
  (test-assert (not (web-url-p 42))
               "non-strings are not web URLs")
  nil)

(-> test-hyperlinked-rendering () null)
(defun test-hyperlinked-rendering ()
  "Test OSC 8 wrapping of URLs in styled output and its absence elsewhere."
  (let* ((styled (make-instance 'recording-terminal :columns 40 :styled-p t))
         (plain-terminal (make-instance 'recording-terminal :columns 40))
         (spans (list (terminal-span ':plain "go to https://example.com/x now")))
         (plain (terminal--spans-text spans))
         (display (terminal--render-spans styled spans))
         (open (terminal--hyperlink-open-sequence "https://example.com/x"))
         (open-at (search open display)))
    (test-assert open-at
                 "styled output opens an OSC 8 hyperlink at the URL")
    (test-assert (and open-at
                      (search (terminal--hyperlink-close-sequence) display
                              :start2 (+ open-at (length open))))
                 "the hyperlink closes after the URL")
    (test-assert (string= (clinedi:ansi-strip display) plain)
                 "hyperlinks leave the visible text unchanged")
    (let ((rows (clinedi:wrap-styled-text plain display 14)))
      (test-assert (>= (count-if (lambda (pair)
                                   (search (format nil "~C]8;;https://example.com/x" #\Escape)
                                           (second pair)))
                                 rows)
                       2)
                   "every wrapped row showing the URL re-opens its hyperlink"))
    (test-assert (not (search (format nil "~C]8;;" #\Escape)
                              (terminal--render-spans plain-terminal spans)))
                 "unstyled output carries no OSC 8 controls")
    (test-assert (string= "see https://例え.jp/p"
                          (terminal--hyperlinked-text "see https://例え.jp/p"))
                 "non-ASCII URLs stay unlinked")
    (test-assert (eq spans (terminal--presentation-spans spans))
                 "span lists without widgets pass through unchanged"))
  nil)

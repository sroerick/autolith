(in-package #:autolith)

;;;; -- Semantic Syntax Highlighting --

(-> syntax--terminal-span (keyword string) cons)
(defun syntax--terminal-span (role text)
  "Return one Autolith terminal span for Colordiff ROLE and TEXT."
  (terminal-span
   (case role
     (:context-gutter ':dim)
     (:removed-gutter ':failure)
     (:added-gutter ':success)
     ((:context :elision) ':dim)
     (otherwise role))
   text))

(-> syntax--highlight-spans
    (string &key (:language (option language)) (:pathname t))
    (option list))
(defun syntax--highlight-spans (source &key language pathname)
  "Return SOURCE as one sequence of syntax-highlighted terminal spans."
  (highlight-spans source
                   :language language
                   :pathname pathname
                   :span-function #'syntax--terminal-span))

(-> syntax--highlight-lines
    (string &key (:language (option language)) (:pathname t))
    (option vector))
(defun syntax--highlight-lines (source &key language pathname)
  "Return SOURCE as syntax-highlighted rows, or NIL without a language."
  (highlight-lines source
                   :language language
                   :pathname pathname
                   :span-function #'syntax--terminal-span))

(-> syntax--spans-subseq (list integer integer) list)
(defun syntax--spans-subseq (spans start end)
  "Return the character range from START to END within styled SPANS."
  (let ((position 0)
        (result nil))
    (dolist (span spans (nreverse result))
      (let* ((text (terminal-span-text span))
             (span-end (+ position (length text)))
             (part-start (max start position))
             (part-end (min end span-end)))
        (when (< part-start part-end)
          (push (terminal-span
                 (terminal-span-style span)
                 (subseq text
                         (- part-start position)
                         (- part-end position)))
                result))
        (setf position span-end)))))

(in-package #:autolith)

;;;; -- Edit Line Model --

(defparameter *fs-edit-near-match-maximum-characters* 4000
  "The maximum exact near-match characters included in one fs.edit failure.")

(defstruct (workspace-edit-line
            (:constructor workspace--make-edit-line
                (content delimiter start content-end end))
            (:copier nil))
  "One text line with its original delimiter and source offsets."
  (content     "" :type string :read-only t)
  (delimiter   "" :type string :read-only t)
  (start        0 :type (integer 0) :read-only t)
  (content-end  0 :type (integer 0) :read-only t)
  (end          0 :type (integer 0) :read-only t))

(defstruct (workspace-edit-match
            (:constructor workspace--make-edit-match
                (start end first-line last-line indentation-delta kind))
            (:copier nil))
  "One line-wise fs.edit candidate in a file."
  (start              0 :type (integer 0) :read-only t)
  (end                0 :type (integer 0) :read-only t)
  (first-line         1 :type (integer 1) :read-only t)
  (last-line          1 :type (integer 1) :read-only t)
  (indentation-delta  0 :type integer :read-only t)
  (kind          ':eol
                 :type (member :eol :indentation :trim)
                 :read-only t))

(-> workspace--text-lines (string) list)
(defun workspace--text-lines (text)
  "Return TEXT as line records preserving delimiters and exact offsets."
  (loop with size = (length text)
        with start = 0
        while (< start size)
        for newline = (position #\Newline text :start start)
        if newline
          collect (let* ((crlf-p (and (> newline start)
                                      (char= (char text (1- newline)) #\Return)))
                         (content-end (if crlf-p (1- newline) newline))
                         (end (1+ newline)))
                    (prog1
                        (workspace--make-edit-line
                         (subseq text start content-end)
                         (if crlf-p (format nil "~C~C" #\Return #\Newline)
                             (string #\Newline))
                         start
                         content-end
                         end)
                      (setf start end)))
        else
          collect (prog1
                      (workspace--make-edit-line
                       (subseq text start size) "" start size size)
                    (setf start size))))

(-> workspace--line-ending-style (list) (member :none :lf :crlf :mixed))
(defun workspace--line-ending-style (lines)
  "Return the homogeneous delimiter style in LINES, or :MIXED."
  (let ((style ':none))
    (dolist (line lines style)
      (let ((delimiter (workspace-edit-line-delimiter line)))
        (unless (zerop (length delimiter))
          (let ((line-style (if (= (length delimiter) 2) ':crlf ':lf)))
            (cond
              ((eq style ':none)
               (setf style line-style))
              ((not (eq style line-style))
               (return ':mixed)))))))))

(-> workspace--leading-spaces (string) (values (integer 0) boolean))
(defun workspace--leading-spaces (line)
  "Return LINE's leading space count and whether its indentation has no tabs."
  (loop for index below (length line)
        for character = (char line index)
        when (char= character #\Tab)
          do (return (values index nil))
        unless (char= character #\Space)
          do (return (values index t))
        finally (return (values (length line) t))))

;;;; -- Candidate Matching --

(-> workspace--indentation-delta (list list) (values boolean integer))
(defun workspace--indentation-delta (requested-lines actual-lines)
  "Return whether ACTUAL-LINES are one uniform space shift of REQUESTED-LINES."
  (let ((delta nil)
        (different-p nil))
    (loop for requested in requested-lines
          for actual in actual-lines
          for requested-content = (workspace-edit-line-content requested)
          for actual-content = (workspace-edit-line-content actual)
          do (multiple-value-bind (requested-spaces requested-safe-p)
                 (workspace--leading-spaces requested-content)
               (multiple-value-bind (actual-spaces actual-safe-p)
                   (workspace--leading-spaces actual-content)
                 (unless (and requested-safe-p actual-safe-p
                              (string= requested-content actual-content
                                       :start1 requested-spaces
                                       :start2 actual-spaces))
                   (return-from workspace--indentation-delta (values nil 0)))
                 (unless (and (zerop (length requested-content))
                              (zerop (length actual-content)))
                   (let ((line-delta (- actual-spaces requested-spaces)))
                     (when (/= line-delta 0)
                       (setf different-p t))
                     (if delta
                         (unless (= delta line-delta)
                           (return-from workspace--indentation-delta
                             (values nil 0)))
                         (setf delta line-delta)))))))
    (values (and different-p (not (null delta))) (or delta 0))))

(-> workspace--line-range-match
    (list list (member :eol :indentation :trim))
    (values boolean integer))
(defun workspace--line-range-match (requested-lines actual-lines kind)
  "Return whether ACTUAL-LINES match REQUESTED-LINES under KIND and its shift."
  (ecase kind
    (:eol
     (values (loop for requested in requested-lines
                   for actual in actual-lines
                   always (string= (workspace-edit-line-content requested)
                                   (workspace-edit-line-content actual)))
             0))
    (:indentation
     (workspace--indentation-delta requested-lines actual-lines))
    (:trim
     (values
      (loop for requested in requested-lines
            for actual in actual-lines
            always (string=
                    (string-trim '(#\Space #\Tab)
                                 (workspace-edit-line-content requested))
                    (string-trim '(#\Space #\Tab)
                                 (workspace-edit-line-content actual))))
      0))))

(-> workspace--line-match-candidates
    (list list (member :eol :indentation :trim))
    list)
(defun workspace--line-match-candidates (requested-lines file-lines kind)
  "Return every line-aligned FILE-LINES candidate matching REQUESTED-LINES."
  (let* ((count (length requested-lines))
         (candidate-count (max 0 (1+ (- (length file-lines) count))))
         (terminal-delimiter-p
           (plusp (length (workspace-edit-line-delimiter
                           (first (last requested-lines)))))))
    (loop for tail on file-lines
          for first-line from 1
          repeat candidate-count
          for actual-lines = (subseq tail 0 count)
          for last = (first (last actual-lines))
          when (or (not terminal-delimiter-p)
                   (plusp (length (workspace-edit-line-delimiter last))))
            append (multiple-value-bind (matched-p indentation-delta)
                       (workspace--line-range-match
                        requested-lines actual-lines kind)
                     (when matched-p
                       (list
                        (workspace--make-edit-match
                         (workspace-edit-line-start (first actual-lines))
                         (if terminal-delimiter-p
                             (workspace-edit-line-end last)
                             (workspace-edit-line-content-end last))
                         first-line
                         (+ first-line count -1)
                         indentation-delta
                         kind)))))))

(-> workspace--replacement-anchored-p (list list) boolean)
(defun workspace--replacement-anchored-p (old-lines new-lines)
  "Return true when NEW-LINES retain a nonblank boundary line from OLD-LINES."
  (or (null new-lines)
      (let* ((nonblank-old-lines
               (remove-if (lambda (line)
                            (zerop (length (workspace-edit-line-content line))))
                          old-lines))
             (boundaries
               (remove nil
                       (remove-duplicates
                        (list (first nonblank-old-lines)
                              (first (last nonblank-old-lines)))))))
        (not
         (null
          (some (lambda (new-line)
                  (find (workspace-edit-line-content new-line)
                        boundaries
                        :test #'string=
                        :key #'workspace-edit-line-content))
                new-lines))))))

;;;; -- Replacement Projection --

(-> workspace--shift-indentation (string integer) (values (option string) string))
(defun workspace--shift-indentation (content delta)
  "Shift CONTENT by DELTA leading spaces, returning NIL and a reason if unsafe."
  (when (zerop (length content))
    (return-from workspace--shift-indentation (values "" "")))
  (multiple-value-bind (spaces safe-p)
      (workspace--leading-spaces content)
    (cond
      ((not safe-p)
       (values nil "new-text uses a tab in leading indentation"))
      ((minusp (+ spaces delta))
       (values nil
               (format nil "a new-text line cannot be shifted left by ~D space~:P"
                       (- delta))))
      ((minusp delta)
       (values (subseq content (- delta)) ""))
      ((plusp delta)
       (values (concatenate 'string
                            (make-string delta :initial-element #\Space)
                            content)
               ""))
      (t
       (values content "")))))

(-> workspace--render-replacement
    (string string integer (member :eol :indentation))
    (values (option string) string))
(defun workspace--render-replacement (new-text delimiter indentation-delta kind)
  "Render NEW-TEXT for DELIMITER and KIND, or return NIL and an unsafe reason."
  (let* ((lines (workspace--text-lines new-text))
         (style (workspace--line-ending-style lines)))
    (when (eq style ':mixed)
      (return-from workspace--render-replacement
        (values nil "new-text uses mixed line endings")))
    (values
     (with-output-to-string (stream)
       (dolist (line lines)
         (let ((content (workspace-edit-line-content line)))
           (when (eq kind ':indentation)
             (multiple-value-bind (shifted reason)
                 (workspace--shift-indentation content indentation-delta)
               (unless shifted
                 (return-from workspace--render-replacement
                   (values nil reason)))
               (setf content shifted)))
           (write-string content stream)
           (when (plusp (length (workspace-edit-line-delimiter line)))
             (write-string delimiter stream)))))
     "")))

;;;; -- Relaxed Edit Execution --

(-> workspace--bounded-near-match (string) string)
(defun workspace--bounded-near-match (text)
  "Return bounded exact TEXT for an actionable fs.edit near-match diagnostic."
  (if (<= (length text) *fs-edit-near-match-maximum-characters*)
      text
      (format nil "~A~%[near match truncated; read the reported lines before retrying]"
              (subseq text 0 *fs-edit-near-match-maximum-characters*))))

(-> workspace--line-ending-delimiter
    ((member :lf :crlf))
    string)
(defun workspace--line-ending-delimiter (style)
  "Return the delimiter string represented by STYLE."
  (ecase style
    (:lf (string #\Newline))
    (:crlf (format nil "~C~C" #\Return #\Newline))))

(-> workspace--replace-range (string string integer integer) string)
(defun workspace--replace-range (text replacement start end)
  "Return TEXT with the half-open range START through END replaced."
  (concatenate 'string
               (subseq text 0 start)
               replacement
               (subseq text end)))

(-> workspace--near-match-failure
    (pathname string workspace-edit-match string)
    string)
(defun workspace--near-match-failure (path text match reason)
  "Return an actionable failure for an unsafe unique line-wise MATCH."
  (let ((candidate (subseq text
                           (workspace-edit-match-start match)
                           (workspace-edit-match-end match))))
    (let ((first-line (workspace-edit-match-first-line match))
          (last-line (workspace-edit-match-last-line match)))
      (format nil
              "The old-text was not found exactly in ~A. A unique line-wise ~
               near match exists at ~:[line~;lines~] ~D-~D, but it was not ~
               changed because ~A. Retry with this exact old-text:~%~A"
              path
              (/= first-line last-line)
              first-line
              last-line
              reason
              (workspace--bounded-near-match candidate)))))

(-> workspace--ambiguous-near-match-failure
    (pathname list (member :eol :indentation :trim))
    string)
(defun workspace--ambiguous-near-match-failure (path matches kind)
  "Return the unique-match failure for relaxed MATCHES of KIND."
  (format nil
          "The old-text was not found exactly and has ~D line-wise near ~
           matches in ~A after ~A. Include exact context; replace-all applies ~
           only to exact matches."
          (length matches)
          path
          (ecase kind
            (:eol "normalizing line endings")
            (:indentation "adjusting uniform leading-space indentation")
            (:trim "trimming line-edge whitespace"))))

(-> workspace--relaxed-edit
    (pathname string string string)
    (values (member :success :failure :none) (option string) string))
(defun workspace--relaxed-edit (path old-text new-text text)
  "Try one unique, conservative line-wise edit after exact matching fails."
  (let* ((requested-lines (workspace--text-lines old-text))
         (file-lines (workspace--text-lines text))
         (requested-style (workspace--line-ending-style requested-lines))
         (file-style (workspace--line-ending-style file-lines)))
    (when (or (< (length requested-lines) 2)
              (eq requested-style ':mixed))
      (return-from workspace--relaxed-edit (values ':none nil "")))
    (when (eq file-style ':mixed)
      (return-from workspace--relaxed-edit
        (values ':failure nil
                (format nil
                        "The old-text was not found in ~A. Exact matching is ~
                         required because the file uses mixed line endings."
                        path))))
    (when (eq file-style ':none)
      (return-from workspace--relaxed-edit (values ':none nil "")))
    (let ((delimiter (workspace--line-ending-delimiter file-style)))
      (dolist (kind '(:eol :indentation :trim) (values ':none nil ""))
        (let ((matches (workspace--line-match-candidates
                        requested-lines file-lines kind)))
          (when matches
            (when (> (length matches) 1)
              (return-from workspace--relaxed-edit
                (values ':failure nil
                        (workspace--ambiguous-near-match-failure
                         path matches kind))))
            (let* ((match (first matches))
                   (new-lines (workspace--text-lines new-text)))
              (when (eq kind ':trim)
                (return-from workspace--relaxed-edit
                  (values ':failure nil
                          (workspace--near-match-failure
                           path text match
                           "the line-edge whitespace differences cannot be projected as one uniform leading-space shift"))))
              (when (and (eq kind ':indentation)
                         (not (workspace--replacement-anchored-p
                               requested-lines new-lines)))
                (return-from workspace--relaxed-edit
                  (values ':failure nil
                          (workspace--near-match-failure
                           path text match
                           "new-text does not retain an unchanged nonblank old-text boundary line to anchor its indentation"))))
              (multiple-value-bind (replacement reason)
                  (workspace--render-replacement
                   new-text delimiter
                   (workspace-edit-match-indentation-delta match)
                   kind)
                (unless replacement
                  (return-from workspace--relaxed-edit
                    (values ':failure nil
                            (workspace--near-match-failure
                             path text match reason))))
                (return-from workspace--relaxed-edit
                  (values
                   ':success
                   (workspace--replace-range
                    text replacement
                    (workspace-edit-match-start match)
                    (workspace-edit-match-end match))
                   (ecase kind
                     (:eol
                      (format nil
                              "Replaced 1 occurrence in ~A using a unique ~
                               line-ending-normalized match."
                              path))
                     (:indentation
                      (let ((delta
                              (workspace-edit-match-indentation-delta match)))
                        (format nil
                                "Replaced 1 occurrence in ~A using a unique ~
                                 uniform-indentation match; shifted new-text ~
                                 ~:[left~;right~] by ~D space~:P."
                                path
                                (plusp delta)
                                (abs delta)))))))))))))))

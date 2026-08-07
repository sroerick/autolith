(in-package #:autolith)

;;;; -- Native Source Compatibility --

(-> native-source-normalize-reader-boundaries (string) string)
(defun native-source-normalize-reader-boundaries (source)
  "Preserve standard keyword and string boundaries for SEXP-CONFIG.

A double quote terminates a token in the standard Common Lisp readtable, but
SEXP-CONFIG's lexical preflight currently requires explicit whitespace after a
keyword. Insert reader-equivalent spaces only at those boundaries while copying
strings and nested comments verbatim. The restricted grammar and reader still
own all syntax and value validation."
  (let ((length              (length source))
        (token-kind          nil)
        (block-comment-depth 0)
        (line-comment-p      nil)
        (string-p            nil)
        (escaped-p           nil))
    (with-output-to-string (output)
      (loop with index = 0
            while (< index length)
            do (let ((character (char source index))
                     (next      (and (< (1+ index) length)
                                     (char source (1+ index))))
                     (step      1))
                 (cond
                   (line-comment-p
                    (write-char character output)
                    (when (find character '(#\Newline #\Return))
                      (setf line-comment-p nil
                            token-kind nil)))
                   (string-p
                    (write-char character output)
                    (cond
                      (escaped-p
                       (setf escaped-p nil))
                      ((char= character #\\)
                       (setf escaped-p t))
                      ((char= character #\")
                       (setf string-p nil
                             token-kind nil))))
                   ((plusp block-comment-depth)
                    (write-char character output)
                    (cond
                      ((and (char= character #\#) (eql next #\|))
                       (write-char next output)
                       (incf block-comment-depth)
                       (setf step 2))
                      ((and (char= character #\|) (eql next #\#))
                       (write-char next output)
                       (decf block-comment-depth)
                       (setf step 2
                             token-kind nil))))
                   ((char= character #\;)
                    (write-char character output)
                    (setf line-comment-p t
                          token-kind nil))
                   ((and (char= character #\#) (eql next #\|))
                    (write-char character output)
                    (write-char next output)
                    (setf block-comment-depth 1
                          token-kind nil
                          step 2))
                   ((char= character #\")
                    (when (eq token-kind :keyword)
                      (write-char #\Space output))
                    (write-char character output)
                    (setf string-p t
                          token-kind nil))
                   ((find character
                          '(#\Space #\Tab #\Newline #\Return #\Page #\( #\)))
                    (write-char character output)
                    (setf token-kind nil))
                   (t
                    (when (null token-kind)
                      (setf token-kind
                            (if (char= character #\:) :keyword :other)))
                    (write-char character output)))
                 (incf index step))))))

(in-package #:autolith)

;;;; -- Terminal Themes --

(-> test-terminal-theme-tables () null)
(defun test-terminal-theme-tables ()
  "Every theme renders every semantic style, and the Almighty theme degrades gracefully."
  (dolist (theme *terminal-themes*)
    (dolist (name *terminal-style-names*)
      (test-assert (assoc name (terminal-theme-style-table theme))
                   (format nil "theme ~A renders ~A" (terminal-theme-name theme) name))))
  (let ((previous (terminal-theme-name *terminal-theme*)))
    (unwind-protect
         (progn
           (terminal-theme-install ':almighty)
           (test-assert (eq (terminal-theme-style-table *terminal-theme*) *terminal-style-table*)
                        "installing a theme swaps the live style table")
           (dolist (case '((:truecolor "38;2;252;224;77" "24-bit yellow")
                           (:indexed "38;5;221" "the nearest indexed yellow")
                           (:basic "[1;93m" "bright yellow")))
             (destructuring-bind (level fragment description) case
               (let ((cl-colorist:*color-level* level))
                 (test-assert (search fragment (terminal-style-sequence ':brand))
                              (format nil "the Almighty brand style renders as ~A at ~A"
                                      description level)))))
           (let ((cl-colorist:*color-level* ':truecolor))
             (test-assert (search "48;2;22;36;86" (terminal-style-sequence ':status-plain))
                          "the Almighty status bar sits on the site's darkest blue")
             (test-assert (null (terminal-style-sequence ':plain))
                          "plain text keeps the terminal default the theme imposes"))
           (test-assert
            (equal (terminal-theme-enter-sequence *terminal-theme*)
                   (format nil "~C]10;rgb:EF/F6/FF~C~C]11;rgb:19/3C/B8~C"
                           #\Escape #\Bel #\Escape #\Bel))
            "entering the Almighty theme sets the terminal default colors")
           (test-assert
            (equal (terminal-theme-leave-sequence *terminal-theme*)
                   (format nil "~C]110~C~C]111~C" #\Escape #\Bel #\Escape #\Bel))
            "leaving the Almighty theme restores the terminal default colors"))
      (terminal-theme-install previous)))
  (let ((autolith (terminal-theme-find ':autolith)))
    (test-assert (and (string= "" (terminal-theme-enter-sequence autolith))
                      (string= "" (terminal-theme-leave-sequence autolith)))
                 "the default theme leaves the terminal default colors alone"))
  (test-assert (handler-case (progn (terminal-theme-find ':nope) nil)
                 (error () t))
               "unknown theme names are rejected")
  nil)

(-> test-terminal-theme-presentation () null)
(defun test-terminal-theme-presentation ()
  "The Almighty theme colors the fullscreen viewport and replaces the boot mark."
  (let ((previous (terminal-theme-name *terminal-theme*))
        (terminal (make-instance 'recording-terminal :columns 80 :rows 40)))
    (unwind-protect
         (progn
           (terminal-theme-install ':almighty)
           (with-terminal-ui (ui (fullscreen-test--ui terminal))
             (let ((writes (apply #'concatenate 'string
                                  (reverse (recording-terminal-chunks terminal)))))
               (test-assert (search (format nil "~C]11;rgb:19/3C/B8~C" #\Escape #\Bel) writes)
                            "entering the viewport imposes the theme background"))
             (let* ((frame (terminal-ui--boot-screen-frame
                            ui :phase "phase" :detail "detail" :height 40))
                    (plain-rows (mapcar #'clinedi:ansi-strip frame))
                    (plain (format nil "~{~A~%~}" plain-rows)))
               (test-assert (search "A L M I G H T Y  L I S P" plain)
                            "the Almighty boot panel carries its title")
               (test-assert (search "ALMIGHTY TOOLS FOR ALMIGHTY PROGRAMMERS" plain)
                            "the Almighty boot panel carries the site's tagline")
               (test-assert (search "██ ████ ██" plain)
                            "the Almighty boot panel shows the block mark")
               (let* ((rows (mapcar #'clinedi:ansi-strip frame))
                      (almighty (find-if (lambda (row) (search "██   ██ ██      ████  ████" row))
                                         rows))
                      (lisp (find-if (lambda (row) (search "██      ██ ███████ ██████" row))
                                     rows)))
                 (test-assert (and almighty lisp
                                   (= (position #\█ almighty) (position #\█ lisp)))
                              "ALMIGHTY and LISP share one left edge"))
               (let ((border (find-if (lambda (row) (search "┌" row)) plain-rows)))
                 (test-assert (and border (= 72 (- (position #\┐ border) (position #\┌ border) -1)))
                              "the Almighty panel is wider than the default"))
               (test-assert (not (search "A U T O L I T H" plain))
                            "the default title is replaced"))
             (setf (recording-terminal-chunks terminal) nil)
             (terminal-ui-fullscreen-leave ui)
             (let ((writes (apply #'concatenate 'string
                                  (reverse (recording-terminal-chunks terminal)))))
               (test-assert (search (format nil "~C]111~C" #\Escape #\Bel) writes)
                            "leaving the viewport restores the terminal background"))))
      (terminal-theme-install previous)))
  nil)

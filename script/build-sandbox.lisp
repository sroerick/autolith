(require :asdf)

;;; The cl-exec-sandbox helper wraps Linux bubblewrap, seccomp, and network
;;; namespaces, so only Linux builds it. Other platforms load the library's
;;; portable fallback, which runs unsandboxed policies directly and signals
;;; sandbox-unavailable for sandboxed policies.
#+linux
(let ((installed-helper (uiop:getenv "CL_EXEC_SANDBOX_HELPER")))
  (unless (and installed-helper (probe-file installed-helper))
    (let* ((system-root (asdf:system-source-directory :cl-exec-sandbox))
           (builder (merge-pathnames "scripts/build-helper" system-root)))
      (unless (probe-file builder)
        (error "cl-exec-sandbox helper builder is missing at ~A." builder))
      (uiop:run-program (list "/usr/bin/env" "bash" (namestring builder))
                        :output ':interactive
                        :error-output ':interactive))))

#-linux
(format t "~&Skipping the Linux-only cl-exec-sandbox helper on this platform.~%")

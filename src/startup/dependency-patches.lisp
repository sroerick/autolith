(in-package #:autolith)

;;;; -- Pinned Dependency Provenance Patch --

;;; sbcl-generations at df5128f03e60 hardcodes Git queries in image build
;;; records, compatibility checks, fresh savers, and saved-core probes. In a jj
;;; workspace those queries must use the workspace parent and requested-input
;;; cleanliness while retaining Git blob identities. Non-jj builds, including
;;; Nix's Git metadata, still use the library's original implementation.
;;; Patch only that command boundary to keep the build-record protocol intact.
;;; Remove this patch when upstream provides a source-provenance callback used
;;; by all four paths, including the fresh saver and saved-core probe.
(eval-when (:load-toplevel :execute)
  (load (asdf:system-relative-pathname :autolith "script/source-provenance.lisp")))

(defvar *active-image-git-provenance-function*
  (symbol-function 'sbcl-generations::image--git-output)
  "The image library's unmodified provenance implementation.")

(defun active-image--source-command (source-root arguments)
  "Use jj provenance in jj workspaces and the original library otherwise."
  (cl-user::autolith-source-command source-root arguments
                                   *active-image-git-provenance-function*))

(eval-when (:load-toplevel :execute)
  (setf (symbol-function 'sbcl-generations::image--git-output)
        #'active-image--source-command))

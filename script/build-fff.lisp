(require :asdf)
(require :sb-posix)

(defun fff--prepare-build-environment ()
  "Set host-specific compiler flags required to build fff's C dependencies.

  LMDB uses SysV semaphores on BSD and only defines union semun when
  _SEM_SEMUN_UNDEFINED is set. glibc defines that macro. NetBSD does
  not define it and also omits the union, so the flag is required there.
  OpenBSD 7.9 already provides union semun, so the flag would redefine it."
  #+netbsd
  (let ((previous (or (uiop:getenv "CFLAGS") "")))
    (unless (search "-D_SEM_SEMUN_UNDEFINED" previous)
      (sb-posix:setenv "CFLAGS"
                       (string-trim
                        '(#\Space)
                        (format nil "~A -D_SEM_SEMUN_UNDEFINED" previous))
                       1)))
  nil)

(let* ((script-path (truename *load-truename*))
       (script-directory (uiop:pathname-directory-pathname script-path))
       (source-root (uiop:pathname-parent-directory-pathname script-directory))
       (commit-pathname (merge-pathnames "native/fff/commit" source-root))
       (commit (string-trim '(#\Space #\Tab #\Newline #\Return)
                            (uiop:read-file-string commit-pathname)))
       (home (user-homedir-pathname))
       (cache-home
         (uiop:ensure-directory-pathname
          (or (uiop:getenv "XDG_CACHE_HOME")
              (merge-pathnames ".cache/" home))))
       (data-home
         (uiop:ensure-directory-pathname
          (or (uiop:getenv "XDG_DATA_HOME")
              (merge-pathnames ".local/share/" home))))
       (checkout
         (merge-pathnames (format nil "autolith/build/fff/~A/" commit)
                          cache-home))
       (install-root (merge-pathnames "autolith/native/fff/" data-home))
       (library-name #+darwin "libfff_c.dylib"
                     #-darwin "libfff_c.so")
       (library (merge-pathnames library-name install-root))
       (static-build-p
         (equal (uiop:getenv "AUTOLITH_BUILD_STATIC_NATIVE") "1"))
       (static-library-name "libfff_c.a")
       (static-library (merge-pathnames static-library-name install-root))
       (manifest (merge-pathnames "manifest.sexp" install-root)))
  (labels ((run (command &key directory)
             "Run one build COMMAND, preserving its output."
             (uiop:run-program command
                               :directory directory
                               :input ':interactive
                               :output ':interactive
                               :error-output ':interactive))

           (manifest-current-p ()
             "Return true when the installed private library matches COMMIT."
             (and (probe-file library)
                  (or (not static-build-p) (probe-file static-library))
                  (probe-file manifest)
                  (handler-case
                      (with-open-file (stream manifest
                                              :direction ':input
                                              :external-format ':utf-8)
                        (let ((*read-eval* nil))
                          (equal (read stream nil nil)
                                 (list :fff-library
                                       :version 1
                                       :commit commit))))
                    (error ()
                      nil))))

           (publish-file (source target)
             "Atomically copy SOURCE over TARGET."
             (let ((temporary
                     (make-pathname
                      :name (format nil ".~A.~D"
                                    (pathname-name target)
                                    (sb-posix:getpid))
                      :type (pathname-type target)
                      :defaults target)))
               (unwind-protect
                    (progn
                      (uiop:copy-file source temporary)
                      (uiop:rename-file-overwriting-target temporary target))
                 (when (probe-file temporary)
                   (delete-file temporary))))))
    (if (manifest-current-p)
        (format t "~&The pinned private fff library is already installed at ~A.~%"
                library)
        (progn
          (unless (probe-file (merge-pathnames ".git/" checkout))
            (ensure-directories-exist checkout)
            (run (list "git" "init" (namestring checkout)))
            (run (list "git"
                       "-C" (namestring checkout)
                       "remote" "add" "origin"
                       "https://github.com/dmtrKovalenko/fff.git")))
          (format t "~&Fetching fff at ~A.~%" commit)
          (finish-output)
          (run (list "git"
                     "-C" (namestring checkout)
                     "fetch" "--depth" "1" "origin" commit))
          (run (list "git"
                     "-C" (namestring checkout)
                     "checkout" "--detach" "--force" "FETCH_HEAD"))
          (let ((actual
                  (string-trim
                   '(#\Space #\Tab #\Newline #\Return)
                   (uiop:run-program
                    (list "git" "-C" (namestring checkout) "rev-parse" "HEAD")
                    :output ':string))))
            (unless (string= actual commit)
              (error "Fetched fff commit ~A instead of ~A." actual commit)))
            (format t "~&Building fff's C library. The first build can take several minutes.~%")
            (finish-output)
            (fff--prepare-build-environment)
            (run (list "cargo" "build" "--locked" "--release" "-p" "fff-c")
                 :directory checkout)
            (when static-build-p
              (run (list "cargo" "rustc" "--locked" "--release"
                         "-p" "fff-c" "--crate-type" "staticlib")
                   :directory checkout))
          (let ((built (merge-pathnames
                        (format nil "target/release/~A" library-name)
                        checkout)))
            (unless (probe-file built)
              (error "fff did not produce ~A." built))
            (ensure-directories-exist library)
            (publish-file built library)
            (when static-build-p
              (let ((built-static
                      (merge-pathnames
                       (format nil "target/release/~A" static-library-name)
                       checkout)))
                (unless (probe-file built-static)
                  (error "fff did not produce ~A." built-static))
                (publish-file built-static static-library)))
            (let ((temporary
                    (make-pathname
                     :name (format nil ".manifest.~D" (sb-posix:getpid))
                     :type "sexp"
                     :defaults manifest)))
              (unwind-protect
                   (progn
                     (with-open-file (stream temporary
                                             :direction ':output
                                             :if-exists ':supersede
                                             :if-does-not-exist ':create
                                             :external-format ':utf-8)
                       (prin1 (list :fff-library
                                    :version 1
                                    :commit commit)
                              stream)
                       (terpri stream)
                       (finish-output stream))
                     (uiop:rename-file-overwriting-target temporary manifest))
                (when (probe-file temporary)
                  (delete-file temporary))))
            (format t "~&Installed the private fff library at ~A.~%" library))))))

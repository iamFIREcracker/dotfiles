(require 'asdf)

;; Install/load quicklisp
#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (if (probe-file quicklisp-init)
      (load quicklisp-init)
      (progn
        (load (merge-pathnames "quicklisp.lisp" *load-truename*))
        (funcall (find-symbol "INSTALL" (find-package "QUICKLISP-QUICKSTART"))))))

;;; General
(setf *print-escape* t
      *print-circle* t
      *print-pretty* t)

(defun cls ()
  "Clears the screen, and move the cursor to the top.

    Outputs:

    - \033[2J: ANSI sequence to clear the screen
    - \033[H: ANSI sequence to move the cursor to the _home_ position

    It does using some FORMAT magic (e.g. `~:*` to move back in the format argument list)"
  (format t "~A[2J~:*~A[H" #\escape))

;;;; Scratch Marker
(defun sharp-semicolon-reader (stream sub-char numarg)
  (declare (ignore sub-char numarg))
  (loop :while (read-line stream nil nil))
  (values))
(set-dispatch-macro-character #\# #\; #'sharp-semicolon-reader)

;;; Debugging
(load "~/.lisp/pmdb.lisp")

;;; Stop symlinking -- https://www.reddit.com/r/Common_Lisp/comments/lx6al4/loading_an_asdf_system_from_current_directory/gplgpww/
(pushnew '*default-pathname-defaults* asdf:*central-registry*)
(pushnew #P"~/opt/vlime/__main__/lisp/" asdf:*central-registry*)
; (pushnew #P"~/opt/vlime/fix-input-reading/lisp/" asdf:*central-registry*)
; (pushnew #P"~/opt/vlime/phmarek-master/lisp/" asdf:*central-registry*)
; (pushnew #P"~/opt/vlime/upstream-master/lisp/" asdf:*central-registry*)
(pushnew #P"~/opt/slime/__main__/" asdf:*central-registry*)
; (pushnew #P"~/opt/slime/phmarek-master/" asdf:*central-registry*)
; (pushnew #P"~/opt/slime/upstream-master/" asdf:*central-registry*)
(pushnew #P"~/opt/yason/__main__/" asdf:*central-registry*)

;;; Enable specific behaviors when running locally
(pushnew :running-locally *features*)

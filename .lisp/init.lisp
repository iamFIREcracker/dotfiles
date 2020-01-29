;;; The following lines added by ql:add-to-init-file:
#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

;;; General
(setf *print-escape* t
      *print-circle* t
      *print-pretty* t)

;;; Debugging
(load "~/.lisp/pmdb.lisp")

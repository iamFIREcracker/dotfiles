(in-package #:cl-user)
(defpackage #:vlime-lswank (:use #:cl))
(in-package #:vlime-lswank)

(ql:quickload "swank")
(ql:quickload "find-port")

; (setq swank:*log-events* t)

(let ((vlime-dir (find-if (lambda (p) (search "vlime" (namestring p)))
                          asdf:*central-registry*)))
  (if (not vlime-dir)
    (error "vlime not found inside ASDF:*CENTRAL-REGISTRY")
    (let ((vlime-init (merge-pathnames "load-vlime.lisp"
                                       vlime-dir)))
      (format t "Loading Vlime from: ~a~%" vlime-init)
      (load vlime-init))))

(defun read-port (&optional (prompt "a"))
  (format t "Enter ~a port: " prompt)
  (force-output)
  (read))

(defun read-vlime-port () (list (read-port "Vlime")))

(defun run (vlime-port)
  (loop
    :until (restart-case (progn (vlime:main :port vlime-port
                                            #+allegro :backend #+allegro :vlime-patched
                                            )
                                t)
             (choose-different-port (p)
               :report "Choose a different Vlime port"
               :interactive read-vlime-port
               (setf vlime-port p)
               nil))))

(run (find-port:find-port :min 7002))

(load "~/opt/vlime/lisp/load-vlime.lisp")
; (ql:quickload "cffi") ;; Solves: Package CFFI-FEATURES does not exist
(swank-loader:init :load-contribs t) ;; Solves: The name SWANK-REPL does not designate any package

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

(run 7002)

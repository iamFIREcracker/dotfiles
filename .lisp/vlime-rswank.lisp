(in-package #:cl-user)
(defpackage #:vlime-rswank (:use #:cl))
(in-package #:vlime-rswank)

(ql:quickload "dns-client")
(ql:quickload "split-sequence")

(let ((vlime-dir (find-if (lambda (p) (search "vlime" (namestring p)))
                          asdf:*central-registry*)))
  (if (not vlime-dir)
    (error "vlime not found inside ASDF:*CENTRAL-REGISTRY")
    (let ((vlime-init (merge-pathnames "load-vlime.lisp"
                                       vlime-dir)))
      (format t "Loading Vlime from: ~a~%" vlime-init)
      (load vlime-init))))

; (ql:quickload "cffi") ;; Solves: Package CFFI-FEATURES does not exist
(swank-loader:init :load-contribs t) ;; Solves: The name SWANK-REPL does not designate any package

(defun read-ip-address (&optional (prompt "a"))
  (format t "Enter ~a IP: " prompt)
  (force-output)
  (let* ((name-or-ip (read-line))
         (ip (or (org.shirakumo.dns-client:resolve name-or-ip)
                 name-or-ip)))
     (map 'vector #'parse-integer (split-sequence:split-sequence #\. ip))))

(defun read-port (&optional (prompt "a"))
  (format t "Enter ~a port: " prompt)
  (force-output)
  (read))

(defun read-vlime-port () (list (read-port "Vlime")))

(defun run (vlime-port swank-ip swank-port)
  (loop
    :until (restart-case (progn (vlime:main :port vlime-port
                                            :start-swank nil
                                            :swank-interface swank-ip
                                            :swank-port swank-port
                                            #+allegro :backend #+allegro :vlime-patched
                                            )
                                t)
             (choose-different-port (p)
               :report "Choose a different Vlime port"
               :interactive read-vlime-port
               (setf vlime-port p)
               nil))))

(run 7002 (read-ip-address "Swank") (read-port "Swank"))

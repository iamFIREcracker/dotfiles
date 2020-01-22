(load "~/.lisp/init.lisp")

(defvar *last-package* nil)
(defvar *cached-prompt* nil)
(defun package-prompt (stream arg colon-p at-sign-p)
  (declare (ignore arg colon-p at-sign-p))
  (when (not (eq *last-package* *package*))
    (setf *cached-prompt*
          (format nil "~%[CCL] ~A> "
                  (or (first (package-nicknames *package*))
                      (package-name *package*))))
    (setf *last-package* *package*))
  (terpri)
  (princ *cached-prompt* stream))

;; What sorcery is this?  In summary:
;;
;; - Use the first argument as conditional (I believe it represents the number
;;   of pending stacktraces or something
;; - If none, call PACKAGE-PROMPT -- we need to re-add one argument to the stack
;;   or invoking user defined functions would fail
;; - If there are pending exceptions to process, print the lot followed by `]`
;;
;; FORMAT directives 101
;;
;; ~[...~] Conditional expression. This is a set of control strings, called
;;   clauses, one of which is chosen and used. The clauses are separated by ~;
;;   and the construct is terminated by ~].  Also, ~:; can be used to mark a
;;   default clause
;; ~/name/ calls the user defined function, NAME
;; ~:* ignores backwards; that is, it backs up in the list of arguments so
;;   that the argument last processed will be processed again. ~n:* backs up
;;   n arguments.
;;
;; Source: https://lists.clozure.com/pipermail/openmcl-devel/2015-January/010862.html
(setf ccl::*listener-prompt-format* "~[~:*~/package-prompt/~:;~:*~d]~]")

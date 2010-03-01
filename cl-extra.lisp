(defmacro mapcar-true (fn list)
  "Like `mapcar', but remove nil elements."
  `(remove-if #'null (mapcar ,fn ,list)))

(defmacro mapcan-true (fn list)
  "Like `mapcan', but remove nil elements."
  `(remove-if #'null (mapcan ,fn ,list)))

(define-condition unexpected-input (error) ((text :initarg :text :reader text))
  (:report (lambda (condition stream)
	     (format stream "Unexpected input: ~A" (text condition)))))

(defmacro aif (test conseq &optional (else nil))
  "Anaphoric if macro, binds IT to the value of TEST during the
execution of CONSEQ, and SETF-IT to a macro which SETFs TEST to its
argument during the execution of ELSE. Yanked from
http://www.cliki.net/common-idioms"
  `(let ((it ,test))
     (declare (ignorable it))
     (if it ,conseq
         (macrolet ((setf-it (val) (list 'setf ',test val)))
           ,else))))
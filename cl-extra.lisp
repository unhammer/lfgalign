;;; Tell SBCL we want full debugging info (eg. no function inlining),
;;; but don't care about speed:
(declaim (optimize (speed 0) (safety 3) (debug 3)))

(in-package #:lfgalign)

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
	 ;; TODO: can setf-it work on conseq too?
         (macrolet ((setf-it (val) (list 'setf ',test val)))
           ,else))))

(defmacro awhen (test &body body)
  `(let ((it ,test))
     (declare (ignorable it))
     (when it ,@body)))

;;; Disjoint set implementation. Remember to (setf *print-circle* t)
;;; if you want to use this! Also, to accumulate child-vals, don't set
;;; `dset-parent', but use `dset-setparent' instead.
(defstruct dset parent val (rank 0) child-vals)

(defun dset-find (d)
  "Find the root"
  (aif (dset-parent d)
       (setf (dset-parent d) (dset-find it))
       d))

(defun dset-setparent (child parent)
  (setf (dset-parent child) parent)
  (setf (dset-child-vals parent) (union
 				  (cons child
					(dset-child-vals child))
 				  (dset-child-vals parent)))
  (setf (dset-child-vals child) nil))

(defun dset-union (d1 d2)
  "Destructively union `d1' and `d2', returning the new root."
  (let* ((root1 (dset-find d1))
	 (root2 (dset-find d2))
	 (rank1 (dset-rank root1))
	 (rank2 (dset-rank root2)))
    (cond
      ((> rank1 rank2) (dset-setparent root2 root1))
      ((< rank1 rank2) (dset-setparent root1 root2))
      ((not (eq root1 root2))
       (dset-setparent root2 root1)
       (incf (dset-rank root1))
       root1)
      (t root1))))

(defun dset-collect (equivs)
  "Return a table where keys are f-structure variable numbers, values
are dsets representing the equivalence classes. Runs on the |eqvar|
alist from `dup-alist-to-table'. Non-numbers are not added, since
their hashes are not unique."
  (labels ((addnew (val tab)
	     (aif (gethash val tab)
		  it
		  (if (numberp val)
		      (setf (gethash val tab) (make-dset :val val))
		      (make-dset :val val)))))
    
      (loop for (aval . bval) in equivs
	 with dsets = (make-hash-table)
	 for aset = (addnew aval dsets)
	 for bset = (addnew bval dsets)
	 do (dset-union bset aset)
	 finally (return dsets))))

(defun dset-findall (val dsets)
  "Return all dsets in the same set as `val' in the hash table
`dsets'."
  (aif (gethash val dsets)
       (let ((root (dset-find it)))
	 (mapcar #'dset-val
		 (cons root (dset-child-vals root))))))


;;; Alternative, simpler dset-implementation. TODO: which one's
;;; better?
(defun dset2-union (apair bpair)
  (unless (cdr apair) (setf (cdr apair) (list apair)))
  (unless (cdr bpair) (setf (cdr bpair) (list bpair)))
  (setf (cdr apair)
	(union (cdr apair) (cdr bpair)))
  (mapcar
   (lambda (xpair) (setf (cdr xpair) (cdr apair)))
   (cdr apair)))

(defun dset2-collect (equivs)
  (labels ((addnew (val tab)
	     (aif (gethash val tab)
		  it
		  (if (numberp val)
		      (setf (gethash val tab) (list val))
		      (list val)))))
    
      (loop for (aval . bval) in equivs
	 with dsets = (make-hash-table)
	 for apair = (addnew aval dsets)
	 for bpair = (addnew bval dsets)
	 do (dset2-union bpair apair)
	 finally (return dsets))))

(defun dset2-findall (val dsets)
  (mapcar #'car (cdr (gethash val dsets))))
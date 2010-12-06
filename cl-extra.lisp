;;; Tell SBCL we want full debugging info (eg. no function inlining),
;;; but don't care about speed:
(declaim (optimize (speed 0) (safety 3) (debug 3)))

(in-package #:lfgalign)

;;;;;;;; HELPER MACROS:
;;;;;;;; --------------
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

(defmacro out (string &rest args) `(format t ,string ,@args))

(defgeneric add (key val store)
  (:documentation "Add `val' to `store' indexed on `key'."))

(defgeneric get-val (key store)
  (:documentation "Return the value indexed on `key' in `store'."))

(defun member-either (pair list)
  "Either car or cdr of `pair' appears as car or cdr, respectively, in
the list of pairs `list'."
  (member pair list
	  :test (lambda (a b) (or (eq (car a) (car b))
				  (eq (cdr a) (cdr b))))))

(defun assoc-equal (item alist &key key)
  (assoc item alist :key key :test #'equal))

(defun split-str-by (string char)
  (loop for i = 0 then (1+ j)
     as j = (position char string :start i)
     collect (subseq string i j)
     while j))

;;;;;;;; DSET implementations. Remember to (setf *print-circle* t) 
;;;;;;;; ----------------------------------------------------------

;;; Implementation 1, standard disjoint set with union-by-rank and
;;; path compression. Unfortunately, we need to accumultate child-vals
;;; for findall, "almost" walking the whole lists for each
;;; union. To accumulate child-vals, don't set `dset-parent', but use
;;; `dset-setparent' instead.
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


;;; Implementation 2, singly linked list. Simpler, but still needs to
;;; walk both lists completely during union, resetting their cdr:
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

;;; Implementation 3, doubly linked lists. Probably the best one,
;;; union is constant time; findall still needs to walk the list, but
;;; we always have to do that to clean it up. Use `dset3-new' instead
;;; of `make-dset3' as constructor.
(defstruct dset3 val next prev)

(defun dset3-new (val)
  (let ((node (make-dset3 :val val)))
    (unless (and (dset3-next node) (dset3-prev node))
      (setf (dset3-next node) node)
      (setf (dset3-prev node) node))
    node))

(defun dset3-union (a b)
  "u-a-x-u
U  v-y-b-v
=> u-a-b-v-y-x-u"
  (let ((x (dset3-next a))
	(y (dset3-prev b)))
    (setf (dset3-next a) b)
    (setf (dset3-prev b) a)
    (setf (dset3-next y) x)
    (setf (dset3-prev x) y)))

(defun dset3-collect (equivs)
  (labels ((addnew (val tab)
	     (aif (gethash val tab)
		  it
		  (if (numberp val)
		      (setf (gethash val tab) (dset3-new val))
		      (dset3-new val)))))

      (loop for (aval . bval) in equivs
	 with dsets = (make-hash-table)
	 for apair = (addnew aval dsets)
	 for bpair = (addnew bval dsets)
	 do (dset3-union bpair apair)
	 finally (return dsets))))

(defun dset3-findall (val dsets)
  (aif (gethash val dsets)
       (loop with first = it
	     for current = (dset3-next first) then (dset3-next current)
	     collect (dset3-val current)
	     until (eq current first))))

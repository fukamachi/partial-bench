(in-package :cl-user)
(defpackage partial-bench
  (:use :cl)
  (:import-from :partial-bench.pretty-table
                :print-table)
  (:import-from :local-time
                :now
                :timestamp-difference)
  (:import-from :alexandria
                :with-gensyms
                :hash-table-keys)
  (:export :*enable-partial-bench*
           :with-bench
           :print-stats))
(in-package :partial-bench)

(defparameter *enable-partial-bench* nil)

(defvar *benchmark-count* (make-hash-table :test 'eq))
(defvar *benchmark-time* (make-hash-table :test 'eq))
(defvar *benchmark-max-time* (make-hash-table :test 'eq))
(defvar *benchmark-min-time* (make-hash-table :test 'eq))

(defmacro with-bench (name &body body)
  (with-gensyms (start diff)
    `(if *enable-partial-bench*
         (let ((,start (now)))
           (prog1 (progn ,@body)
             (incf (gethash ',name *benchmark-count* 0))
             (let ((,diff (timestamp-difference (now) ,start)))
               (setf (gethash ',name *benchmark-max-time*)
                     (max (gethash ',name *benchmark-max-time* 0)
                          ,diff))
               (setf (gethash ',name *benchmark-min-time*)
                     (min (gethash ',name *benchmark-min-time* most-positive-fixnum)
                          ,diff))
               (incf (gethash ',name *benchmark-time* 0)
                     ,diff))))
         (progn ,@body))))

(defun print-stats ()
  (print-table
   '("Name" "Seconds" "Calls" "Avg" "Max" "Min")
   '(:center :right :right :right :right :right)
   (loop for name in (sort (hash-table-keys *benchmark-time*)
                           (lambda (a b)
                             (< (gethash b *benchmark-time*)
                                (gethash a *benchmark-time*))))
         collect (list name
                       (format nil "~6$" (gethash name *benchmark-time*))
                       (gethash name *benchmark-count*)
                       (format nil "~12$" (/ (gethash name *benchmark-time*) (gethash name *benchmark-count*)))
                       (format nil "~12$" (gethash name *benchmark-max-time*))
                       (format nil "~12$" (gethash name *benchmark-min-time*)))))
  (setf *benchmark-time* (make-hash-table :test 'eq)
        *benchmark-count* (make-hash-table :test 'eq))
  (values))

#|
  This file is a part of partial-bench project.
  Copyright (c) 2014 Eitaro Fukamachi (e.arrows@gmail.com)
|#

#|
  Author: Eitaro Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage partial-bench-asd
  (:use :cl :asdf))
(in-package :partial-bench-asd)

(defsystem partial-bench
  :version "0.1"
  :author "Eitaro Fukamachi"
  :license "Public Domain"
  :depends-on (:local-time
               :alexandria)
  :components ((:module "src"
                :components
                ((:file "partial-bench" :depends-on ("pretty-table"))
                 (:file "pretty-table"))))
  :description ""
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.markdown"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq))))

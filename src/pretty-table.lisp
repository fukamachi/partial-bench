(in-package :cl-user)
(defpackage partial-bench.pretty-table
  (:use :cl)
  (:export :print-table))
(in-package :partial-bench.pretty-table)

(defun print-field (object &key (field-space (length object)) (align :center))
  (let ((object (princ-to-string object)))
    (assert (<= (length object) field-space))
    (let ((rest-space (- field-space (length object))))
      (ecase align
        (:center
         (dotimes (i (floor (/ rest-space 2)))
           (write-char #\Space))
         (princ object)
         (dotimes (i (ceiling (/ rest-space 2)))
           (write-char #\Space)))
        (:left
         (princ object)
         (dotimes (i rest-space)
           (write-char #\Space)))
        (:right
         (dotimes (i rest-space)
           (write-char #\Space))
         (princ object))))))

(defun print-line (fields field-settings &key (delimiter "|"))
  (fresh-line)
  (princ delimiter)
  (write-char #\Space)
  (loop for field in fields
        for (space align) in field-settings
        do (print-field field :field-space space :align align)
           (write-char #\Space)
           (princ delimiter)
           (write-char #\Space))
  (write-char #\Newline))

(defun print-delimiter-line (field-settings &key (delimiter "|"))
  (fresh-line)
  (princ delimiter)
  (loop for (space align) in field-settings do
    (if (eq align :left)
        (write-char #\:)
        (write-char #\Space))
    (dotimes (i space)
      (write-char #\-))
    (if (eq align :right)
        (write-char #\:)
        (write-char #\Space))
    (princ delimiter))
  (write-char #\Newline))

(defun print-table (headers align-settings table)
  (let* ((column-count (length headers))
         (column-length (make-array column-count :element-type 'fixnum :initial-element 0)))
    (flet ((calc-max-space (line)
             (loop for i from 0
                   for column in line
                   do (setf (aref column-length i)
                            (max (aref column-length i)
                                 (length (princ-to-string column)))))))
      (calc-max-space headers)
      (loop for line in table do
        (calc-max-space line)))

    (print-line headers
                (loop for space across column-length
                      collect (list space :center)))
    (let ((settings (loop for space across column-length
                          for align in align-settings
                          collect (list space align))))
      (print-delimiter-line settings)
      (loop for line in table do
        (print-line line settings)))))

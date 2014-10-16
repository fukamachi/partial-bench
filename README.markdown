# Partial-Bench

This is a tiny benchmarking library for Common Lisp that allows you to get time for running a specific _partial_ code.

## Usage

```common-lisp
(use-package :partial-bench)

;; Add `with-bench` to where you want to benchmark.
(defun do-something ()
  (flet ((fn1 ()
           (with-bench fn1-bench
             ...))
         (fn2 ()
           (with-bench fn2-bench
             ...)))
    ...))

(setf *enable-partial-bench* t)

(dotimes (i 1000000)
  (do-something))

(print-stats)
;-> |   Name    | Seconds  |  Calls  |       Avg      |      Max       |      Min       |
;   | --------- | --------:| -------:| --------------:| --------------:| --------------:|
;   | FN1-BENCH | 0.309170 | 1000000 | 0.000000309170 | 0.009572000000 | 0.000000000000 |
;   | FN2-BENCH | 0.255818 |   81932 | 0.000003122321 | 0.006627000000 | 0.000000000000 |
;=> NIL
```

## Author

* Eitaro Fukamachi (e.arrows@gmail.com)

## License

Licensed under the Public Domain License.

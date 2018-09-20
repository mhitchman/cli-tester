;;;; package.lisp

(defpackage #:cli-tester
  (:use #:cl)
  (:export #:register-program
	   #:run-tests
	   #:deftest))

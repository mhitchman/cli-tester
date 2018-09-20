;;;; cli-tester.lisp

(in-package #:cli-tester)

(defparameter *program-to-run* nil)
(defparameter *test-list* (make-hash-table :test 'equal))

(defun register-program (program)
  #| check that the program exists
  and is executable |#
  (setf *program-to-run* program))

(defun deftest (&key name args (expected-result ""))
  (unless name
    (setf name (gensym)))
  (setf (gethash name *test-list*) (list :args args :expected-result expected-result)))

(defun run-tests ()
  (let ((passed ())
	(failed ()))
    (loop for name being the hash-keys in *test-list* using (hash-value test-case)
	  do (let* ((args (getf test-case :args))
		    (expected-result (getf test-case :expected-result))
		    (result (typecase args
			      (list (apply #'run-and-return-result args))
			      (null (funcall #'run-and-return-result))
			      (otherwise (funcall #'run-and-return-result args)))))
	       (if (string= expected-result result)
		   (push (list name result) passed)
		   (push (list name result) failed))))
    (print-test-results passed failed)))

(defun print-test-results (passed-tests failed-tests)
  (format t "Failed:~&")
  (dolist (test-case failed-tests)
    (let* ((name (first test-case))
	   (args (getf (gethash name *test-list*) :args))
	   (expected-result (getf (gethash name *test-list*) :expected-result))
	   (command-result (second test-case)))
      (format t " ~a ~a~%   Failed with:~%~0,1,5,' @a~%   Expected:~%~0,1,5,' @a" *program-to-run* args command-result expected-result))))

(defun run-and-return-result (&rest args)
  (let ((stream (uiop:process-info-output (uiop:launch-program `(,*program-to-run* ,@args) :output :stream)))
	(final-string))
    (loop
      (multiple-value-bind (line eof) (read-line stream nil)
	(if eof
	    (return)
	    (push line final-string))))
    (format nil "~{~a~^~&~}" (reverse final-string))))

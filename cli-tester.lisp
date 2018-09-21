;;;; cli-tester.lisp

(in-package #:cli-tester)

(defparameter *program-to-run* nil)
(defparameter *test-list* (make-hash-table :test 'equal))

(defun register-program (program)
  "Expects string or pathname to the executable to be tested.
The path can be either relative or absolute."
  (let* ((full-pathname (if (uiop:relative-pathname-p program)
					(merge-pathnames program (uiop:getcwd))
					program))
	 (program-exists (uiop:file-exists-p full-pathname)))
    (if program-exists
	(setf *program-to-run* full-pathname)
	(error "~a doesn't exist" full-pathname))))

(defun deftest (&key name args (expected-result ""))
  "Creates a test to be run with a name for the test, arguments for the program
and the expected program output. Args should be a string or list of strings for
multiple arguments."
  (unless name
    (setf name (gensym)))
  (setf (gethash name *test-list*)
	(list :args args :expected-result expected-result)))

(defun run-tests ()
  "Run through all the tests created with deftest and print the results and
details on failure."
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
  "Print the results of the testing."
  (unless (uiop:emptyp failed-tests)
    (format t "Failed:~&")
    (dolist (test-case failed-tests)
      (let* ((name (first test-case))
	     (args (getf (gethash name *test-list*) :args))
	     (expected-result (getf (gethash name *test-list*) :expected-result))
	     (command-result (second test-case)))
	(format t " Test: ~a ~:[~a~;~{~a~^ ~}~]~%   Failed with:~%~a~2%   Expected:~%~a~2%"
		(pathname-name *program-to-run*)
		(listp args)
		args command-result expected-result))))
  (format t "~&Passed: ~a       Failed: ~a~%" (length passed-tests) (length failed-tests)))

(defun run-and-return-result (&rest args)
  (let ((stream (uiop:process-info-output (uiop:launch-program `(,*program-to-run* ,@args) :output :stream)))
	(final-string))
    (loop
      (multiple-value-bind (line eof) (read-line stream nil)
	(if eof
	    (return)
	    (push line final-string))))
    (format nil "~{~a~^~&~}" (reverse final-string))))

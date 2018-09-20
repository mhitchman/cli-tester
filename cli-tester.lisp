;;;; cli-tester.lisp

(in-package #:cli-tester)

(defparameter *program-to-run* nil)
(defparameter *test-list* ())

(defun register-program (program)
  #| check that the program exists
  and is executable |#
  (setf *program-to-run* program))

(defun check (args expected-result)
  (push (list args expected-result) *test-list*))

(defun run-tests ()
  (setf *test-list* (nreverse *test-list*))
  (let ((passed 0)
	(failed 0))
    (dolist (test-case *test-list*)
      (let ((args (first test-case))
	    (expected-result (second test-case)))
	(if (string= expected-result (typecase args
				       (list (apply #'run-and-return-result args))
				       (null (funcall #'run-and-return-result))
				       (otherwise (funcall #'run-and-return-result args))))
	    (incf passed)
	    (incf failed))))
    (format t "Passed: ~a~%Failed: ~a~%" passed failed)))

(defun run-and-return-result (&rest args)
  (let ((stream (uiop:process-info-output (uiop:launch-program `(,*program-to-run* ,@args) :output :stream)))
	(final-string))
    (loop
      (multiple-value-bind (line eof) (read-line stream nil)
	(if eof
	    (return)
	    (push line final-string))))
    (format nil "~{~a~^~&~}" (reverse final-string))))

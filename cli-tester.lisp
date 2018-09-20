;;;; cli-tester.lisp

(in-package #:cli-tester)

(defparameter *program-to-run* nil)

(defun register-program (program)
  #| check that the program exists
  and is executable |#
  (setf *program-to-run* program))

(defun check (args expected-result)
  (string= expected-result (typecase args
			     (list (apply #'run-and-return-result args))
			     (null (funcall #'run-and-return-result))
			     (otherwise (funcall #'run-and-return-result args)))))

(defun run-and-return-result (&rest args)
  (let ((stream (uiop:process-info-output (uiop:launch-program `(,*program-to-run* ,@args) :output :stream)))
	(final-string))
    (loop
      (multiple-value-bind (line eof) (read-line stream nil)
	(if eof
	    (return)
	    (push line final-string))))
    (format nil "~{~a~^~&~}" (reverse final-string))))

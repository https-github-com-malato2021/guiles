(define-module (core log))

(export log)

(define (log level msg)
  (display (format #f
		   "~a [~a] ~a~%"
		   (strftime "%F %T" (localtime (current-time)))
		   level
		   msg)
	   (current-error-port)))

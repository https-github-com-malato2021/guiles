(define-module (core util))

(define (assoc-def key lst def)
  (let ((pair (assoc key lst)))
    (if pair
	(cdr pair)
	def)))

(define (exit-with-error msg)
  (let ((port (current-error-port)))
    (display msg port)
    (newline port)
    (quit 1)))

(export assoc-def
	exit-with-error)

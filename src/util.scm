(define-module (util))

(define (exit-with-error msg)
  (let ((port (current-error-port)))
    (display msg port)
    (newline port)
    (quit 1)))

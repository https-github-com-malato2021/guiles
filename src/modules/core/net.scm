(define-module (core net))

(define (make-connection host port)
  (let* ((sock (socket PF_INET SOCK_STREAM 0))
	 (con (connect sock
		       AF_INET
		       (car (vector-ref (gethost host) 4))
		       port)))
    (list host port sock con)))

(export make-connection)

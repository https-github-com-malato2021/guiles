(define-module (core net))

(define (make-connection host port)
  (let* ((host-info (car (getaddrinfo host)))
	 (sock (socket PF_INET SOCK_STREAM 0))
	 (con (connect sock
		       AF_INET
		       (vector-ref (addrinfo:addr host-info) 1)
		       port)))
    (list host port sock con)))

(export make-connection)

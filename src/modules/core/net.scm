(define-module (core net))

(use-modules (gnutls))

(define (make-connection host port ssl)
  (let* ((sock (socket PF_INET SOCK_STREAM 0))
	 (con (connect sock
		       AF_INET
		       (car (vector-ref (gethost host) 4))
		       port)))
    (list host
	  port	  
	  (if ssl
	      (let ((client (make-session connection-end/client)))
		(set-session-default-priority! client)
		(set-session-priorities! client "NORMAL:%COMPAT:-VERS-SSL3.0")
		(set-session-transport-fd! client (fileno sock))
		(set-session-credentials!
		 client
		 (make-certificate-credentials))
		(handshake client)
		(session-record-port client))
	      sock))))



(export make-connection)

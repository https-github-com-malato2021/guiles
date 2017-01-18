(define-module (core init))

(use-modules (core util)
	     (core hooks)
	     (core net)
	     (core irc)
	     (core log)
	     (ice-9 rdelim))

(define-public *connection* #f)

(define-public (start-bot config)
  (let* ((host (assoc-def 'host config "127.0.0.1"))
	 (port (assoc-def 'port config 6667))
	 (nick (assoc-def 'nick config "guiles"))
	 (ssl (assoc-def 'ssl config #f))
	 (user-name (assoc-def 'user-name config "guiles"))
	 (real-name (assoc-def 'real-name config "Guiles")))
    (when (or (not host) (not port))
       (begin
	 (when (not host)
	   (log 'error "Config does not contain host definition."))
	 (when (not port)
	   (log 'error "Config does not contain port definition."))
	 (exit-with-error "Problem with config, check logs.")))
    (set! *connection* (make-connection host port ssl))
    (add-hook 'irc-raw
       (λ (con args)
	 (let ((cmd (decode-cmd (car args))))
	   (if cmd
	       (begin
		 (run-event con
			    (string->symbol
			     (string-append "irc-command-"
					    (string-downcase (cmd-name cmd))))
			    (list cmd))
		 (log 'debug (format #f "Run event ~a~%" (string-append "irc-command-" (string-downcase (cmd-name cmd)))))
		 cmd)
	       (begin
		 (display "Could not read command")
		 (newline))))))
    (register (caddr *connection*) nick 2 user-name real-name)
    (add-hook 'irc-command-ping ping-handler)
    (let read-loop ((line (read-line (caddr *connection*))))
      (when (not (eof-object? line))
	(begin
	  (log 'debug (string-append "LINE:" line))
	  (run-event *connection* 'irc-raw (list line))
	  (read-loop (read-line (caddr *connection*))))))))


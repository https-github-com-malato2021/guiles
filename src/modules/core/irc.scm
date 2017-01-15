(define-module (core irc))

(use-modules (srfi srfi-9)
	     (srfi srfi-11)
             (core init)
             (core net))

(export
 make-cmd cmd? cmd-origin cmd-name cmd-args cmd-tail
 make-user user? user-nick user-name user-host
 make-server server? server-name
 make-msg msg? msg-user msg-time msg-text msg-nick
 cmd->string
 decode-cmd
 register
 ping-handler
 join
 renick)

(define-record-type <cmd>
  (make-cmd origin name args tail)
  cmd?
  (origin cmd-origin)
  (name cmd-name)
  (args cmd-args)
  (tail cmd-tail))

(define-record-type <user>
  (make-user nick name host)
  user?
  (nick user-nick)
  (name user-name)
  (host user-host))

(define-record-type <server>
  (make-server name)
  server?
  (name server-name))

(define-record-type <msg>
  (make-msg user time text)
  msg?
  (user msg-user)
  (time msg-time)
  (text msg-text))

(define (msg-nick msg)
  (if (msg? msg)
      (user-nick (msg-user msg))
      #f))

(define (parse-origin str)
  (if (string-index str #\!)
      (let* ((halves (string-split str #\!))
             (nick (car halves))
             (host-str (cadr halves))
             (at-index (string-index host-str #\@))
             (name (substring host-str 1 at-index))
             (host (substring host-str (+ 1 at-index))))
        (make-user nick name host))
      (make-server str)))

(define (decode-cmd str)
  (let ((extract-origin
	 (λ (str)
	   (if (string-prefix? ":" str)
	       (let* ((space-index (string-index str #\space))
		      (colon-index (string-index str #\:))
		      (origin-str (substring str 0 space-index))
		      (rest-str (substring str (1+ space-index))))
		 (values origin-str rest-str))
	       (values #t str))))
	(extract-cmd
	 (λ (str)
	   (let* ((space-index (string-index str #\space))
		  (cmd (if space-index
			   (substring str 0 space-index)
			   str))
		  (tail (if space-index
			    (substring str (1+ space-index))
			    #f)))
	     (values cmd tail))))
	(extract-args-and-tail
	 (λ (str)
	   (if (string-prefix? ":" str)
	       (values #f (substring str 1))
	       (let ((colon-index (string-index str #\:)))
		 (if colon-index
		     (values (string-split (substring str 0 (1- colon-index)) #\space)
			     (substring str (1+ colon-index)))
		     (values (string-split str #\space)
			     #f)))))))
    (let*-values (((origin orest) (extract-origin str))
		  ((cmd crest) (extract-cmd orest))
		  ((args tail) (extract-args-and-tail crest)))
      (make-cmd origin cmd args tail))))

(define (cmd->string cmd)
  (let ((origin (cmd-origin cmd))
	(name (cmd-name cmd))
	(args (cmd-args cmd))
	(tail (cmd-tail cmd)))
    (string-join `(,@(if origin
			 (list (string-append ":" origin))
			 '())
		   ,name
		   ,@(if args
			 args
			 '())
		   ,@(if tail
			 (list (string-append ":" tail))
			 '())))))

(define (register sock nick mode user-name real-name)
  (display (cmd->string (make-cmd #f "NICK" '() nick)) sock)
  (newline sock)
  (display (cmd->string (make-cmd #f "USER" (list user-name "1" "0") real-name)) sock)
  (newline sock))

(define (ping-handler con args)
  (let* ((cmd (car args))
	 (ping-data (cmd-tail cmd))
	 (sock (caddr con)))
    (display (cmd->string (make-cmd #f "PONG" '() ping-data) sock))
    (newline sock)))

(define-module (core irc))

(use-modules (srfi srfi-9)
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
  (if (string-prefix? ":" str)
      (let* ((str (string-trim-right (substring str 1)))
             (space-index (string-index str #\space))
             (colon-index (string-index str #\:))
             (origin-str (substring str 0 space-index))
             (parts (string-split
                     (string-trim-both
                      (substring str
                                space-index
                                (or colon-index
                                    (- (string-length str) 1))))
                     #\space))
             (cmd (if (not (null? parts))
                      (car parts)
                      #f))
             (args (if (> (length parts) 1)
                       (cdr parts)
                       '()))
             (tail (if colon-index
                       (substring str (+ 1 colon-index))
                       #f)))
          (make-cmd (parse-origin origin-str) cmd args tail))
      #f))

(define (cmd->string cmd)
  (format #f "~a ~a :~a" (cmd-name cmd)
	                  (string-join (cmd-args cmd))
			  (cmd-tail cmd)))

(define (register sock nick mode user-name real-name)
  (display (cmd->string (make-cmd #f "NICK" '() nick)) sock)
  (newline sock)
  (display (cmd->string (make-cmd #f "USER" (list user-name "1" "0") real-name)) sock)
  (newline sock))

(define (ping-handler con args)
  (let* ((cmd (car args))
	 (ping-data (cmd-tail cmd))
	 (sock (caddr con)))
    (display (make-cmd #f "PONG" '() ping-data) sock)
    (newline sock)))

;; Guiles, a simple IRC bot written in Guile Scheme
;; Copyright (C) 2017 Robert Bolton

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(define-module (core irc))

(use-modules (srfi srfi-9)
	     (srfi srfi-11)
	     (srfi srfi-1)
             (core init)
             (core net)
	     (core log)
	     (core hooks))

(export
 make-cmd cmd? cmd-origin cmd-name cmd-args cmd-tail
 make-user user? user-nick user-name user-host
 make-server server? server-name
 make-msg msg? msg-user msg-time msg-text msg-nick
 cmd->string
 decode-cmd
 send-cmd
 register
 ping-handler
 join
 part
 nick
 say)

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

;;; TODO: Modify cmd sending to break the commands into 512 byte
;;; commands, potentially by breaking it into several commands
;;; (e.g. in the case of PRIVMSG) or simply trim to 512 bytes
;;; (or fewer) in the case of most other things

(define (msg-nick msg)
  (if (msg? msg)
      (user-nick (msg-user msg))
      #f))

(define (cmd-overhead cmd)
  (+ 3
     (string-length (cmd-name cmd))
     (fold + 0 (map string-length (cmd-args cmd)))     
     (- (length (cmd-args cmd)) 1)))
	   

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
  (let* ((str (string-trim-right str #\return))
	 (extract-origin
	  (λ (str)
	    (if (string-prefix? ":" str)
		(let* ((space-index (string-index str #\space))
		       (colon-index (string-index str #\:))
		       (origin-str (substring str 1 space-index))
		       (rest-str (substring str (1+ space-index))))
		  (values (parse-origin origin-str) rest-str))
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


;;; TODO: Fix this to deal with unicode.
;;;       Currently, it calculcateds string length
;;;       which is likely number of characters, not bytes
(define (privmsg-splitter cmd)
  (let* ((cmd-str (cmd->string cmd))
	 (size (string-length cmd-str))
	 (channel (car (cmd-args cmd)))
	 (overhead (cmd-overhead cmd))
	 (max-payload (- 500 overhead))
	 (num-msgs (ceiling (/ size max-payload))))
    (let split ((cmds '())
		(str-left (cmd-tail cmd)))
      (if (= 0 (string-length str-left))
	  (reverse cmds)
	  (let* ((str-len (string-length str-left))
		 (amount-taken (if (< max-payload str-len)
				   max-payload
				   str-len))
		 (tail (substring str-left 0 amount-taken))
		 (rest (substring str-left amount-taken))
		 (cmd (make-cmd #f "PRIVMSG"
				(list channel)
				tail)))
	    (split (cons cmd cmds)
		   rest))))))

(define cmd-splitters
  `(("PRIVMSG" . ,privmsg-splitter)))

(define (split-cmd cmd)
  (let ((len (string-length (cmd->string cmd))))
    (cond
     ((< len 512)
      (list cmd))
     ((assoc (cmd-name cmd) cmd-splitters)
      ((cdr (assoc (cmd-name cmd) cmd-splitters)) cmd))
     (else
      (list (make-cmd #f "PRIVMSG" (cmd-args cmd)
		      (substring (cmd-tail cmd) (- 512 (cmd-overhead cmd)))))))))
	 
   
(define (send-cmd con cmd)
  (let ((io-port (hash-ref con 'io-port)))
    (map (λ (cmd)
	   (log 'debug-as-fuck (format #f "Sending(~a) ~a" (string-length (cmd->string cmd))
			       cmd))
	   (display (cmd->string cmd) io-port)
	   (newline io-port))
	 (split-cmd cmd))))

(define (register con nick mode user-name real-name)
  (let ((io-port (hash-ref con 'io-port)))
    (hash-set! con 'nick nick)
    (hash-set! con 'user-name user-name)
    (hash-set! con 'real-name real-name)
    (send-cmd con (make-cmd #f "NICK" '() nick))
    (send-cmd con (make-cmd #f "USER" (list user-name "1" "0") real-name))))

(define (join con chan)
  (send-cmd con (make-cmd #f "JOIN" (list chan) #f)))

(define (part con chan msg)
  (send-cmd con (make-cmd #f "PART" (list chan) msg)))

(define (nick con new-nick)
  (send-cmd con (make-cmd #f "NICK" '() new-nick))
  (hash-set! con 'nick new-nick))

(define (say con chan msg)
  (send-cmd con (make-cmd #f "PRIVMSG" (list chan) msg)))

(define (raw-handler con line)
  (let ((cmd (decode-cmd line)))
    (if cmd
	(begin
	  (log 'debug (format #f "Received cmd: ~a" cmd))
	  (run-event (string->symbol
		    (string-append "irc-command-"
				   (string-downcase (cmd-name cmd))))
		     (list con cmd)))	
	(begin
	  (log 'debug (format #f "Failed to parse cmd: ~a" line))
	  #f))))

(define (ping-handler con cmd)
  (let ((ping-data (cmd-tail cmd)))
    (send-cmd con (make-cmd #f "PONG" '() ping-data))))

(define hooks
  `((irc-raw . ,raw-handler)
    (irc-command-ping . ,ping-handler)))

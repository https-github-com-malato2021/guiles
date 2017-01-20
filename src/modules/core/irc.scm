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
(define (send-cmd cmd port)
  (display (cmd->string cmd) port)
  (newline port))

(define (register sock nick mode user-name real-name)
  (send-cmd (make-cmd #f "NICK" '() nick) sock)
  (send-cmd (make-cmd #f "USER" (list user-name "1" "0") real-name) sock))

(define (join con chan)
  (send-cmd (make-cmd #f "JOIN" (list chan) #f) (caddr con)))

(define (part con chan msg)
  (send-cmd (make-cmd #f "PART" (list chan) msg) (caddr con)))

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
  (let* ((ping-data (cmd-tail cmd))
	 (sock (caddr con)))
    (send-cmd (make-cmd #f "PONG" '() ping-data) sock)))

(define hooks
  `((irc-raw . ,raw-handler)
    (irc-command-ping . ,ping-handler)))

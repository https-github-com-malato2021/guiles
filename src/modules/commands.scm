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

(define-module (commands))

(use-modules (core irc)
	     (core hooks)
	     (core log)
	     (srfi srfi-11))

(define *command-str* "`")

(define (extract-cmd msg)
  (let* ((space-index (string-index msg #\space))
	 (has-args (and space-index
			(> (string-length msg) (+ 2 space-index))))
	 (cmd (substring msg 1 space-index)))
    (values cmd (if has-args
		    (substring msg (1+ space-index))
		    #f))))

(define (msg-handler con cmd)
  (let*-values (((msg) (cmd-tail cmd))
		((command args) (extract-cmd msg)))
    (log 'debug (format #f "Handling ~a ~a" msg (string-prefix? *command-str* msg)))
    (if (and msg (string-prefix? *command-str* msg))
	  (run-event (string->symbol (string-append "command-" command))
		     (list con
			   (cmd-origin cmd)
			   (car (cmd-args cmd))
			   args))
	  #f)))

(define (help-handler con usr chan args)
  (log 'debug (format #f "Calling help handler with ~a ~a ~a" usr chan args))
  (send-cmd (make-cmd #f "PRIVMSG" (list chan)
		      (format #f "~a: ko smaji" (user-name usr)))
	    (caddr con)))

(define hooks
  `((irc-command-privmsg . ,msg-handler)
    (command-help . ,help-handler)))

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

(define-module (commands init))

(use-modules (core irc)
	     (core hooks)
	     (core log)
	     (srfi srfi-11))

(export set-cmd-str)

(define *command-str* "`")

(define (set-cmd-str str)
  (when (string? str)
	(set! *command-str* str)))

(define (is-cmd? str)
  (and str
       (string-prefix? *command-str* str)
       (< 1 (string-length str))))

(define (parse-cmd str)
  (let* ((parts (string-split (string-trim-right (substring str (string-length *command-str*)))  #\space))
	 (cmd (car parts))
	 (args (cdr parts)))
    (values cmd args)))

(define (msg-handler con cmd-struct)
  (when (is-cmd? (cmd-tail cmd-struct))
	(let-values (((command args) (parse-cmd (cmd-tail cmd-struct))))
	  (run-event (string->symbol (string-append "command-" command))
		     (list con ; connection
			   (cmd-origin cmd-struct) ; sender
			   (car (cmd-args cmd-struct)) ; channel
			   args))))) ; arguments

(define (help-handler con usr chan args)
  (log 'debug (format #f "Calling help handler with ~a ~a ~a" usr chan args))
  (send-cmd con (make-cmd #f "PRIVMSG" (list chan)
			  (format #f "~a: ko smaji" (user-name usr)))))

(define hooks
  `((irc-command-privmsg . ,msg-handler)
    (command-help . ,help-handler)))

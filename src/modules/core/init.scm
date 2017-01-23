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

(define-module (core init))

(use-modules (core util)
	     (core hooks)
	     (core net)
	     (core irc)
	     (core log)
	     (core modules)
	     (ice-9 rdelim))

(define-public *connection* #f)

;; Starts the bot and begins sending irc-raw events
;; for each line received
(define-public (start-bot config)
  (let* ((host (assoc-def 'host config "127.0.0.1"))
	 (port (assoc-def 'port config 6667))
	 (nick (assoc-def 'nick config "guiles"))
	 (ssl (assoc-def 'ssl config #f))
	 (user-name (assoc-def 'user-name config "guiles"))
	 (real-name (assoc-def 'real-name config "Guiles"))
	 (modules (assoc-def 'modules config '())))
    (when (or (not host) (not port))
       (begin
	 (when (not host)
	   (log 'error "Config does not contain host definition."))
	 (when (not port)
	   (log 'error "Config does not contain port definition."))
	 (exit-with-error "Problem with config, check logs.")))
    (set! *connection* (make-connection host port ssl))
    (map register-module modules)
    (register *connection* nick 2 user-name real-name)
    (let ((io-port (hash-ref *connection* 'io-port)))
      (let read-loop ((line (read-line io-port)))
	(when (not (eof-object? line))
	      (begin
		(log 'debug (string-append "LINE:" line))
		(run-event 'irc-raw (list *connection* line))
		(read-loop (read-line io-port))))))))


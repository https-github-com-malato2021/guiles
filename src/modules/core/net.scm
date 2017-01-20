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

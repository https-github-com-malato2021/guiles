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

(define-module (web html-fetcher))

(use-modules (web client)
	     (web response)
	     (core hooks)
	     (srfi srfi-11)
             (ice-9 receive)
	     (core log)
	     (ice-9 pretty-print)
	     (web uri)
	     (gnutls))

;;; TODO: Convert threading model over to use a thread pool (or just
;;;       a separate thread for running tasks).

(define (uri-handler con usr chan uri)
  (log 'debug (format #f "Getting HEAD from ~a" uri))
  (let ((head (http-head uri)))
    (cond
     ((equal? (car (response-content-type head)) 'text/html)			 
      (receive (resp body)
          (http-get uri)
        (run-event 'html-str (list con
    				 usr
    				 chan
    				 uri
                                 body)))))))

(define hooks
  `((uri . ,uri-handler)))

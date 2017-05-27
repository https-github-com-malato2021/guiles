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

(define-module (core log))

(export log add-logger remove-logger)

(define (default-logger level msg)
  (display (format #f
		   "~a [~a] ~a~%"
		   (strftime "%F %T" (localtime (current-time)))
		   level
		   msg)
	   (current-error-port)))

(define (add-logger name logger)
  (if (assoc name loggers)
      #f
      (set! loggers (cons (cons name logger) loggers))))

(define (remove-logger name)
  (set! loggers (filter (λ (log-pair)
			  (not (equal? name (car log-pair))))
			loggers)))

(define loggers `((default-logger . ,default-logger)))

(define (log level msg)
  (map (λ (logger)
	 (catch #t
	   (λ ()
	     (logger level msg))
	   (λ (key . args)
	     (display (format #f ">>>> ~a | ~a <<<<" key args)
		      (current-error-port)))))
       (map cdr loggers)))


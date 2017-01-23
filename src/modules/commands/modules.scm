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

(define-module (commands modules))

(use-modules (core modules)
	     (core irc))

(define (handle-register con usr chan args)
  (when (> (length args) 0)
    (let* ((module (string->symbol (string-join args)))
	   (success? (register-module module)))
      (say con
	   chan
	   (format #f (if success?
			  "~a: la ~a cu pagbu"
			  "~a: o'anai la ~a cu srera")
		   (user-nick usr)
		   module)))))
	       
(define (handle-deregister con usr chan args)
  (when (> (length args) 0)
    (let* ((module (string->symbol (string-join args)))
	   (success? (deregister-module module)))
      (say con
	   chan
	   (format #f (if success?
			  "~a: la ~a cu nai pagbu"
			  "~a: o'anai la ~a cu srera")
		   (user-nick usr)
		   module)))))

(define hooks
  `((command-register . ,handle-register)
    (command-deregister . ,handle-deregister)))

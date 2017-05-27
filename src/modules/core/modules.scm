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

(define-module (core modules))

(use-modules (core hooks)
	     (core log))

(export register-module
	deregister-module
        reload-module
	*modules-loaded*)

(define *modules-loaded* '())

(define (register-module module)
  (let ((loaded? (member module *modules-loaded*)))
    (if loaded?
	#f
	(begin
	  (set! *modules-loaded*
		(cons module *modules-loaded*))
	  (run-event 'module-register (list module))
	  #t))))

(define (deregister-module module)
  (let ((loaded? (member module *modules-loaded*)))
    (if loaded?
	(begin
	  (set! *modules-loaded*
		(filter (λ (mod)
			  (log 'debug (format #f "mod:~a (module: ~a)" mod module))
			  (not (equal? mod module)))
			*modules-loaded*))
	  (run-event 'module-deregister (list module))
	  #t)
	#f)))

(define (reload-module module)
  (let ((loaded? (member module *modules-loaded*)))
    (if loaded?
        (begin (reload-module (resolve-module module)) #t)
        #f)))

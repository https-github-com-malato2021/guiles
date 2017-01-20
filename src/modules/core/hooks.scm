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

(define-module (core hooks))

(use-modules (ice-9 threads)
	     (core log))

(define hook-condition (make-condition-variable))
(define hook-mutex (make-mutex 'allow-external-unlock))

(define registered-hooks '())

(define (add-hook module hook handler)
  (when (lock-mutex hook-mutex)
    (begin
      (set! registered-hooks (cons (list module hook handler) registered-hooks))
      (unlock-mutex hook-mutex))))

(define (remove-hook hook handler)
  (when (lock-mutex hook-mutex)
    (begin
      (set! registered-hooks (delete (cons hook handler) registered-hooks))
      (unlock-mutex hook-mutex))))
      
(define (find-hooks hook)
  (lock-mutex hook-mutex)
  (let ((found-hooks (filter (λ (hook-list)
			       (log 'debug (format #f "hook <~a> matches <~a>? ~a"
						   hook
						   hook-list
						   (equal? (cadr hook-list) hook)))
			       (equal? (cadr hook-list)
				       hook))
			     registered-hooks)))
    (unlock-mutex hook-mutex)
    found-hooks))

(define (run-event hook args)
  (let ((matching-hooks (find-hooks hook)))
    (log 'debug (format #f "Running hook <~a> args <~a> [hooks:<~a>]" hook args matching-hooks))
    (catch #t
       (λ ()
	 (par-map
	  (λ (hook-list)
	    (log 'debug (format #f "Hook list: ~a" hook-list))
	    (log 'debug (format #f "Running: ~a : ~a" (caddr hook-list)
				   	     	      args))
	    (apply (caddr hook-list) args))
	  matching-hooks))
       (λ (key . args)
	 (log 'error (format #f "Error running hook '~a': ~a" hook args))))))

(define (handle-register module)
  (let* ((mod (resolve-module module))
	 (maybe-mod-hooks (module-variable mod 'hooks)))			
    (log 'debug (format #f "Registering mod ~a, hooks: ~a" mod maybe-mod-hooks))
    (when maybe-mod-hooks
	  (set! registered-hooks (append registered-hooks
			      (map (λ (hook-pair)
				     (list module
					   (car hook-pair)
					   (cdr hook-pair)))
				   (variable-ref maybe-mod-hooks)))))))

(define (handle-deregister module)
  (set! registered-hooks (filter (λ (hook)
				   (not (equal? (car hook) module)))
				 registered-hooks)))	

(define hooks
  `((module-register . ,handle-register)
    (module-deregister . ,handle-deregister)))

(handle-register '(core hooks))

(export add-hook
	remove-hook
	find-hooks
	run-event
	hooks)

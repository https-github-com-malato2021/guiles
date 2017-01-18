(define-module (core hooks))

(use-modules (ice-9 threads)
	     (core log))

(define hook-condition (make-condition-variable))
(define hook-mutex (make-mutex 'allow-external-unlock))

(define hooks '())

(define (add-hook hook handler)
  (when (lock-mutex hook-mutex)
    (begin
      (set! hooks (cons (cons hook handler) hooks))
      (unlock-mutex hook-mutex))))

(define (remove-hook hook handler)
  (when (lock-mutex hook-mutex)
    (begin
      (set! hooks (delete (cons hook handler) hooks))
      (unlock-mutex hook-mutex))))
      
(define (find-hooks hook)
  (lock-mutex hook-mutex)
  (let ((found-hooks (filter (λ (hook-pair)
			       (equal? (car hook-pair)
				       hook))
			     hooks)))
    (unlock-mutex hook-mutex)
    found-hooks))

(define (run-event con hook args)
  (let ((hooks (find-hooks hook)))
    (catch #t
       (λ ()
	 (par-map
	  (λ (hook-pair)
	    ((cdr hook-pair) con args))
	  hooks))
       (λ (key . args)
	 (log 'error (format #f "Error running hook '~a': ~a" hook args))))))

(export add-hook
	remove-hook
	find-hooks
	run-event)

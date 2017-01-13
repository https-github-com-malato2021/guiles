#!/usr/bin/guile \
-e main -s
!#

(add-to-load-path (string-append (dirname (current-filename)) "/modules"))
(add-to-load-path (dirname (current-filename)))

(use-modules (ice-9 getopt-long)
	     (system repl server)
             (core init))

(define version '(0 0 1))

(define (display-err msg)
  (let ((out (current-error-port)))
    (display msg out)
    (newline out)))

(define (exit-err msg)
  (display-err msg)
  (exit 1))

(define (print-version)
  (display-err
   (format #f "guiles version ~A.~A.~A"
              (car version)
              (cadr version)
              (caddr version))))

(define (print-help)
  (display-err "Usage: dlisper [OPTS]
      Options:
        -v,      --version           Print version information.
        -c file, --config-file file  File containing configuration information.
	-d,	 --debug       	     Enables the debug REPL. Use with care.
  " out))

(define (main args)
  (let* ((options-spec `((version (single-char #\v) (value #f))
                         (help (value #f))
			 (config-file (single-char #\c) (value #t))
			 (debug (single-char #\d) (value #f))))
         (options (getopt-long args options-spec))
         (version-wanted (option-ref options 'version #f))
         (help-wanted (option-ref options 'help #f))
	 (debug (option-ref options 'debug #f))
         (config-file (option-ref options 'config-file "config.scm"))
	 (config (load config-file)))
    (when debug
	  (spawn-server (make-unix-domain-server-socket #:path "/tmp/guiles.sock")))
    (cond
     ((or version-wanted help-wanted)
        (begin
          (when version-wanted (print-version))
          (when help-wanted (print-help))))
     (config
      (start-bot config))
     (else
      (exit-err "No host supplied")))))

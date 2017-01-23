#!/usr/bin/guile \
-e main -s
!#

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
			 (debug (single-char #\d) (value #t))))
         (options (getopt-long args options-spec))
         (version-wanted (option-ref options 'version #f))
         (help-wanted (option-ref options 'help #f))
	 (socket-path (option-ref options 'debug #f))
         (config-file (option-ref options 'config-file "config.scm"))
	 (config (load config-file)))
    (when socket-path
	  (spawn-server (make-unix-domain-server-socket #:path socket-path)))
    (cond
     ((or version-wanted help-wanted)
        (begin
          (when version-wanted (print-version))
          (when help-wanted (print-help))))
     (config
      (start-bot config))
     (else
      (exit-err "No host supplied")))))

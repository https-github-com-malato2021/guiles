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

(define-module (commands cowsay))

(use-modules (ice-9 popen)
	     (ice-9 rdelim)
	     (ice-9 ftw)
	     (core irc))

(export handle-cowsay)

(define (handle-cowsay con usr chan args)
  (when (> (length args) 1)
    (let* ((cowfile (car args))
	   (text (string-join (cdr args)))
	   (pipe (open-pipe* OPEN_READ "cowsay"
			     	       "-f"
				       cowfile
				       text)))
      (let outloop ((line (read-line pipe)))
	(when (not (eof-object? line))
	      (say con chan line)
	      (outloop (read-line pipe)))))))

(define (handle-cows con usr chan args)
  (when (= (length args) 0)
    (let ((cow-files (scandir "/usr/share/cows"
			      (λ (name)
				(not (string-prefix? "." name))))))
      (say con chan (format #f "~a: bakni: ~a"
		       (user-nick usr)
		       (string-join
			(map (λ (file)
			       (substring file 0 (- (string-length file) 4)))
			     cow-files)))))))
	 

(define hooks
  `((command-cowsay . ,handle-cowsay)
    (command-cows . ,handle-cows)))

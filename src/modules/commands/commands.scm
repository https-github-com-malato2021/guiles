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

(define-module (commands commands))

(use-modules (core irc)
	     (core hooks))

(define (handle-cmds con usr chan args)
  (when (= (length args) 0)
    (let* ((modules
	    (filter (λ (mod)
		      (string-prefix? "command-" (symbol->string (car mod))))
		    registered-hooks))
	   (command-names (map (λ (mod)
				 (substring (string-join (map symbol->string (car mod)))
					    8))
			       modules)))
      (say con chan (format #f "~a: ~a" (user-nick usr)
			    (string-join command-names))))))

(define hooks
  `((command-cmds . ,handle-cmds)))

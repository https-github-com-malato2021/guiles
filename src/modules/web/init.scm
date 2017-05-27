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

(define-module (web init))

(use-modules (ice-9 regex)
	     (core hooks)
	     (core irc)
	     (web uri))

(define uri-regex (make-regexp "https?://[^ /]+\\.[^ /]+(/[^/ ]+/?)*" regexp/icase))

(define (uri-handler con cmd)
  (let ((matches (filter
		  (λ (str)
		    (string->uri str))
		  (map match:substring (list-matches uri-regex (cmd-tail cmd))))))
    (when (not (null? matches))
      (map (λ (uri)
	     (run-event 'uri (list con
				   (cmd-origin cmd)
				   (car (cmd-args cmd))
				   uri)))
	   matches))))

(define hooks
  `((irc-command-privmsg . ,uri-handler)))

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

(define-module (web title-scanner))

(use-modules (ice-9 regex)
	     (core irc))

(define title-regex (make-regexp "title[^>]*>([^<]+)<[^>]*/title" regexp/icase))

(define (handle-html con usr chan uri html)
  (let ((matches (list-matches title-regex html)))
    (when (and (>= 1 (length matches)))
	  (say con chan (format #f "~a: ~a"
				   (user-nick usr)
				   (substring html
					      (match:start (car matches) 1)
					      (match:end (car matches) 1)))))))

(define hooks
  `((html-str . ,handle-html)))

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

(define-module (commands command-str))

(use-modules (core irc)
	     (commands))

(define (set-cmd-str-handler con usr chan args)
  (if (not (= (length args) 1))
      (say con chan (format "~a: nitcu pa lo mlatu" (user-nick usr)))
      (begin
	(set-cmd-str (car args))
	(say con chan (format "~a: snada" (user-nick usr))))))

(define hooks
  `((command-cmd-str . ,set-cmd-str-handler)))

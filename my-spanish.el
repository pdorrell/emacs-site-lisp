;; Spanish accent toggling - easy typing of Spanish accented characters

;; Copyright (C) 1999  Philip Dorrell
     
;;     This program is free software; you can redistribute it and/or
;;     modify it under the terms of the GNU General Public License
;;     as published by the Free Software Foundation; either version 2
;;     of the License, or (at your option) any later version.
     
;;     This program is distributed in the hope that it will be useful,
;;     but WITHOUT ANY WARRANTY; without even the implied warranty of
;;     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;     GNU General Public License for more details.
     
;;     A copy of the GNU General Public License is included with the
;;     GNU Emacs distribution.

;; Email : pdorrell@.com      (add "thinkinghard" after "@" and before ".com").
;; (The previous line is to confuse spambots trying to read this page.)
;; Web : www.thinkinghard.com

;; Standard GNU emacs contains two main functions that help with
;; editing accented non-English languages -

;; standard-european, which is used here, to display higher characters
;; as accented characters

;; iso-accents-mode, which allows one to use keys `'"^/~ as special
;; accent keys to compose accented characters

;; This file defines spanish-toggle-accent which toggles between accented and 
;; non-accented characters (or rotates around the choices if there is more than
;; one accented character for a given non-accented character).

;; This file is encoded in UTF-8.

(require 'cl)
;-----------------------------------------------------------------
;; These next two are utility routines that help you to see
;; which character is which. They are not needed to make the
;; Spanish mode work.

(defun octal-digits (n)
  "Return NUMBER as a string of octal digits"
  (let ( (n1 n) (x 0) (digits nil) (len 0))
    (while (> n1 0)
      (setq n2 (/ n1 8))
      (setq digit (- n1 (* 8 n2)))
      (setq digits (cons digit digits))
      (setq x (+ (* x 10) digit))
      (setq len (1+ len))
      (setq n1 n2) )
    (setq string (make-string len 0))
    (dotimes (i len)
      (aset string i (+ ?0 (nth i digits))) )
    string) )

(defun show-char-codes ()
  "Show all characters and codes in a buffer"
  (interactive)
  (let ( (output (get-buffer-create "*characters*")) )
    (switch-to-buffer-other-window output)
    (delete-region (point-min) (point-max))
    (standard-display-european 1)
    (dotimes (i 32)
      (dotimes (j 8)
	(let ( (n (+ (* i 8) j)) )
	  (insert (octal-digits n) ": " n " ") ) )
      (insert "\n") ) ) )

;-----------------------------------------------------------------
(defun make-toggle-array (&rest strings)
  "Make a lookup array to toggle characters around the STRINGS"
  (let ( (toggle-array (make-vector 65536 nil)) )
    (dolist (string strings)
      (let ( (len (length string)) j)
	(dotimes (i len)
	  (setq j (1+ i))
	  (if (>= j len) (setq j 0))
	  (aset toggle-array (aref string i) (aref string j)) ) ) )
    toggle-array) )

(defvar spanish-accents-toggler
  (make-toggle-array
   "AÁ" "aáª" "EÉ" "eé" "IÍ" "ií" "NÑ" "nñ" "OÓ" "oóº" "UÚÜ" "uúü" "?¿" "!¡" "$€" "\"«»" "'‹›")
  "Array to toggle normal ASCII characters with most
common Spanish variants")

(defun toggle-character (toggle-array)
  "Toggle character just before cursor according to
toggle array"
  (let ( (ch (char-after (1- (point)))) new)
    (if ch
	(progn
	  (setq new (aref toggle-array ch))
	  (if new
	      (progn 
		(delete-backward-char 1)
		(insert (make-string 1 new)) )
	    (message 
	     (concat "No toggle value for character " (make-string 1 ch))) ) )
      (message "No character to toggle") ) ) )

(defun spanish-toggle-accent()
  "Toggle Spanish accent or other special character"
  (interactive)
  (toggle-character spanish-accents-toggler) )

(global-set-key [f8] 'spanish-toggle-accent)


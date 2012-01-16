;;; Copyright Philip Dorrell 2012

;;; This file supplies an GNU Emacs editing mode for
;;; Propositional source files.

;;; Author Philip Dorrell. Email: http://www.1729.com/email.html

;;; Version Date: 16 January 2012

;;; Permission is hereby granted, free of charge, to any person obtaining a copy
;;; of this software and associated documentation files (the "Software"), to deal
;;; in the Software without restriction, including without limitation the rights
;;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;;; copies of the Software, and to permit persons to whom the Software is
;;; furnished to do so, subject to the following conditions:
;;; 
;;; The above copyright notice and this permission notice shall be included in
;;; all copies or substantial portions of the Software.
;;; 
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
;;; THE SOFTWARE.

(defvar propositional-mode-hook nil "Hook function for Propositional mode")

(define-derived-mode propositional-mode fundamental-mode "Propositional"
  "Major mode for editing Propositional files."
;;  (set (make-local-variable 'font-lock-defaults) '(propositional-font-lock-keywords))
;;  (set (make-local-variable 'indent-line-function) 'propositional-indent-line)
;;  (set (make-local-variable 'parse-sexp-ignore-comments) t ) 
  )


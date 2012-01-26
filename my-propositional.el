;;; This is some sample code of how to use propositional-mode.el

;;; Author Philip Dorrell. Email: http://www.1729.com/email.html

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

(autoload 'propositional-mode "propositional-mode")

(defun set-extension-mode (extension mode)
  (setq auto-mode-alist
	(cons (cons (concat "\\" extension "\\'") mode)
	      auto-mode-alist) ) )

(set-extension-mode ".propositional" 'propositional-mode)

(add-hook 'propositional-mode-hook 'my-propositional-hook)

(defun propositional-return-and-indent()
  (interactive)
  (newline) (indent-for-tab-command) )

(defun propolize-run-file(file)
  (interactive)
  (save-this-buffer-and-others)
  (ruby-run-file *propolize-ruby-script* file ".." "../")  )

(defun my-propositional-hook ()
  (setq run-file-function #'propolize-run-file)
  )


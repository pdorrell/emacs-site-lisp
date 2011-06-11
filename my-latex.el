(require 'tex-mode)

(setq latex-executable (concat *latex-bin-directory* "latex.exe"))
(setq makeindex-executable (concat *latex-bin-directory* "makeindex.exe"))
(setq yap-executable (concat *latex-bin-directory* "yap.exe"))
(setq dvips-executable (concat *latex-bin-directory* "dvips.exe"))

(defvar dvips-flag t "Run dvips instead of yap")

;;(defvar ps-page-size "a4")
(defvar ps-page-size "sixByNine")
;;(defvar ps-page-size "sixByNineWidened")
;;(defvar ps-page-size "a5")
;; (setq ps-page-size "comic")

(add-hook 'tex-mode-hook 'tex-mode-hook-function)

;(list-colors-display)
;(list-faces-display)

(def-bg-face tex-newterm-face "light sky blue")
(def-bg-face tex-newtermi-face "cyan")
(def-bg-face tex-index-face "plum1")
(def-bg-face tex-indexed-face "khaki1")
(def-bg-face tex-emph-face "yellow2")
(def-bg-face tex-heading-face "yellow")

(regexp-opt '("newterm") t)

(setq my-extra-tex-font-lock-keywords
      (let* (
	     ;; standard bits
	     (slash "\\\\")
	     (opt " *\\(\\[[^]]*\\] *\\)*")
	     (simple-arg "{\\([^}]*\\)}")
	     (arg "{\\(\\(?:[^{}\\]+\\|\\\\.\\|{[^}]*}\\)+\\)") )
	(list 
	 ;; my extras
	 (list (concat slash "\\(newterm\\)" " *" arg) 2 'tex-newterm-face)
	 (list (concat slash "\\(newtermi\\)" " *" arg) 2 'tex-newtermi-face)
	 (list (concat slash "\\(indexed\\)" " *" arg) 2 'tex-indexed-face)
	 (list (concat slash "\\(authori\\)" " *" "\\(" simple-arg simple-arg "\\)") 2 'tex-indexed-face)
	 (list (concat slash "\\(emph\\)" " *" arg) 2 'tex-emph-face)
	 ) ) )

(defconst tex-font-lock-keywords-4
  (append my-extra-tex-font-lock-keywords tex-font-lock-keywords-3) )

(defun tex-run-main-file ()
  (interactive)
  (save-this-buffer-and-others)
  (find-file (project-file :latex-main-file))
  (tex-and-yap-file) )

(defun tex-mode-hook-function ()
  (local-set-key [?\M-N] 'tex-and-yap-file)
  (local-set-key [C-M-S-kp-right] 'shift-close-bracket-one-word-right)
  (local-set-key [C-M-S-kp-left] 'shift-close-bracket-one-word-left)
  (local-set-key [?\M-i] 'tex-insert-index)
  (local-set-key [C-M-f6] 'tex-run-main-file)
  (setq expansion-key 'latex-expansion-key)
  (setq font-lock-defaults
	'(tex-font-lock-keywords-4
	  nil nil ((?$ . "\"")) nil
	  ;; Who ever uses that anyway ???
	  (font-lock-mark-block-function . mark-paragraph)
	  (font-lock-syntactic-face-function
	   . tex-font-lock-syntactic-face-function)))
  (font-lock-mode 1)
  )

(defun tex-insert-index ()
  (interactive)
  (insert "\\index{}")
  (forward-char -1) )

(defun tex-and-yap-file ()
  (interactive)
  (save-this-buffer-and-others)
  (let* ( (base-file-name (file-name-minus-extension (buffer-name)))
	  (chapter-file-name (concat base-file-name ".chapter")) )
    (if (file-exists-p (concat default-directory chapter-file-name ".tex"))
	(tex-and-yap default-directory chapter-file-name dvips-flag)
      (tex-and-yap default-directory base-file-name dvips-flag) ) ) )

(defun tex-and-yap (directory base-file-name dvips)
  (let* ( (tex-file (concat directory base-file-name ".tex"))
	  (output-directory (concat directory "output/"))
	  (dvi-file (concat output-directory base-file-name ".dvi"))
	  (ps-file (concat output-directory base-file-name ".ps"))
	  (aux-file (concat output-directory base-file-name ".aux"))
	  (log-file (concat output-directory base-file-name ".log")) tex-buffer
	  (index-file (concat output-directory base-file-name ".idx")) )
    (dolist (file (list dvi-file))
      (if (file-exists-p file)
	  (delete-file file) ) )
    (setq tex-buffer (get-buffer-create "*tex*"))
    (with-current-buffer tex-buffer (erase-buffer))
    (call-process latex-executable nil "*tex*" t "-c-style-errors" "-aux-directory=output" "-output-directory=output" "-halt-on-error" "-interaction=errorstopmode" tex-file)
    (if (file-exists-p index-file)
	(call-process makeindex-executable nil "*tex*" t index-file))
    (switch-to-buffer-other-window "*tex*")
    (if (file-exists-p dvi-file)
	(if dvips
	    (progn
	      (call-process dvips-executable nil "*tex*" t "-t" ps-page-size dvi-file "-o" ps-file)
	      (run-file ps-file) )
	  (progn
	    (message "Yapping %s" dvi-file)
	    (start-process "yap" "*tex*" yap-executable dvi-file) ) )
      (message "DVI file %s not created" dvi-file) ) ) )

(defun shift-close-bracket-one-word-right ()
  (interactive)
  (if (looking-at "}")
      (progn
	(delete-char 1)
	(forward-sexp)
	(insert "}")
	(forward-char -1) )
    (message "Not looking at }") ) )

(defun shift-close-bracket-one-word-left ()
  (interactive)
  (if (looking-at "}")
      (progn
	(delete-char 1)
	(backward-sexp)
	(backward-char 1)
	(insert "}")
	(forward-char -1) )
    (message "Not looking at }") ) )

(defvar latex-word-table (make-alpha-table letters-digits-string))

(defun def-latex-abbrev (abbrev expansion)
  "Define ABBREV to have EXPANSION in latex mode"
  (set-abbrev abbrev expansion 'latex-expansion-key) )

(def-latex-abbrev "au" '("\\authori{" mark "}{}" goto-mark))

(def-latex-abbrev "ch" '("\\chapter{" mark "}" goto-mark))
(def-latex-abbrev "s" '("\\section{" mark "}" goto-mark))
(def-latex-abbrev "ss" '("\\subsection{" mark "}" goto-mark))

(def-latex-abbrev "bq" '("\\begin{quote}" mark "\\end{quote}" goto-mark))

(def-latex-abbrev "b" '("{\\bf " mark "}" goto-mark))
(def-latex-abbrev "mb" '("{\\mathbf " mark "}" goto-mark))

(def-latex-abbrev "l" '("\\begin{itemize}" return "  \\item " mark return "\\end{itemize}" goto-mark))

(def-latex-abbrev "sl" '("%=================================================================================="
			 return "\\begin{slide}{" mark "}" return return "\\end{slide}" return goto-mark))

(def-latex-abbrev "li" '("  \\item "))

(def-latex-abbrev "em" '("\\emph{" mark "}" goto-mark))

(def-latex-abbrev "rt" '("\\reftitle{" mark "}" goto-mark))

(def-latex-abbrev "ar" '("\\article{" mark "}" goto-mark))

(def-latex-abbrev "fn" '("\\footnote{" mark "}" goto-mark))

(def-latex-abbrev "cr" '("Chapter~\\ref{" mark "}" goto-mark))
(def-latex-abbrev "fr" '("Figure~\\ref{" mark "}" goto-mark))
(def-latex-abbrev "sr" '("Section~\\ref{" mark "}" goto-mark))
(def-latex-abbrev "ssr" '("Subsection~\\ref{" mark "}" goto-mark))

(def-latex-abbrev "ra" '("\\rightarrow "))

(def-latex-abbrev "ba" '("\\begin{align*}" return "   " mark return "\\end{align*}" return goto-mark))

(def-latex-abbrev "nt" '("\\newterm{" mark "}" goto-mark))
(def-latex-abbrev "nti" '("\\newtermi{" mark "}" goto-mark))

(def-latex-abbrev "chi" '("\\chapterintro{" return mark return "}" goto-mark))

(def-latex-abbrev "in" '("\\index{" mark "}" goto-mark))

(def-latex-abbrev "lb" '("\\label{" mark "}" goto-mark))

(def-latex-abbrev "rf" '("~\\ref{" mark "}" goto-mark))

(def-latex-abbrev "i" '("\\indexed{" mark "}" goto-mark))
(def-latex-abbrev "ig" '("\\includegraphics{" mark "}" goto-mark))

(def-latex-abbrev "pic" '("\\pictureplaceholder{" mark "}" goto-mark))

(def-latex-abbrev "fig" '("\\begin{figure}[ht]" return " \\centering" return 
			  " \\includegraphics{diagrams/" mark ".eps}" return
			  " \\caption{}" return
			  " \\label{figure-}" return
			  "\\end{figure}" return goto-mark) )

(def-latex-abbrev "n" '("\\nonteaser{"))

(set-extension-mode ".dtx" 'tex-mode)

(defun add-d-prefix (param)
  (concat "-d" param) )

(defvar make-pdf-params
  '(
    "PDFWRDEBUG"
    "NOEPS"
    "CompatibilityLevel=1.3"
    "PDFSETTINGS=/prepress"
    "DoThumbnails=false"
    "AutoRotatePages=/None"
    "Binding=/Left"

    "CompressPages=true"

    "DownsampleColorImages=true"
    "ColorImageDownsampleType=/Bicubic"
    "ColorImageResolution=300"
    "ColorImageDownsampleThreshold=1.5"
    
    "DownsampleGrayImages=true"
    "GrayImageDownsampleType=/Bicubic"
    "GrayImageResolution=300"
    "GrayImageDownsampleThreshold=1.5"
    
    "DownsampleMonoImages=true"
    "MonoImageDownsampleType=/Bicubic"
    "MonoImageResolution=1200"
    "MonoImageDownsampleThreshold=1.5"
    "MonoImageFilter=/CCITTFaxEncode"

    "EmbedAllFonts=true"
    "MaxSubsetPct=100"
    "SubsetFonts=true"
    
    "ColorConversionStrategy=/LeaveColorUnchanged"
    "PreserveHalftoneInfo=false"
    "PreserveOverprintSettings=true"
    "TransferFunctionInfo=/Preserve"
    "UCRandBGInfo=/Preserve"
    
    "ParseDSCCommentsForDocInfo=true"
    "ParseDSCComments=true"
    "EmitDSCWarnings=false"
    "CreateJobTicket=true"
    "PreserveEPSInfo=true"
    "AutoPositionEPSFiles=false"
    "PreserveCopyPage=true"
    "UsePrologue=false"
    "PreserveOPIComments"
    "LockDistillerParams=false"
    ))

(mapcar #'add-d-prefix make-pdf-params)

(defun make-pdf ()
  (interactive)
  (let* ( (ps-file (windowize-filename (expand-file-name (filename-at-point))) )
	  (out-file (concat ps-file ".pdf")) result )
    (delete-file-if-exists out-file)
    (message "Converting %s to PDF file ..." ps-file)
    (setq make-pdf-buffer (get-buffer-create "*make-pdf*"))
    (with-current-buffer make-pdf-buffer (erase-buffer))
    (setq command-line `("gswin32c" nil "*make-pdf*" t
		    "-dBATCH" "-dNOPAUSE" "-sDEVICE=pdfwrite" "-r2400"
		    ,@(mapcar #'add-d-prefix make-pdf-params)
		    ,(concat "-sOutputFile=" out-file)
		    ,ps-file) )
    (save-window-excursion
      (switch-to-buffer-other-window "*make-pdf*")
      (insert (format "%S" command-line)) )
    (setq result (apply #'call-process command-line) )
    (switch-to-buffer-other-window "*make-pdf*")
    (message "Finished result=%s" result)
    (if (eql result 0)
	(run-file out-file) )
    ) )

(global-set-key [C-f12] 'make-pdf)

;;; magit-section-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "magit-section" "magit-section.el" (0 0 0 0))
;;; Generated autoloads from magit-section.el

(autoload 'magit--handle-bookmark "magit-section" "\
Open a bookmark created by `magit--make-bookmark'.

Call the generic function `magit-bookmark-get-buffer-create' to get
the appropriate buffer without displaying it.

Then call the `magit-*-setup-buffer' function of the the major-mode
with the variables' values as arguments, which were recorded by
`magit--make-bookmark'.

\(fn BOOKMARK)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit-section" '("context-menu-region" "isearch-clean-overlays" "magit-")))

;;;***

;;;### (autoloads nil nil ("magit-section-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; magit-section-autoloads.el ends here

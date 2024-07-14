;;; lsp-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "lsp-actionscript" "lsp-actionscript.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from lsp-actionscript.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-actionscript" '("lsp-actionscript-")))

;;;***

;;;### (autoloads nil "lsp-ada" "lsp-ada.el" (0 0 0 0))
;;; Generated autoloads from lsp-ada.el
(put 'lsp-ada-project-file 'safe-local-variable 'stringp)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-ada" '("lsp-ada-")))

;;;***

;;;### (autoloads nil "lsp-angular" "lsp-angular.el" (0 0 0 0))
;;; Generated autoloads from lsp-angular.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-angular" '("lsp-client")))

;;;***

;;;### (autoloads nil "lsp-ansible" "lsp-ansible.el" (0 0 0 0))
;;; Generated autoloads from lsp-ansible.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-ansible" '("lsp-ansible-")))

;;;***

;;;### (autoloads nil "lsp-asm" "lsp-asm.el" (0 0 0 0))
;;; Generated autoloads from lsp-asm.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-asm" '("lsp-asm-")))

;;;***

;;;### (autoloads nil "lsp-astro" "lsp-astro.el" (0 0 0 0))
;;; Generated autoloads from lsp-astro.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-astro" '("lsp-astro--get-initialization-options")))

;;;***

;;;### (autoloads nil "lsp-autotools" "lsp-autotools.el" (0 0 0 0))
;;; Generated autoloads from lsp-autotools.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-autotools" '("lsp-autotools-")))

;;;***

;;;### (autoloads nil "lsp-awk" "lsp-awk.el" (0 0 0 0))
;;; Generated autoloads from lsp-awk.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-awk" '("lsp-awk-executable")))

;;;***

;;;### (autoloads nil "lsp-bash" "lsp-bash.el" (0 0 0 0))
;;; Generated autoloads from lsp-bash.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-bash" '("lsp-bash-")))

;;;***

;;;### (autoloads nil "lsp-beancount" "lsp-beancount.el" (0 0 0 0))
;;; Generated autoloads from lsp-beancount.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-beancount" '("lsp-beancount-")))

;;;***

;;;### (autoloads nil "lsp-bufls" "lsp-bufls.el" (0 0 0 0))
;;; Generated autoloads from lsp-bufls.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-bufls" '("lsp-bufls-")))

;;;***

;;;### (autoloads nil "lsp-camel" "lsp-camel.el" (0 0 0 0))
;;; Generated autoloads from lsp-camel.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-camel" '("lsp-camel-")))

;;;***

;;;### (autoloads nil "lsp-clangd" "lsp-clangd.el" (0 0 0 0))
;;; Generated autoloads from lsp-clangd.el

(autoload 'lsp-cpp-flycheck-clang-tidy-error-explainer "lsp-clangd" "\
Explain a clang-tidy ERROR by scraping documentation from llvm.org.

\(fn ERROR)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-clangd" '("lsp-c")))

;;;***

;;;### (autoloads nil "lsp-clojure" "lsp-clojure.el" (0 0 0 0))
;;; Generated autoloads from lsp-clojure.el

(autoload 'lsp-clojure-show-test-tree "lsp-clojure" "\
Show a test tree and focus on it if IGNORE-FOCUS? is nil.

\(fn IGNORE-FOCUS\\=\\?)" t nil)

(autoload 'lsp-clojure-show-project-tree "lsp-clojure" "\
Show a project tree with source-paths and dependencies.
Focus on it if IGNORE-FOCUS? is nil.

\(fn IGNORE-FOCUS\\=\\?)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-clojure" '("lsp-clojure-")))

;;;***

;;;### (autoloads nil "lsp-cmake" "lsp-cmake.el" (0 0 0 0))
;;; Generated autoloads from lsp-cmake.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-cmake" '("lsp-cmake-")))

;;;***

;;;### (autoloads nil "lsp-cobol" "lsp-cobol.el" (0 0 0 0))
;;; Generated autoloads from lsp-cobol.el

(add-hook 'cobol-mode-hook #'lsp-cobol-start-ls)

(autoload 'lsp-cobol-start-ls "lsp-cobol" "\
Start the COBOL language service." t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-cobol" '("lsp-cobol-")))

;;;***

;;;### (autoloads nil "lsp-completion" "lsp-completion.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from lsp-completion.el

(define-obsolete-variable-alias 'lsp-prefer-capf 'lsp-completion-provider "lsp-mode 7.0.1")

(define-obsolete-variable-alias 'lsp-enable-completion-at-point 'lsp-completion-enable "lsp-mode 7.0.1")

(autoload 'lsp-completion-at-point "lsp-completion" "\
Get lsp completions." nil nil)

(autoload 'lsp-completion--enable "lsp-completion" "\
Enable LSP completion support." nil nil)

(autoload 'lsp-completion-mode "lsp-completion" "\
Toggle LSP completion support.

If called interactively, enable Lsp-Completion mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(add-hook 'lsp-configure-hook (lambda nil (when (and lsp-auto-configure lsp-completion-enable) (lsp-completion--enable))))

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-completion" '("lsp-")))

;;;***

;;;### (autoloads nil "lsp-credo" "lsp-credo.el" (0 0 0 0))
;;; Generated autoloads from lsp-credo.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-credo" '("lsp-credo-")))

;;;***

;;;### (autoloads nil "lsp-crystal" "lsp-crystal.el" (0 0 0 0))
;;; Generated autoloads from lsp-crystal.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-crystal" '("lsp-clients-crystal-executable")))

;;;***

;;;### (autoloads nil "lsp-csharp" "lsp-csharp.el" (0 0 0 0))
;;; Generated autoloads from lsp-csharp.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-csharp" '("lsp-csharp-")))

;;;***

;;;### (autoloads nil "lsp-css" "lsp-css.el" (0 0 0 0))
;;; Generated autoloads from lsp-css.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-css" '("lsp-css-")))

;;;***

;;;### (autoloads nil "lsp-cucumber" "lsp-cucumber.el" (0 0 0 0))
;;; Generated autoloads from lsp-cucumber.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-cucumber" '("lsp-cucumber-")))

;;;***

;;;### (autoloads nil "lsp-cypher" "lsp-cypher.el" (0 0 0 0))
;;; Generated autoloads from lsp-cypher.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-cypher" '("lsp-client--cypher-ls-server-command")))

;;;***

;;;### (autoloads nil "lsp-diagnostics" "lsp-diagnostics.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from lsp-diagnostics.el

(define-obsolete-variable-alias 'lsp-diagnostic-package 'lsp-diagnostics-provider "lsp-mode 7.0.1")

(define-obsolete-variable-alias 'lsp-flycheck-default-level 'lsp-diagnostics-flycheck-default-level "lsp-mode 7.0.1")

(autoload 'lsp-diagnostics-lsp-checker-if-needed "lsp-diagnostics" nil nil nil)

(autoload 'lsp-diagnostics--enable "lsp-diagnostics" "\
Enable LSP checker support." nil nil)

(autoload 'lsp-diagnostics-mode "lsp-diagnostics" "\
Toggle LSP diagnostics integration.

If called interactively, enable Lsp-Diagnostics mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(add-hook 'lsp-configure-hook (lambda nil (when lsp-auto-configure (lsp-diagnostics--enable))))

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-diagnostics" '("lsp-diagnostics-")))

;;;***

;;;### (autoloads nil "lsp-dired" "lsp-dired.el" (0 0 0 0))
;;; Generated autoloads from lsp-dired.el

(defvar lsp-dired-mode nil "\
Non-nil if Lsp-Dired mode is enabled.
See the `lsp-dired-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `lsp-dired-mode'.")

(custom-autoload 'lsp-dired-mode "lsp-dired" nil)

(autoload 'lsp-dired-mode "lsp-dired" "\
Display `lsp-mode' icons for each file in a dired buffer.

If called interactively, enable Lsp-Dired mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-dired" '("lsp-dired-")))

;;;***

;;;### (autoloads nil "lsp-dockerfile" "lsp-dockerfile.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from lsp-dockerfile.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-dockerfile" '("lsp-dockerfile-language-server-command")))

;;;***

;;;### (autoloads nil "lsp-dot" "lsp-dot.el" (0 0 0 0))
;;; Generated autoloads from lsp-dot.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-dot" '("lsp-dot--dot-ls-server-command")))

;;;***

;;;### (autoloads nil "lsp-earthly" "lsp-earthly.el" (0 0 0 0))
;;; Generated autoloads from lsp-earthly.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-earthly" '("lsp-earthly-")))

;;;***

;;;### (autoloads nil "lsp-elixir" "lsp-elixir.el" (0 0 0 0))
;;; Generated autoloads from lsp-elixir.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-elixir" '("lsp-elixir-")))

;;;***

;;;### (autoloads nil "lsp-elm" "lsp-elm.el" (0 0 0 0))
;;; Generated autoloads from lsp-elm.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-elm" '("lsp-")))

;;;***

;;;### (autoloads nil "lsp-emmet" "lsp-emmet.el" (0 0 0 0))
;;; Generated autoloads from lsp-emmet.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-emmet" '("lsp-emmet-ls-command")))

;;;***

;;;### (autoloads nil "lsp-erlang" "lsp-erlang.el" (0 0 0 0))
;;; Generated autoloads from lsp-erlang.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-erlang" '("lsp-erlang-")))

;;;***

;;;### (autoloads nil "lsp-eslint" "lsp-eslint.el" (0 0 0 0))
;;; Generated autoloads from lsp-eslint.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-eslint" '("lsp-")))

;;;***

;;;### (autoloads nil "lsp-fortran" "lsp-fortran.el" (0 0 0 0))
;;; Generated autoloads from lsp-fortran.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-fortran" '("lsp-clients-")))

;;;***

;;;### (autoloads nil "lsp-fsharp" "lsp-fsharp.el" (0 0 0 0))
;;; Generated autoloads from lsp-fsharp.el

(autoload 'lsp-fsharp--workspace-load "lsp-fsharp" "\
Load all of the provided PROJECTS.

\(fn PROJECTS)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-fsharp" '("lsp-fsharp-")))

;;;***

;;;### (autoloads nil "lsp-gdscript" "lsp-gdscript.el" (0 0 0 0))
;;; Generated autoloads from lsp-gdscript.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-gdscript" '("lsp-gdscript-")))

;;;***

;;;### (autoloads nil "lsp-gleam" "lsp-gleam.el" (0 0 0 0))
;;; Generated autoloads from lsp-gleam.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-gleam" '("lsp-gleam-executable")))

;;;***

;;;### (autoloads nil "lsp-glsl" "lsp-glsl.el" (0 0 0 0))
;;; Generated autoloads from lsp-glsl.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-glsl" '("lsp-glsl-executable")))

;;;***

;;;### (autoloads nil "lsp-go" "lsp-go.el" (0 0 0 0))
;;; Generated autoloads from lsp-go.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-go" '("lsp-go-")))

;;;***

;;;### (autoloads nil "lsp-golangci-lint" "lsp-golangci-lint.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from lsp-golangci-lint.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-golangci-lint" '("lsp-golangci-lint-")))

;;;***

;;;### (autoloads nil "lsp-graphql" "lsp-graphql.el" (0 0 0 0))
;;; Generated autoloads from lsp-graphql.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-graphql" '("lsp-")))

;;;***

;;;### (autoloads nil "lsp-groovy" "lsp-groovy.el" (0 0 0 0))
;;; Generated autoloads from lsp-groovy.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-groovy" '("lsp-groovy-")))

;;;***

;;;### (autoloads nil "lsp-hack" "lsp-hack.el" (0 0 0 0))
;;; Generated autoloads from lsp-hack.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-hack" '("lsp-clients-hack-command")))

;;;***

;;;### (autoloads nil "lsp-haxe" "lsp-haxe.el" (0 0 0 0))
;;; Generated autoloads from lsp-haxe.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-haxe" '("lsp-")))

;;;***

;;;### (autoloads nil "lsp-headerline" "lsp-headerline.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from lsp-headerline.el

(autoload 'lsp-headerline-breadcrumb-mode "lsp-headerline" "\
Toggle breadcrumb on headerline.

If called interactively, enable Lsp-Headerline-Breadcrumb mode if
ARG is positive, and disable it if ARG is zero or negative.  If
called from Lisp, also enable the mode if ARG is omitted or nil,
and toggle it if ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(autoload 'lsp-breadcrumb-go-to-symbol "lsp-headerline" "\
Go to the symbol on breadcrumb at SYMBOL-POSITION.

\(fn SYMBOL-POSITION)" t nil)

(autoload 'lsp-breadcrumb-narrow-to-symbol "lsp-headerline" "\
Narrow to the symbol range on breadcrumb at SYMBOL-POSITION.

\(fn SYMBOL-POSITION)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-headerline" '("lsp-headerline-")))

;;;***

;;;### (autoloads nil "lsp-html" "lsp-html.el" (0 0 0 0))
;;; Generated autoloads from lsp-html.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-html" '("lsp-html-")))

;;;***

;;;### (autoloads nil "lsp-hy" "lsp-hy.el" (0 0 0 0))
;;; Generated autoloads from lsp-hy.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-hy" '("lsp-clients-hy-server-executable")))

;;;***

;;;### (autoloads nil "lsp-icons" "lsp-icons.el" (0 0 0 0))
;;; Generated autoloads from lsp-icons.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-icons" '("lsp-")))

;;;***

;;;### (autoloads nil "lsp-ido" "lsp-ido.el" (0 0 0 0))
;;; Generated autoloads from lsp-ido.el

(autoload 'lsp-ido-workspace-symbol "lsp-ido" "\
`ido' for lsp workspace/symbol.
When called with prefix ARG the default selection will be symbol at point.

\(fn ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-ido" '("lsp-ido-")))

;;;***

;;;### (autoloads nil "lsp-idris" "lsp-idris.el" (0 0 0 0))
;;; Generated autoloads from lsp-idris.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-idris" '("lsp-idris2-lsp-")))

;;;***

;;;### (autoloads nil "lsp-iedit" "lsp-iedit.el" (0 0 0 0))
;;; Generated autoloads from lsp-iedit.el

(autoload 'lsp-iedit-highlights "lsp-iedit" "\
Start an `iedit' operation on the documentHighlights at point.
This can be used as a primitive `lsp-rename' replacement if the
language server doesn't support renaming.

See also `lsp-enable-symbol-highlighting'." t nil)

(autoload 'lsp-iedit-linked-ranges "lsp-iedit" "\
Start an `iedit' for `textDocument/linkedEditingRange'" t nil)

(autoload 'lsp-evil-multiedit-highlights "lsp-iedit" "\
Start an `evil-multiedit' operation on the documentHighlights at point.
This can be used as a primitive `lsp-rename' replacement if the
language server doesn't support renaming.

See also `lsp-enable-symbol-highlighting'." t nil)

(autoload 'lsp-evil-multiedit-linked-ranges "lsp-iedit" "\
Start an `evil-multiedit' for `textDocument/linkedEditingRange'" t nil)

(autoload 'lsp-evil-state-highlights "lsp-iedit" "\
Start `iedit-mode'. for `textDocument/documentHighlight'" t nil)

(autoload 'lsp-evil-state-linked-ranges "lsp-iedit" "\
Start `iedit-mode'. for `textDocument/linkedEditingRange'" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-iedit" '("lsp-iedit--on-ranges")))

;;;***

;;;### (autoloads nil "lsp-javascript" "lsp-javascript.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from lsp-javascript.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-javascript" '("lsp-")))

;;;***

;;;### (autoloads nil "lsp-jq" "lsp-jq.el" (0 0 0 0))
;;; Generated autoloads from lsp-jq.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-jq" '("lsp-clients-jq-server-executable")))

;;;***

;;;### (autoloads nil "lsp-json" "lsp-json.el" (0 0 0 0))
;;; Generated autoloads from lsp-json.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-json" '("lsp-")))

;;;***

;;;### (autoloads nil "lsp-jsonnet" "lsp-jsonnet.el" (0 0 0 0))
;;; Generated autoloads from lsp-jsonnet.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-jsonnet" '("lsp-clients-jsonnet-server-executable")))

;;;***

;;;### (autoloads nil "lsp-kotlin" "lsp-kotlin.el" (0 0 0 0))
;;; Generated autoloads from lsp-kotlin.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-kotlin" '("lsp-")))

;;;***

;;;### (autoloads nil "lsp-lens" "lsp-lens.el" (0 0 0 0))
;;; Generated autoloads from lsp-lens.el

(autoload 'lsp-lens--enable "lsp-lens" "\
Enable lens mode." nil nil)

(autoload 'lsp-lens-show "lsp-lens" "\
Display lenses in the buffer." t nil)

(autoload 'lsp-lens-hide "lsp-lens" "\
Delete all lenses." t nil)

(autoload 'lsp-lens-mode "lsp-lens" "\
Toggle code-lens overlays.

If called interactively, enable Lsp-Lens mode if ARG is positive,
and disable it if ARG is zero or negative.  If called from Lisp,
also enable the mode if ARG is omitted or nil, and toggle it if
ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(autoload 'lsp-avy-lens "lsp-lens" "\
Click lsp lens using `avy' package." t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-lens" '("lsp-")))

;;;***

;;;### (autoloads nil "lsp-lisp" "lsp-lisp.el" (0 0 0 0))
;;; Generated autoloads from lsp-lisp.el

(autoload 'lsp-lisp-alive-start-ls "lsp-lisp" "\
Start the alive-lsp." t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-lisp" '("lsp-lisp-a")))

;;;***

;;;### (autoloads nil "lsp-lua" "lsp-lua.el" (0 0 0 0))
;;; Generated autoloads from lsp-lua.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-lua" '("lsp-")))

;;;***

;;;### (autoloads nil "lsp-magik" "lsp-magik.el" (0 0 0 0))
;;; Generated autoloads from lsp-magik.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-magik" '("lsp-magik-")))

;;;***

;;;### (autoloads nil "lsp-markdown" "lsp-markdown.el" (0 0 0 0))
;;; Generated autoloads from lsp-markdown.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-markdown" '("lsp-markdown-")))

;;;***

;;;### (autoloads nil "lsp-marksman" "lsp-marksman.el" (0 0 0 0))
;;; Generated autoloads from lsp-marksman.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-marksman" '("lsp-marksman-")))

;;;***

;;;### (autoloads nil "lsp-mdx" "lsp-mdx.el" (0 0 0 0))
;;; Generated autoloads from lsp-mdx.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-mdx" '("lsp-mdx-server-command")))

;;;***

;;;### (autoloads nil "lsp-meson" "lsp-meson.el" (0 0 0 0))
;;; Generated autoloads from lsp-meson.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-meson" '("lsp-meson-")))

;;;***

;;;### (autoloads nil "lsp-mint" "lsp-mint.el" (0 0 0 0))
;;; Generated autoloads from lsp-mint.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-mint" '("lsp-clients-mint-executable")))

;;;***

;;;### (autoloads nil "lsp-mode" "lsp-mode.el" (0 0 0 0))
;;; Generated autoloads from lsp-mode.el
(put 'lsp-enable-file-watchers 'safe-local-variable #'booleanp)
(put 'lsp-file-watch-ignored-directories 'safe-local-variable 'lsp--string-listp)
(put 'lsp-file-watch-ignored-files 'safe-local-variable 'lsp--string-listp)
(put 'lsp-file-watch-threshold 'safe-local-variable (lambda (i) (or (numberp i) (not i))))

(autoload 'lsp--string-listp "lsp-mode" "\
Return t if all elements of SEQUENCE are strings, else nil.

\(fn SEQUENCE)" nil nil)

(autoload 'lsp-load-vscode-workspace "lsp-mode" "\
Load vscode workspace from FILE

\(fn FILE)" t nil)

(autoload 'lsp-save-vscode-workspace "lsp-mode" "\
Save vscode workspace to FILE

\(fn FILE)" t nil)

(autoload 'lsp-install-server "lsp-mode" "\
Interactively install or re-install server.
When prefix UPDATE? is t force installation even if the server is present.

\(fn UPDATE\\=\\? &optional SERVER-ID)" t nil)

(autoload 'lsp-uninstall-server "lsp-mode" "\
Delete a LSP server from `lsp-server-install-dir'.

\(fn DIR)" t nil)

(autoload 'lsp-uninstall-servers "lsp-mode" "\
Uninstall all installed servers." t nil)

(autoload 'lsp-update-server "lsp-mode" "\
Interactively update (reinstall) a server.

\(fn &optional SERVER-ID)" t nil)

(autoload 'lsp-update-servers "lsp-mode" "\
Update (reinstall) all installed servers." t nil)

(autoload 'lsp-ensure-server "lsp-mode" "\
Ensure server SERVER-ID

\(fn SERVER-ID)" nil nil)

(autoload 'lsp "lsp-mode" "\
Entry point for the server startup.
When ARG is t the lsp mode will start new language server even if
there is language server which can handle current language. When
ARG is nil current file will be opened in multi folder language
server if there is such. When `lsp' is called with prefix
argument ask the user to select which language server to start.

\(fn &optional ARG)" t nil)

(autoload 'lsp-deferred "lsp-mode" "\
Entry point that defers server startup until buffer is visible.
`lsp-deferred' will wait until the buffer is visible before invoking `lsp'.
This avoids overloading the server with many files when starting Emacs." nil nil)

(autoload 'lsp-start-plain "lsp-mode" "\
Start `lsp-mode' using minimal configuration using the latest `melpa' version
of the packages.

In case the major-mode that you are using for " t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-mode" '("defcustom-lsp" "lsp-" "make-lsp-client" "when-lsp-workspace" "with-lsp-workspace")))

;;;***

;;;### (autoloads nil "lsp-modeline" "lsp-modeline.el" (0 0 0 0))
;;; Generated autoloads from lsp-modeline.el

(define-obsolete-variable-alias 'lsp-diagnostics-modeline-scope 'lsp-modeline-diagnostics-scope "lsp-mode 7.0.1")

(autoload 'lsp-modeline-code-actions-mode "lsp-modeline" "\
Toggle code actions on modeline.

If called interactively, enable Lsp-Modeline-Code-Actions mode if
ARG is positive, and disable it if ARG is zero or negative.  If
called from Lisp, also enable the mode if ARG is omitted or nil,
and toggle it if ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(define-obsolete-function-alias 'lsp-diagnostics-modeline-mode 'lsp-modeline-diagnostics-mode "lsp-mode 7.0.1")

(autoload 'lsp-modeline-diagnostics-mode "lsp-modeline" "\
Toggle diagnostics modeline.

If called interactively, enable Lsp-Modeline-Diagnostics mode if
ARG is positive, and disable it if ARG is zero or negative.  If
called from Lisp, also enable the mode if ARG is omitted or nil,
and toggle it if ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(autoload 'lsp-modeline-workspace-status-mode "lsp-modeline" "\
Toggle workspace status on modeline.

If called interactively, enable Lsp-Modeline-Workspace-Status
mode if ARG is positive, and disable it if ARG is zero or
negative.  If called from Lisp, also enable the mode if ARG is
omitted or nil, and toggle it if ARG is `toggle'; disable the
mode otherwise.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-modeline" '("lsp-")))

;;;***

;;;### (autoloads nil "lsp-mojo" "lsp-mojo.el" (0 0 0 0))
;;; Generated autoloads from lsp-mojo.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-mojo" '("lsp-mojo-executable")))

;;;***

;;;### (autoloads nil "lsp-move" "lsp-move.el" (0 0 0 0))
;;; Generated autoloads from lsp-move.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-move" '("lsp-clients-")))

;;;***

;;;### (autoloads nil "lsp-nginx" "lsp-nginx.el" (0 0 0 0))
;;; Generated autoloads from lsp-nginx.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-nginx" '("lsp-nginx-")))

;;;***

;;;### (autoloads nil "lsp-nim" "lsp-nim.el" (0 0 0 0))
;;; Generated autoloads from lsp-nim.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-nim" '("lsp-nim-l")))

;;;***

;;;### (autoloads nil "lsp-nix" "lsp-nix.el" (0 0 0 0))
;;; Generated autoloads from lsp-nix.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-nix" '("lsp-nix-")))

;;;***

;;;### (autoloads nil "lsp-nushell" "lsp-nushell.el" (0 0 0 0))
;;; Generated autoloads from lsp-nushell.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-nushell" '("lsp-nushell-language-server-command")))

;;;***

;;;### (autoloads nil "lsp-ocaml" "lsp-ocaml.el" (0 0 0 0))
;;; Generated autoloads from lsp-ocaml.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-ocaml" '("lsp-")))

;;;***

;;;### (autoloads nil "lsp-openscad" "lsp-openscad.el" (0 0 0 0))
;;; Generated autoloads from lsp-openscad.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-openscad" '("lsp-openscad-")))

;;;***

;;;### (autoloads nil "lsp-perl" "lsp-perl.el" (0 0 0 0))
;;; Generated autoloads from lsp-perl.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-perl" '("lsp-perl-")))

;;;***

;;;### (autoloads nil "lsp-perlnavigator" "lsp-perlnavigator.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from lsp-perlnavigator.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-perlnavigator" '("lsp-perlnavigator-")))

;;;***

;;;### (autoloads nil "lsp-php" "lsp-php.el" (0 0 0 0))
;;; Generated autoloads from lsp-php.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-php" '("lsp-")))

;;;***

;;;### (autoloads nil "lsp-pls" "lsp-pls.el" (0 0 0 0))
;;; Generated autoloads from lsp-pls.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-pls" '("lsp-pls-")))

;;;***

;;;### (autoloads nil "lsp-prolog" "lsp-prolog.el" (0 0 0 0))
;;; Generated autoloads from lsp-prolog.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-prolog" '("lsp-prolog-server-command")))

;;;***

;;;### (autoloads nil "lsp-protocol" "lsp-protocol.el" (0 0 0 0))
;;; Generated autoloads from lsp-protocol.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-protocol" '("dash-expand:&RangeToPoint" "lsp")))

;;;***

;;;### (autoloads nil "lsp-purescript" "lsp-purescript.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from lsp-purescript.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-purescript" '("lsp-purescript-")))

;;;***

;;;### (autoloads nil "lsp-pwsh" "lsp-pwsh.el" (0 0 0 0))
;;; Generated autoloads from lsp-pwsh.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-pwsh" '("lsp-pwsh-")))

;;;***

;;;### (autoloads nil "lsp-pyls" "lsp-pyls.el" (0 0 0 0))
;;; Generated autoloads from lsp-pyls.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-pyls" '("lsp-")))

;;;***

;;;### (autoloads nil "lsp-pylsp" "lsp-pylsp.el" (0 0 0 0))
;;; Generated autoloads from lsp-pylsp.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-pylsp" '("lsp-")))

;;;***

;;;### (autoloads nil "lsp-qml" "lsp-qml.el" (0 0 0 0))
;;; Generated autoloads from lsp-qml.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-qml" '("lsp-qml-server-command")))

;;;***

;;;### (autoloads nil "lsp-r" "lsp-r.el" (0 0 0 0))
;;; Generated autoloads from lsp-r.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-r" '("lsp-clients-r-server-command")))

;;;***

;;;### (autoloads nil "lsp-racket" "lsp-racket.el" (0 0 0 0))
;;; Generated autoloads from lsp-racket.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-racket" '("lsp-racket-lang")))

;;;***

;;;### (autoloads nil "lsp-remark" "lsp-remark.el" (0 0 0 0))
;;; Generated autoloads from lsp-remark.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-remark" '("lsp-remark-server-command")))

;;;***

;;;### (autoloads nil "lsp-rf" "lsp-rf.el" (0 0 0 0))
;;; Generated autoloads from lsp-rf.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-rf" '("expand-start-command" "lsp-rf-language-server-" "parse-rf-language-server-")))

;;;***

;;;### (autoloads nil "lsp-roslyn" "lsp-roslyn.el" (0 0 0 0))
;;; Generated autoloads from lsp-roslyn.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-roslyn" '("lsp-roslyn-")))

;;;***

;;;### (autoloads nil "lsp-rpm-spec" "lsp-rpm-spec.el" (0 0 0 0))
;;; Generated autoloads from lsp-rpm-spec.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-rpm-spec" '("lsp-rpm-spec-")))

;;;***

;;;### (autoloads nil "lsp-rubocop" "lsp-rubocop.el" (0 0 0 0))
;;; Generated autoloads from lsp-rubocop.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-rubocop" '("lsp-rubocop-")))

;;;***

;;;### (autoloads nil "lsp-ruby-lsp" "lsp-ruby-lsp.el" (0 0 0 0))
;;; Generated autoloads from lsp-ruby-lsp.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-ruby-lsp" '("lsp-ruby-lsp-")))

;;;***

;;;### (autoloads nil "lsp-ruby-syntax-tree" "lsp-ruby-syntax-tree.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from lsp-ruby-syntax-tree.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-ruby-syntax-tree" '("lsp-ruby-syntax-tree-")))

;;;***

;;;### (autoloads nil "lsp-ruff-lsp" "lsp-ruff-lsp.el" (0 0 0 0))
;;; Generated autoloads from lsp-ruff-lsp.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-ruff-lsp" '("lsp-ruff-lsp-")))

;;;***

;;;### (autoloads nil "lsp-rust" "lsp-rust.el" (0 0 0 0))
;;; Generated autoloads from lsp-rust.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-rust" '("lsp-")))

;;;***

;;;### (autoloads nil "lsp-semantic-tokens" "lsp-semantic-tokens.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from lsp-semantic-tokens.el

(defvar-local semantic-token-modifier-cache (make-hash-table) "\
A cache of modifier values to the selected fonts.
This allows whole-bitmap lookup instead of checking each bit. The
expectation is that usage of modifiers will tend to cluster, so
we will not have the full range of possible usages, hence a
tractable hash map.

This is set as buffer-local. It should probably be shared in a
given workspace/language-server combination.

This cache should be flushed every time any modifier
configuration changes.")

(autoload 'lsp--semantic-tokens-initialize-buffer "lsp-semantic-tokens" "\
Initialize the buffer for semantic tokens.
IS-RANGE-PROVIDER is non-nil when server supports range requests." nil nil)

(autoload 'lsp--semantic-tokens-initialize-workspace "lsp-semantic-tokens" "\
Initialize semantic tokens for WORKSPACE.

\(fn WORKSPACE)" nil nil)

(autoload 'lsp-semantic-tokens--warn-about-deprecated-setting "lsp-semantic-tokens" "\
Warn about deprecated semantic highlighting variable." nil nil)

(autoload 'lsp-semantic-tokens--enable "lsp-semantic-tokens" "\
Enable semantic tokens mode." nil nil)

(autoload 'lsp-semantic-tokens-mode "lsp-semantic-tokens" "\
Toggle semantic-tokens support.

If called interactively, enable Lsp-Semantic-Tokens mode if ARG
is positive, and disable it if ARG is zero or negative.  If
called from Lisp, also enable the mode if ARG is omitted or nil,
and toggle it if ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-semantic-tokens" '("lsp-")))

;;;***

;;;### (autoloads nil "lsp-semgrep" "lsp-semgrep.el" (0 0 0 0))
;;; Generated autoloads from lsp-semgrep.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-semgrep" '("lsp-semgrep-" "semgrep-")))

;;;***

;;;### (autoloads nil "lsp-sml" "lsp-sml.el" (0 0 0 0))
;;; Generated autoloads from lsp-sml.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-sml" '("lsp-sml-millet-")))

;;;***

;;;### (autoloads nil "lsp-solargraph" "lsp-solargraph.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from lsp-solargraph.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-solargraph" '("lsp-solargraph-")))

;;;***

;;;### (autoloads nil "lsp-solidity" "lsp-solidity.el" (0 0 0 0))
;;; Generated autoloads from lsp-solidity.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-solidity" '("lsp-client--solidity-")))

;;;***

;;;### (autoloads nil "lsp-sorbet" "lsp-sorbet.el" (0 0 0 0))
;;; Generated autoloads from lsp-sorbet.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-sorbet" '("lsp-sorbet-")))

;;;***

;;;### (autoloads nil "lsp-sql" "lsp-sql.el" (0 0 0 0))
;;; Generated autoloads from lsp-sql.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-sql" '("lsp-sql-")))

;;;***

;;;### (autoloads nil "lsp-sqls" "lsp-sqls.el" (0 0 0 0))
;;; Generated autoloads from lsp-sqls.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-sqls" '("lsp-sql")))

;;;***

;;;### (autoloads nil "lsp-steep" "lsp-steep.el" (0 0 0 0))
;;; Generated autoloads from lsp-steep.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-steep" '("lsp-steep-")))

;;;***

;;;### (autoloads nil "lsp-svelte" "lsp-svelte.el" (0 0 0 0))
;;; Generated autoloads from lsp-svelte.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-svelte" '("lsp-svelte-plugin-")))

;;;***

;;;### (autoloads nil "lsp-terraform" "lsp-terraform.el" (0 0 0 0))
;;; Generated autoloads from lsp-terraform.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-terraform" '("construct-tf-package" "lsp-t")))

;;;***

;;;### (autoloads nil "lsp-tex" "lsp-tex.el" (0 0 0 0))
;;; Generated autoloads from lsp-tex.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-tex" '("lsp-")))

;;;***

;;;### (autoloads nil "lsp-toml" "lsp-toml.el" (0 0 0 0))
;;; Generated autoloads from lsp-toml.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-toml" '("lsp-toml-")))

;;;***

;;;### (autoloads nil "lsp-trunk" "lsp-trunk.el" (0 0 0 0))
;;; Generated autoloads from lsp-trunk.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-trunk" '("lsp-trunk-")))

;;;***

;;;### (autoloads nil "lsp-ttcn3" "lsp-ttcn3.el" (0 0 0 0))
;;; Generated autoloads from lsp-ttcn3.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-ttcn3" '("lsp-ttcn3-lsp-server-command")))

;;;***

;;;### (autoloads nil "lsp-typeprof" "lsp-typeprof.el" (0 0 0 0))
;;; Generated autoloads from lsp-typeprof.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-typeprof" '("lsp-typeprof-")))

;;;***

;;;### (autoloads nil "lsp-v" "lsp-v.el" (0 0 0 0))
;;; Generated autoloads from lsp-v.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-v" '("lsp-v-vls-executable")))

;;;***

;;;### (autoloads nil "lsp-vala" "lsp-vala.el" (0 0 0 0))
;;; Generated autoloads from lsp-vala.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-vala" '("lsp-clients-vala-ls-executable")))

;;;***

;;;### (autoloads nil "lsp-verilog" "lsp-verilog.el" (0 0 0 0))
;;; Generated autoloads from lsp-verilog.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-verilog" '("lsp-clients-")))

;;;***

;;;### (autoloads nil "lsp-vetur" "lsp-vetur.el" (0 0 0 0))
;;; Generated autoloads from lsp-vetur.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-vetur" '("lsp-vetur-")))

;;;***

;;;### (autoloads nil "lsp-vhdl" "lsp-vhdl.el" (0 0 0 0))
;;; Generated autoloads from lsp-vhdl.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-vhdl" '("ghdl-ls-bin-name" "hdl-checker-bin-name" "lsp-vhdl-" "vhdl-")))

;;;***

;;;### (autoloads nil "lsp-vimscript" "lsp-vimscript.el" (0 0 0 0))
;;; Generated autoloads from lsp-vimscript.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-vimscript" '("lsp-clients-vim-")))

;;;***

;;;### (autoloads nil "lsp-volar" "lsp-volar.el" (0 0 0 0))
;;; Generated autoloads from lsp-volar.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-volar" '("lsp-volar-")))

;;;***

;;;### (autoloads nil "lsp-wgsl" "lsp-wgsl.el" (0 0 0 0))
;;; Generated autoloads from lsp-wgsl.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-wgsl" '("lsp-wgsl-")))

;;;***

;;;### (autoloads nil "lsp-xml" "lsp-xml.el" (0 0 0 0))
;;; Generated autoloads from lsp-xml.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-xml" '("lsp-xml-")))

;;;***

;;;### (autoloads nil "lsp-yaml" "lsp-yaml.el" (0 0 0 0))
;;; Generated autoloads from lsp-yaml.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-yaml" '("lsp-yaml-")))

;;;***

;;;### (autoloads nil "lsp-yang" "lsp-yang.el" (0 0 0 0))
;;; Generated autoloads from lsp-yang.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-yang" '("lsp-yang-")))

;;;***

;;;### (autoloads nil "lsp-zig" "lsp-zig.el" (0 0 0 0))
;;; Generated autoloads from lsp-zig.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-zig" '("lsp-z")))

;;;***

;;;### (autoloads nil nil ("lsp-d.el" "lsp-dhall.el" "lsp-mode-pkg.el"
;;;;;;  "lsp-tilt.el" "lsp.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; lsp-mode-autoloads.el ends here

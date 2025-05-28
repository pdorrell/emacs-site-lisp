;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "magit" "20250527.1223"
  "A Git porcelain inside Emacs."
  '((emacs         "27.1")
    (compat        "30.1.0.0")
    (llama         "0.6.2")
    (magit-section "4.3.5")
    (seq           "2.24")
    (transient     "0.8.8")
    (with-editor   "3.4.3"))
  :url "https://github.com/magit/magit"
  :commit "e3806cbb7dd38ab73624ad48024998705f9d0d20"
  :revdesc "e3806cbb7dd3"
  :keywords '("git" "tools" "vc")
  :authors '(("Marius Vollmer" . "marius.vollmer@gmail.com")
             ("Jonas Bernoulli" . "emacs.magit@jonas.bernoulli.dev"))
  :maintainers '(("Jonas Bernoulli" . "emacs.magit@jonas.bernoulli.dev")
                 ("Kyle Meyer" . "kyle@kyleam.com")))

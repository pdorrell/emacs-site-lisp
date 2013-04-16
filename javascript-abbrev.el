;; Copyright (C) 2000,2001 Philip Dorrell

(def-javascript-abbrev "f" "function ")
(def-javascript-abbrev "p" "console.log \"")

(def-javascript-abbrev "if" '("if (" indent mark ") {" return indent return "}" indent goto-mark))
(def-javascript-abbrev "k" '("/** " mark " */" goto-mark))
(def-javascript-abbrev "l" "length")

(def-javascript-abbrev "r" "return ")
(def-javascript-abbrev "rf" "return false;")
(def-javascript-abbrev "rt" "return true;")

;; Copyright (C) 2000,2001 Philip Dorrell

(set-abbrevs 
 'javascript
 '( 
   ("f" "function ")
   ("p" ("console.log(`" mark "`);" goto-mark))
   
   ("if" ("if (" indent mark ") {" return indent return "}" indent goto-mark))
   ("k" ("/** " mark " */" goto-mark))
   ("l" "length")
   
   ("r" "return ")
   ("rf" "return false;")
   ("rt" "return true;")
   
   ("pr" ("prototype = {" return indent mark return "};" indent goto-mark))
   ("vtt" ("var $this = this;" indent))
   
   ("tn" ("throw new " indent))
   
   ("i" ("inspect(" mark ")" goto-mark)) ) )

(set-abbrevs 
 'typescript
 '( 
   ("c" ("constructor(" mark ")" goto-mark))
   ("f" "function ")
   ("p" "console.log(\"")
   
   ("if" ("if (" indent mark ") {" return indent return "}" indent goto-mark))
   ("k" ("/** " mark " */" goto-mark))
   ("l" "length")
   
   ("r" "return ")
   ("rf" "return false;")
   ("rt" "return true;")
   
   ("tn" ("throw new " indent)) ) )

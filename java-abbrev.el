;; Copyright (C) 2000,2001 Philip Dorrell

(set-abbrevs
 'java
 '(

   ("class"
    ((java-package)
     "\npublic class " base-name " {\n\n"
     indent mark 
     "\n}\n" goto-mark) )

   ("xclass" 
    ((java-package)
     "\npublic class " base-name " extends Exception {\n\n"
     "public " base-name " (String message) {" indent return
     "super (message);" indent return
     "}" indent return return
     "public " base-name " (String message, Throwable throwable) {" indent return
     "super (message, throwable);" indent return
     "}" indent return
     "}" indent return))

   ("iclass" 
    ((java-package)
     "\npublic interface " base-name " {\n\n"
     indent mark 
     "\n}\n" goto-mark) )

   ("pack"
    ((java-package)) )

   ;;-----------------------------------------------------------------
   ("ij" "import java.")
   ("ijb" "import java.beans.*;")
   ("ija" "import java.awt.*;")
   ("ijae" "import java.awt.event.*;")
   ("ijap" "import java.applet.*;")
   ("iji" "import java.io.*;")
   ("ijhs" "import java.io.*;\nimport javax.servlet.*;\nimport javax.servlet.http.*;\n")
   ("ijlr" "import java.lang.reflect.*;")
   ("ijm" "import java.math.*;")
   ("ijn" "import java.net.*;")
   ("ijs" "import javax.swing.*;")
   ("ijse" "import javax.swing.event.*;")
   ("ijsq" "import java.sql.*;")
   ("iju" "import java.util.*;")

   ("ifmt" "import freemarker.template.*;")

   ;;-----------------------------------------------------------------
   ("a" "abstract ")
   ("ad" "addElement (")

   ("aal" 
    ("addActionListener (new ActionListener()" indent " {" return
     "public void actionPerformed (ActionEvent e)" indent " {" return
     indent mark return "}" indent return
     "});" indent goto-mark))

   ("ail" 
    ("addItemListener (new ItemListener()" indent " {" return
     "public void itemStateChanged (ItemEvent e)" indent " {" return
     indent mark return "}" indent return
     "});" indent goto-mark))

   ("al" "ArrayList")

   ("awl" 
    ("addWindowListener (new WindowAdapter()" indent " {" return
     "public void windowClosing (WindowEvent e)" indent " {" return
     indent mark return "}" indent return
     "});" indent goto-mark))

   ("b" "boolean ")
   ("br" "BufferedReader")
   ("bw" "BufferedWriter ")
   ("c" "char ")
   ("cl" (base-name))
   ("d" "double ")
   ("dim" "Dimension ")
   ("e" (new-line-before "else {" indent return indent mark return "}" indent goto-mark))
   ("ei" (new-line-before "else if (" indent mark ") {" indent return indent return "}" indent goto-mark))
   ("el" "elementAt (")
   ("en" "Enumeration ")
   ("eq" "equals (")
   ("ex" "Exception")
   ("f" "float ")
   ("fin" ("finally {" indent return indent mark return "}" indent goto-mark))
   ("fis" "FileInputStream ")
   ("fos" "FileOutputStream ")
   ("fr" "FileReader ")
   ("fw" "FileWriter ")
   ("gm" "getMessage()")
   ("gr" "Graphics ")
   ("i" "int ")
   ("if" ("if (" indent mark ") {" return indent return "}" indent goto-mark))
   ("ioe" "IOException ")
   ("ims" "implements ")
   ("is" "InputStream ")
   ("k" ("/** " mark " */" goto-mark))
   ("l" "length")
   ("m" "implements")
   ("main" ("public static void main (String args[]) {" 
	    indent return indent mark return "}" indent return goto-mark))
   ("n" "new ")
   ("nfe" "NumberFormatException ")
   ("os" "OutputStream ")
   ("p" "public ")
   ("pa" "public abstract ")
   ("pab" "public abstract boolean ")
   ("pac" "public abstract class ")
   ("pai" "public abstract int ")
   ("pas" "public abstract String ")
   ("pav" "public abstract void ")
   ("pb" "public boolean ")
   ("pc" "public class ")
   ("pi" "public int ")
   ("pf" "public final ")
   ("pfi" "public final int ")
   ("pfs" "public final String ")
   ("ps" "public static ")
   ("psb" "public static boolean ")
   ("psc" "public static class ")
   ("psi" "public static int ")
   ("pss" "public static String ")
   ("pst" "public String ")
   ("psv" "public static void ")
   ("pr" "System.out.println (\"")
   ("pri" "private ")
   ("pro" "protected ")
   ("psf" "public static final ")
   ("psfi" "public static final int ")
   ("psfs" "public static final String ")
   ("pv" "public void ")

   ("r" "return ")
   ("rf" "return false;")
   ("rt" "return true;")
   ("ru" "RuntimeException ")
   ("st" "static ")
   ("s" "String ")
   ("sb" "StringBuffer")
   ("sub" "substring")
   ("sv" "static void ")
   ("syn" "synchronized ")
   ("t" ("throws " indent))
   ("tioe" ("throws IOException" indent))
   ("tn" "throw new ")
   ("try" ("try {" return indent mark return "}" indent 
	   return "catch ( e) {" indent return indent return "}" indent goto-mark))
   ("ts" "toString()")
   ("tsq" "throws SQLException")
   ("v" "void ")
   ("ve" "Vector")
   ("wh" ("while (" indent mark ") {" return indent return "}" indent goto-mark))
   ("x" "extends ")

   ("rn" "return null;")
   ("lh" "int low, int high")
   ("rin" "return Invalids.NONE;")

   ("gcn" "getClass().getName()")
   ("ap" ("@param " indent))
   ("ar" ("@return " indent))
   ("at" ("@throws " indent))
   ("ax" ("@exception " indent))

   ("iter" ("Iterator iter = " indent mark ".iterator();" indent return
	    "while (iter.hasNext()) {" indent return
	    "Object object = (Object)iter.next();" indent return
	    "}" indent
	    goto-mark))

   ("psts" ("public String toString() {" indent return
	    "return " indent mark ";" return
	    "}" indent return
	    goto-mark))

   ("tshe" ("throws SQLException, HibernateException {" indent)) ) )

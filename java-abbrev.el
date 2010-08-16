;; Copyright (C) 2000,2001 Philip Dorrell

(def-java-abbrev "class"
  '((java-before-package) (java-package) (java-after-package)
    "\npublic class " base-name " {\n\n"
    indent mark 
    "\n}\n" goto-mark) )

(def-java-abbrev "xclass" 
  '((java-before-package) (java-package) (java-after-package)
    "\npublic class " base-name " extends Exception {\n\n"
    "public " base-name " (String message) {" indent return
    "super (message);" indent return
    "}" indent return return
    "public " base-name " (String message, Throwable throwable) {" indent return
    "super (message, throwable);" indent return
    "}" indent return
    "}" indent return))

(def-java-abbrev "iclass" 
  '((java-before-package) (java-package) (java-after-package)
    "\npublic interface " base-name " {\n\n"
    indent mark 
    "\n}\n" goto-mark) )

(def-java-abbrev "pack"
  '((java-package)) )

;;-----------------------------------------------------------------
(def-java-abbrev "ij" "import java.")
(def-java-abbrev "ijb" "import java.beans.*;")
(def-java-abbrev "ija" "import java.awt.*;")
(def-java-abbrev "ijae" "import java.awt.event.*;")
(def-java-abbrev "ijap" "import java.applet.*;")
(def-java-abbrev "iji" "import java.io.*;")
(def-java-abbrev "ijhs" "import java.io.*;\nimport javax.servlet.*;\nimport javax.servlet.http.*;\n")
(def-java-abbrev "ijlr" "import java.lang.reflect.*;")
(def-java-abbrev "ijm" "import java.math.*;")
(def-java-abbrev "ijn" "import java.net.*;")
(def-java-abbrev "ijs" "import javax.swing.*;")
(def-java-abbrev "ijse" "import javax.swing.event.*;")
(def-java-abbrev "ijsq" "import java.sql.*;")
(def-java-abbrev "iju" "import java.util.*;")

(def-java-abbrev "ifmt" "import freemarker.template.*;")

;;-----------------------------------------------------------------
(def-java-abbrev "a" "abstract ")
(def-java-abbrev "ad" "addElement (")

(def-java-abbrev "aal" 
		 '("addActionListener (new ActionListener()" indent " {" return
		   "public void actionPerformed (ActionEvent e)" indent " {" return
		   indent mark return "}" indent return
		   "});" indent goto-mark))

(def-java-abbrev "ail" 
		 '("addItemListener (new ItemListener()" indent " {" return
		   "public void itemStateChanged (ItemEvent e)" indent " {" return
		   indent mark return "}" indent return
		   "});" indent goto-mark))

(def-java-abbrev "al" "ArrayList")

(def-java-abbrev "awl" 
		 '("addWindowListener (new WindowAdapter()" indent " {" return
		   "public void windowClosing (WindowEvent e)" indent " {" return
		   indent mark return "}" indent return
		   "});" indent goto-mark))

(def-java-abbrev "b" "boolean ")
(def-java-abbrev "br" "BufferedReader")
(def-java-abbrev "bw" "BufferedWriter ")
(def-java-abbrev "c" "char ")
(def-java-abbrev "cl" '(base-name))
(def-java-abbrev "d" "double ")
(def-java-abbrev "dim" "Dimension ")
(def-java-abbrev "e" '(new-line-before "else {" indent return indent mark return "}" indent goto-mark))
(def-java-abbrev "ei" '(new-line-before "else if (" indent mark ") {" indent return indent return "}" indent goto-mark))
(def-java-abbrev "el" "elementAt (")
(def-java-abbrev "en" "Enumeration ")
(def-java-abbrev "eq" "equals (")
(def-java-abbrev "ex" "Exception")
(def-java-abbrev "f" "float ")
(def-java-abbrev "fin" '("finally {" indent return indent mark return "}" indent goto-mark))
(def-java-abbrev "fis" "FileInputStream ")
(def-java-abbrev "fos" "FileOutputStream ")
(def-java-abbrev "fr" "FileReader ")
(def-java-abbrev "fw" "FileWriter ")
(def-java-abbrev "gm" "getMessage()")
(def-java-abbrev "gr" "Graphics ")
(def-java-abbrev "i" "int ")
(def-java-abbrev "if" '("if (" indent mark ") {" return indent return "}" indent goto-mark))
(def-java-abbrev "ioe" "IOException ")
(def-java-abbrev "ims" "implements ")
(def-java-abbrev "is" "InputStream ")
(def-java-abbrev "k" '("/** " mark " */" goto-mark))
(def-java-abbrev "l" "length")
(def-java-abbrev "m" "implements")
(def-java-abbrev "main" '("public static void main (String args[]) {" 
			  indent return indent mark return "}" indent return goto-mark))
(def-java-abbrev "n" "new ")
(def-java-abbrev "nfe" "NumberFormatException ")
(def-java-abbrev "os" "OutputStream ")
(def-java-abbrev "p" "public ")
(def-java-abbrev "pa" "public abstract ")
(def-java-abbrev "pab" "public abstract boolean ")
(def-java-abbrev "pac" "public abstract class ")
(def-java-abbrev "pai" "public abstract int ")
(def-java-abbrev "pas" "public abstract String ")
(def-java-abbrev "pav" "public abstract void ")
(def-java-abbrev "pb" "public boolean ")
(def-java-abbrev "pc" "public class ")
(def-java-abbrev "pi" "public int ")
(def-java-abbrev "pf" "public final ")
(def-java-abbrev "pfi" "public final int ")
(def-java-abbrev "pfs" "public final String ")
(def-java-abbrev "ps" "public static ")
(def-java-abbrev "psb" "public static boolean ")
(def-java-abbrev "psc" "public static class ")
(def-java-abbrev "psi" "public static int ")
(def-java-abbrev "pss" "public static String ")
(def-java-abbrev "pst" "public String ")
(def-java-abbrev "psv" "public static void ")
(def-java-abbrev "pr" "System.out.println (\"")
(def-java-abbrev "pri" "private ")
(def-java-abbrev "pro" "protected ")
(def-java-abbrev "psf" "public static final ")
(def-java-abbrev "psfi" "public static final int ")
(def-java-abbrev "psfs" "public static final String ")
(def-java-abbrev "pv" "public void ")

(def-java-abbrev "r" "return ")
(def-java-abbrev "rf" "return false;")
(def-java-abbrev "rt" "return true;")
(def-java-abbrev "ru" "RuntimeException ")
(def-java-abbrev "st" "static ")
(def-java-abbrev "s" "String ")
(def-java-abbrev "sb" "StringBuffer")
(def-java-abbrev "sub" "substring")
(def-java-abbrev "sv" "static void ")
(def-java-abbrev "syn" "synchronized ")
(def-java-abbrev "t" '("throws " indent))
(def-java-abbrev "tioe" '("throws IOException" indent))
(def-java-abbrev "tn" "throw new ")
(def-java-abbrev "try" '("try {" return indent mark return "}" indent 
			 return "catch ( e) {" indent return indent return "}" indent goto-mark))
(def-java-abbrev "ts" "toString()")
(def-java-abbrev "tsq" "throws SQLException")
(def-java-abbrev "v" "void ")
(def-java-abbrev "ve" "Vector")
(def-java-abbrev "wh" '("while (" indent mark ") {" return indent return "}" indent goto-mark))
(def-java-abbrev "x" "extends ")

(def-java-abbrev "rn" "return null;")
(def-java-abbrev "lh" "int low, int high")
(def-java-abbrev "rin" "return Invalids.NONE;")

(def-java-abbrev "gcn" "getClass().getName()")
(def-java-abbrev "ap" '("@param " indent))
(def-java-abbrev "ar" '("@return " indent))
(def-java-abbrev "at" '("@throws " indent))
(def-java-abbrev "ax" '("@exception " indent))

(def-java-abbrev "iter" '("Iterator iter = " indent mark ".iterator();" indent return
			  "while (iter.hasNext()) {" indent return
			  "Object object = (Object)iter.next();" indent return
			  "}" indent
			  goto-mark))

(def-java-abbrev "psts" '("public String toString() {" indent return
			  "return " indent mark ";" return
			  "}" indent return
			  goto-mark))

(def-java-abbrev "tshe" '("throws SQLException, HibernateException {" indent))

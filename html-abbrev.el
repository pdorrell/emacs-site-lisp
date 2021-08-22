;; Copyright (C) 2000 Philip Dorrell

(def-html-abbrev "style" '("<style type=\"text/css\">" return "<!--" return mark return
			   "-->" return "</style>" return goto-mark))

(def-html-pair-abbrev "p" "<p>" "</p>")
(def-html-pair-abbrev "pp" "</p>\n\n<p>" "")
(def-html-pair-abbrev "html" 
  "<!DOCTYPE html\">\n<html lang=\"en\" >\n<head><meta charset=\"utf-8\">\n<title>" 
  "</title>\n</head>\n\n<body>\n<h1></h1>\n\n</body>\n</html>\n")

(def-html-pair-abbrev "htmlt" 
  "<html>\n<head><title></title>\n<meta name=\"description\" content=\"\">\n<meta name=\"keywords\" content=\"\">\n</head>\n<body text=\"#000000\" bgcolor=\"#ffffff\" vlink=\"#660099\" link=\"#ff0000\">\n<h1></h1>\n<p>"
  "\n\n<hr><a href=\"../index.html\">Home</a>\n</body></html>\n")
(def-html-pair-abbrev "dl" "<dl>\n<dt>" "</dt>\n <dd></dd>\n</dl>\n")
(def-html-pair-abbrev "dt" "<dt>" "\n <dd></dd>\n")
(def-html-pair-abbrev "dts" "<dt><strong>" "</strong></dt>\n <dd></dd>\n")
(def-html-pair-abbrev "img" "<img src=\"" "\"/>")
(def-html-pair-abbrev "ul" "<ul>\n <li>" "</li>\n</ul>")
(def-html-pair-abbrev "ol" "<ol>\n <li>" "</li>\n</ol>")
(def-html-pair-abbrev "li" " <li>" "</li>")
(def-html-pair-abbrev "tr" "<tr><td>" "</td></tr>")
(def-html-pair-abbrev "n" "&nbsp;")
(def-html-pair-abbrev "s" "<strong>" "</strong>")
(def-html-pair-abbrev "bq" "<blockquote>" "</blockquote>")
(def-html-pair-abbrev "bra" "<br><a href=\"" ".html\"></a>")
(def-html-pair-abbrev "a" "<a href=\"" "\"></a>")
(def-html-pair-abbrev "hr" "<hr/>")
(def-html-pair-abbrev "br" "<br/>")
(def-html-pair-abbrev "brn" "<br/>&nbsp;&nbsp;&nbsp;&nbsp;")
(def-html-pair-abbrev "metad" "<meta name=\"description\" content=\"" "\">")
(def-html-pair-abbrev "metak" "<meta name=\"keywords\" content=\"" "\">")

(def-html-pair-abbrev "wiki" '("<html><head><title>" base-name "</title></head>\n"
			  "<body>\n<h1>" base-name "</h1>\n"
			  (file "author.template")
			  "\n<p>" mark "\n\n" (file "bottom.template") "</body>" goto-mark) )

(def-html-pair-abbrev "k" "<!-- " " -->")

(def-html-pair-abbrev "n" "&ndash; ")

(def-html-pair-abbrev "script" "<script language=\"Javascript\">\n<!--\n" "\n//-->\n</script>\n")
(def-html-pair-abbrev "scripte" "<script language=\"Javascript\" src=\"" "\"></script>\n")

(def-html-pair-abbrev "ex" "<@explain>" "</@explain>")

(def-html-pair-abbrev "fonts" "<font size=\"+1\">" "</font>")

(def-html-pair-abbrev "exp" "<@explainPage>" "</@explainPage>")

(def-html-pair-abbrev "div" "<div class=\"" "\"></div>")
(def-html-pair-abbrev "blt" "<b>&lt;" "&gt;</b>")
(def-html-pair-abbrev "span" "<span class=\"" "\"></span>")

(def-html-pair-abbrev "links" "<link rel=\"stylesheet\" type=\"text/css\" href=\"" "\" />")

(def-html-pair-abbrev "t" "&times; ")

(def-html-pair-abbrev "dm" "<div class=\"math\">" "</div>")
(def-html-pair-abbrev "sm" "<span class=\"math\">" "</span>")

(def-html-pair-abbrev "kc" "<!--[" " ]-->")
(def-html-pair-abbrev "ko" "<!--[" " --><!--]-->")

(def-html-pair-abbrev "cdata" "<![CDATA[" "]]>")

(def-html-pair-abbrev "tex" "<!--[#page.tex(\"" "\")--><!--]-->")

(def-html-pair-abbrev "codep" "<pre class=\"code\"><code>\n" "\n</code></pre>")

(def-html-pair-abbrev "r" "<%=" "%>")

(def-html-pair-abbrev "prop" "<li>\n<div class=\"proposition\">" "</div>\n\n</li>\n")

(def-html-pair-abbrev "aname" "<a name=\"" "\"></a>")

(def-html-pair-abbrev "hd" "<!-- [@header] -->")
(def-html-pair-abbrev "ft" "<!-- [@footer] -->")

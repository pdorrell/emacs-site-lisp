;; Copyright (C) 2000 Philip Dorrell

(def-html-abbrevs
  '(
    ("style" ("<style type=\"text/css\">" return mark return
	      "</style>" return goto-mark))
    ("n" "&ndash; ")
    ("br" "<br/>")
    ("brn" "<br/>&nbsp;&nbsp;&nbsp;&nbsp;")
    ("t" "&times; ")
    ("hd" "<!-- [@header] -->")
    ("ft" "<!-- [@footer] -->")
    ("wiki" ("<html><head><title>" base-name "</title></head>\n"
	     "<body>\n<h1>" base-name "</h1>\n"
	     (file "author.template")
	     "\n<p>" mark "\n\n" (file "bottom.template") "</body>" goto-mark) )
    ) )

(def-html-pair-abbrevs
  '(
    ("p" "<p>" "</p>")
    ("pp" "</p>\n\n<p>" "")
    ("html" 
     "<!DOCTYPE html\">\n<html lang=\"en\" >\n<head><meta charset=\"utf-8\">\n<title>" 
     "</title>\n</head>\n\n<body>\n<h1></h1>\n\n</body>\n</html>\n")

    ("htmlt" 
     "<html>\n<head><title></title>\n<meta name=\"description\" content=\"\">\n<meta name=\"keywords\" content=\"\">\n</head>\n<body text=\"#000000\" bgcolor=\"#ffffff\" vlink=\"#660099\" link=\"#ff0000\">\n<h1></h1>\n<p>"
     "\n\n<hr><a href=\"../index.html\">Home</a>\n</body></html>\n")
    ("dl" "<dl>\n<dt>" "</dt>\n <dd></dd>\n</dl>\n")
    ("dt" "<dt>" "\n <dd></dd>\n")
    ("dts" "<dt><strong>" "</strong></dt>\n <dd></dd>\n")
    ("img" "<img src=\"" "\"/>")
    ("ul" "<ul>\n <li>" "</li>\n</ul>")
    ("ol" "<ol>\n <li>" "</li>\n</ol>")
    ("li" " <li>" "</li>")
    ("tr" "<tr><td>" "</td></tr>")
    ("s" "<strong>" "</strong>")
    ("bq" "<blockquote>" "</blockquote>")
    ("bra" "<br><a href=\"" ".html\"></a>")
    ("a" "<a href=\"" "\"></a>")
    ("metad" "<meta name=\"description\" content=\"" "\">")
    ("metak" "<meta name=\"keywords\" content=\"" "\">")

    ("k" "<!-- " " -->")

    ("script" "<script language=\"Javascript\">\n<!--\n" "\n//-->\n</script>\n")
    ("scripte" "<script language=\"Javascript\" src=\"" "\"></script>\n")

    ("ex" "<@explain>" "</@explain>")

    ("fonts" "<font size=\"+1\">" "</font>")

    ("exp" "<@explainPage>" "</@explainPage>")

    ("div" "<div class=\"" "\"></div>")
    ("blt" "<b>&lt;" "&gt;</b>")
    ("span" "<span class=\"" "\"></span>")

    ("links" "<link rel=\"stylesheet\" type=\"text/css\" href=\"" "\" />")

    ("dm" "<div class=\"math\">" "</div>")
    ("sm" "<span class=\"math\">" "</span>")

    ("kc" "<!--[" " ]-->")
    ("ko" "<!--[" " --><!--]-->")

    ("cdata" "<![CDATA[" "]]>")

    ("tex" "<!--[#page.tex(\"" "\")--><!--]-->")

    ("codep" "<pre class=\"code\"><code>\n" "\n</code></pre>")

    ("r" "<%=" "%>")

    ("prop" "<li>\n<div class=\"proposition\">" "</div>\n\n</li>\n")

    ("aname" "<a name=\"" "\"></a>")

    ) )

library(markdown)

# toc example
mkd <- c("# Header 1", "p1", "## Header 2", "p2")

cat(renderMarkdown(mkd))
cat(renderMarkdown(mkd, options = "toc"))

# hard_wrap example
cat(renderMarkdown("foo\nbar\n"))
cat(renderMarkdown("foo\nbar\n", options = "hard_wrap"))

# latex math example
mkd <- c(
  "`$x$` is inline math $x$!", "", "Display style:", "", "$$x + y$$", "",
  "\\begin{eqnarray}
a^{2}+b^{2} & = & c^{2}\\\\
\\sin^{2}(x)+\\cos^{2}(x) & = & 1
\\end{eqnarray}"
)

cat(renderMarkdown(mkd))
cat(renderMarkdown(mkd, options = "-latex_math"))

# smartypants example
cat(renderMarkdown("1/2 (c)"))
cat(renderMarkdown("1/2 (c)", options = "-smartypants"))

mkd <- names(markdown:::pants)
mkd <- paste(c(mkd, paste0('`', mkd, '`')), collapse = ' ')
cat(renderMarkdown(mkd))
cat(renderMarkdown(mkd, options = "-smartypants"))

cat(smartypants("1/2 (c)\n"))

# tables example (need 4 spaces at beginning of line here)
cat(renderMarkdown("
First Header  | Second Header
------------- | -------------
Content Cell  | Content Cell
Content Cell  | Content Cell
"))

# but not here
cat(renderMarkdown("
First Header  | Second Header
------------- | -------------
Content Cell  | Content Cell
Content Cell  | Content Cell
", options = '-table'))

# autolink example
cat(renderMarkdown("https://www.r-project.org/"))
cat(renderMarkdown("https://www.r-project.org/", options = "-autolink"))

# strikethrough example
cat(renderMarkdown("~~awesome~~"))
cat(renderMarkdown("~~awesome~~", options = "-strikethrough"))

# superscript and subscript examples
cat(renderMarkdown("2^10^"))
cat(renderMarkdown("2^10^", options = "-superscript"))
cat(renderMarkdown("H~2~O"))
cat(renderMarkdown("H~2~O", options = "-subscript"))

# skip_html tags
mkd = '<style>a {}</style><script type="text/javascript">console.log("No!");</script>\n[Hello](#)'
cat(renderMarkdown(mkd))
# TODO: wait for https://github.com/r-lib/commonmark/issues/15 to be fixed
# cat(renderMarkdown(mkd, options = "tagfilter"))

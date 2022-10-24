library(markdown)

# toc example
mkd <- c("# Header 1", "p1", "## Header 2", "p2")

cat(renderMarkdown(text = mkd))
cat(renderMarkdown(text = mkd, options = "toc"))

# hard_wrap example
cat(renderMarkdown(text = "foo\nbar\n"))
cat(renderMarkdown(text = "foo\nbar\n", options = "hard_wrap"))

# latex math example
mkd <- c(
  "`$x$` is inline math $x$!", "", "Display style:", "", "$$x + y$$", "",
  "\\begin{eqnarray}
a^{2}+b^{2} & = & c^{2}\\\\
\\sin^{2}(x)+\\cos^{2}(x) & = & 1
\\end{eqnarray}"
)

cat(renderMarkdown(text = mkd))
cat(renderMarkdown(text = mkd, options = "-latex_math"))

# smartypants example
cat(renderMarkdown(text = "1/2 (c)"))
cat(renderMarkdown(text = "1/2 (c)", options = "-smartypants"))

mkd <- names(markdown:::pants)
mkd <- paste(c(mkd, paste0('`', mkd, '`')), collapse = ' ')
cat(renderMarkdown(text = mkd))
cat(renderMarkdown(text = mkd, options = "-smartypants"))

cat(smartypants("1/2 (c)\n"))

# tables example (need 4 spaces at beginning of line here)
cat(renderMarkdown(text = "
First Header  | Second Header
------------- | -------------
Content Cell  | Content Cell
Content Cell  | Content Cell
"))

# but not here
cat(renderMarkdown(text = "
First Header  | Second Header
------------- | -------------
Content Cell  | Content Cell
Content Cell  | Content Cell
", options = '-table'))

# autolink example
cat(renderMarkdown(text = "https://www.r-project.org/"))
cat(renderMarkdown(text = "https://www.r-project.org/", options = "-autolink"))

# strikethrough example
cat(renderMarkdown(text = "~~awesome~~"))
cat(renderMarkdown(text = "~~awesome~~", options = "-strikethrough"))

# superscript and subscript examples
cat(renderMarkdown(text = "2^10^"))
cat(renderMarkdown(text = "2^10^", options = "-superscript"))
cat(renderMarkdown(text = "H~2~O"))
cat(renderMarkdown(text = "H~2~O", options = "-subscript"))

# skip_html tags
mkd = '<style>a {}</style><script type="text/javascript">console.log("No!");</script>\n[Hello](#)'
cat(renderMarkdown(text = mkd))
# TODO: wait for https://github.com/r-lib/commonmark/issues/15 to be fixed
# cat(renderMarkdown(text = mkd, options = "tagfilter"))

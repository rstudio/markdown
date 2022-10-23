library(markdown)

# generate HTML fragments only
options(markdown.html.options = "fragment_only")

# toc example
mkd <- paste(c("# Header 1", "p1", "## Header 2", "p2"), collapse = "\n")

cat(markdownToHTML(text = mkd))
cat(markdownToHTML(text = mkd, options = "toc"))

# hard_wrap example
cat(markdownToHTML(text = "foo\nbar\n"))
cat(markdownToHTML(text = "foo\nbar\n", options = "hard_wrap"))

# smartypants example
cat(markdownToHTML(text = "1/2 (c)"))
cat(markdownToHTML(text = "1/2 (c)", options = "-smartypants"))

mkd <- names(markdown:::pants)
mkd <- paste(c(mkd, paste0('`', mkd, '`')), collapse = ' ')
cat(markdownToHTML(text = mkd))
cat(markdownToHTML(text = mkd, options = "-smartypants"))

cat(smartypants("1/2 (c)\n"))

# tables example (need 4 spaces at beginning of line here)
cat(markdownToHTML(text = "
First Header  | Second Header
------------- | -------------
Content Cell  | Content Cell
Content Cell  | Content Cell
"))

# but not here
cat(markdownToHTML(text = "
First Header  | Second Header
------------- | -------------
Content Cell  | Content Cell
Content Cell  | Content Cell
", options = '-table'))

# autolink example
cat(markdownToHTML(text = "https://www.r-project.org/"))
cat(markdownToHTML(text = "https://www.r-project.org/", options = "-autolink"))

# strikethrough example
cat(markdownToHTML(text = "~~awesome~~"))
cat(markdownToHTML(text = "~~awesome~~", options = "-strikethrough"))

# superscript and subscript examples
cat(markdownToHTML(text = "2^10^"))
cat(markdownToHTML(text = "2^10^", options = "-superscript"))
cat(markdownToHTML(text = "H~2~O"))
cat(markdownToHTML(text = "H~2~O", options = "-subscript"))

# skip_html tags
mkd = '<style>a {}</style><script type="text/javascript">console.log("No!");</script>\n[Hello](#)'
cat(markdownToHTML(text = mkd))
# TODO: wait for https://github.com/r-lib/commonmark/issues/15 to be fixed
# cat(markdownToHTML(text = mkd, options = "tagfilter"))

options(markdown.html.options = NULL)

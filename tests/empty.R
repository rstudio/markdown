library(markdown)
f = tempfile()
if (file.create(f)) {
   markdownToHTML(f, options = '-standalone')
   markdownToHTML(f)
   unlink(f)
}

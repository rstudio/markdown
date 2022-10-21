library(markdown)
f = tempfile()
if (file.create(f)) {
   markdownToHTML(f, options = 'fragment_only')
   markdownToHTML(f)
   unlink(f)
}

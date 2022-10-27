library(markdown)
f = tempfile()
if (file.create(f)) {
   mark_html(f, options = '-standalone')
   mark_html(f)
   unlink(f)
}

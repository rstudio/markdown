library(markdown)

# EXTENSIONS

# no_intra_emphasis example
cat(markdownToHTML(text='foo_bar_function',extensions=c()))
cat(markdownToHTML(text='foo_bar_function',extensions=c('no_intra_emphasis')))

# tables example (need 4 spaces at beginning of line here)
cat(markdownToHTML(text="
    First Header  | Second Header
    ------------- | -------------
    Content Cell  | Content Cell
    Content Cell  | Content Cell
",extensions=c()))

# but not here
cat(markdownToHTML(text="
First Header  | Second Header
------------- | -------------
Content Cell  | Content Cell
Content Cell  | Content Cell
",extensions=c('tables')))

# fenced_code example (need at lest three leading ~ or `)
cat(markdownToHTML(text="
~~~
preformated text here without having to indent
first line.
~~~
",extensions=c()))

cat(markdownToHTML(text="
```
preformated text here without having to indent
first line.
```
",extensions=c('fenced_code')))

# autolink example
cat(markdownToHTML(text="http://www.r-project.org/",extensions=c()))
cat(markdownToHTML(text="http://www.r-project.org/",
                   extensions=c('autolink')))

# strikethrough example
cat(markdownToHTML(text="~~awesome~~",extensions=c()))
cat(markdownToHTML(text="~~awesome~~",extensions=c('strikethrough')))

# lax_html_blocks
cat(markdownToHTML(text="
Embedding html without surrounding with empty newline.
<div>_markdown_</div>
extra text.
",extensions=c('')))
cat(markdownToHTML(text="
Embedding html without surrounding with empty newline.
<div>_markdown_</div>
extra text.
",extensions=c('lax_html_blocks')))

# space_headers example
cat(markdownToHTML(text="
#A Header
even though there is no space between # and A.
",extensions=c('')))
cat(markdownToHTML(text="
#Not A Header
because there is no space between # and N.
",extensions=c('space_headers')))

# superscript example
cat(markdownToHTML(text="2^10",extensions=c()))
cat(markdownToHTML(text="2^10",extensions=c('superscript')))

# HTML OPTIONS

# skip_html example
mkd='<style></style><img src=""><a href="">Hello</a>'
cat(markdownToHTML(text=mkd,options=c()))
cat(markdownToHTML(text=mkd,options=c('skip_html')))

# skip_style example
cat(markdownToHTML(text=mkd,options=c()))
cat(markdownToHTML(text=mkd,options=c('skip_style')))

# skip_images example
cat(markdownToHTML(text=mkd,options=c()))
cat(markdownToHTML(text=mkd,options=c('skip_images')))

# skip_links example
cat(markdownToHTML(text=mkd,options=c()))
cat(markdownToHTML(text=mkd,options=c('skip_links')))

# safelink example
cat(markdownToHTML(text='[foo](foo://bar "baz")',options=c('')))
cat(markdownToHTML(text='[foo](foo://bar "baz")',options=c('safelink')))

# toc example
mkd <- '
# Header 1
p1
## Header 2
p2
'
cat(markdownToHTML(text=mkd,options=c()))
cat(markdownToHTML(text=mkd,options=c('toc')))

# hard_wrap example
cat(markdownToHTML(text='foo\nbar\n',options=c('')))
cat(markdownToHTML(text='foo\nbar\n',options=c('hard_wrap')))

# use_xhtml example
cat(markdownToHTML(text='foo\nbar\n',options=c('hard_wrap')))
cat(markdownToHTML(text='foo\nbar\n',options=c('hard_wrap','use_xhtml')))

# escape example
mkd='<style></style><img src=""><a href="">Hello</a>'
cat(markdownToHTML(text=mkd,options=c('skip_html')))
# overrides all 'skip_*' options
cat(markdownToHTML(text=mkd,options=c('skip_html','escape')))

# smartypants example
cat(markdownToHTML(text='1/2 (c)',options=c('')))
cat(markdownToHTML(text='1/2 (c)',options=c('smartypants')))

cat(smartypants(text='1/2 (c)\n'))


rm(mkd)

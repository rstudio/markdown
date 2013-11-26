\name{markdownExtensions}
\alias{markdownExtensions}
\title{Markdown extensions}
\usage{
markdownExtensions()
}
\value{
A \code{character} vector listing all available extensions.
}
\description{
\code{markdownExtensions} returns a character vector listing all the
extensions that are available in the \pkg{markdown} package.
}
\details{
They are all ON by default.

The \pkg{Sundown} library (upon which \pkg{markdown} is built) has optional
support for several extensions described below. To turn these on globally in
the \pkg{markdown} package, simply place some or all of them in a character
vector and assign to the global option \code{markdown.extensions} like so:

\code{options(markdown.extensions = markdownExtensions())}

To override the global option, pass the \code{extensions} as an argument to
one of the render functions, e.g.:

\code{markdownToHTML(..., extensions = c('no_intra_emphasis'))}

Description of all extensions:

\describe{

\item{\code{'no_intra_emphasis'}}{ skip markdown embedded in words.  }

\item{\code{'tables'}}{ create HTML tables (see Examples). }

\item{\code{'fenced_code'}}{ treat text as verbatim when surrounded with
begin and ending lines with three ~ or \emph{`} characters.  }

\item{\code{'autolink'}}{ create HTML links from urls and email addresses. }

\item{\code{'strikethrough'}}{ create strikethroughs by surrounding text with
~~.  }

\item{\code{'lax_spacing'}}{ allow HTML tags inside paragraphs without being
surrounded by newlines.  }

\item{\code{'space_headers'}}{ add a space between header hashes and the
header itself.  }

\item{\code{'superscript'}}{ translate ^ and subsequent text into HTML
superscript. }

\item{\code{'latex_math'}}{ transforms all math equations into syntactically
correct MathJax equations.  }

}

See the EXAMPLES section to see the output of each extension turned on or
off.
}
\examples{
# List all available extensions:
markdownExtensions()

# To turn on all markdown extensions globally:
options(markdown.extensions = markdownExtensions())

# To turn off all markdown extensions globally:
options(markdown.extensions = NULL)
# The following examples are short, so we set the HTML option 'fragment_only'

options(markdown.HTML.options = "fragment_only")

# no_intra_emphasis example
cat(markdownToHTML(text = "foo_bar_function", extensions = c()))
cat(markdownToHTML(text = "foo_bar_function", extensions = c("no_intra_emphasis")))

# tables example (need 4 spaces at beginning of line here)
cat(markdownToHTML(text = "
    First Header  | Second Header
    ------------- | -------------
    Content Cell  | Content Cell
    Content Cell  | Content Cell
", extensions = c()))

# but not here
cat(markdownToHTML(text = "
First Header  | Second Header
------------- | -------------
Content Cell  | Content Cell
Content Cell  | Content Cell
", extensions = c("tables")))

# fenced_code example (need at least three leading ~ or `)
fenced_block <- function(text, x = "`", n = 3) {
  fence <- paste(rep(x, n), collapse = "")
  paste(fence, text, fence, sep = "")
}
cat(markdownToHTML(text = fenced_block("
preformatted text here without having to indent
first line.
"), extensions = c()))

cat(markdownToHTML(text = fenced_block("
preformatted text here without having to indent
first line.
"), extensions = c("fenced_code")))

# autolink example
cat(markdownToHTML(text = "http://www.r-project.org/", extensions = c()))
cat(markdownToHTML(text = "http://www.r-project.org/", extensions = c("autolink")))

# strikethrough example
cat(markdownToHTML(text = "~~awesome~~", extensions = c()))
cat(markdownToHTML(text = "~~awesome~~", extensions = c("strikethrough")))

# lax_spacing
cat(markdownToHTML(text = "
Embedding html without surrounding with empty newline.
<div>_markdown_</div>
extra text.
", extensions = c("")))
cat(markdownToHTML(text = "
Embedding html without surrounding with empty newline.
<div>_markdown_</div>
extra text.
", extensions = c("lax_spacing")))

# space_headers example
cat(markdownToHTML(text = "#A Header\\neven though there is no space between # and A",
                   extensions = c("")))
cat(markdownToHTML(text = "#Not A Header\\nbecause there is no space between # and N",
                   extensions = c("space_headers")))

# superscript example
cat(markdownToHTML(text = "2^10", extensions = c()))
cat(markdownToHTML(text = "2^10", extensions = c("superscript")))
}
\seealso{
\link{markdownHTMLOptions}
}

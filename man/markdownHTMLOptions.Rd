\name{markdownHTMLOptions}
\alias{markdownHTMLOptions}
\title{Markdown HTML rendering options}
\usage{
markdownHTMLOptions(defaults = FALSE)
}
\arguments{
  \item{defaults}{If \code{TRUE}, then only the default
  options are returned.  Otherwise all options are
  returned.}
}
\value{
A \code{character} vector listing either all available options or
  just the default options.
}
\description{
\code{markdownHTMLOptions} returns a character vector listing all the options
that are available for the HTML renderer in the \pkg{markdown} package. As a
convenience, the package default options were chosen to render well-formed
stand-alone HTML pages. The default options are \code{'use_xhtml'},
\code{'smartypants'}, \code{'base64_images'}, \code{'mathjax'}, and
\code{'highlight_code'}.
}
\details{
The HTML renderer provides several options described below. To turn these on
globally in the \pkg{markdown} package, simply place some or all of them in a
character vector and assign to the global option \code{markdown.HTML.options}
like so:

\code{options(markdown.HTML.options = markdownHTMLOptions())}

To reset the options to package default, use:

\code{options(markdown.HTML.options = markdownHTMLOptions(default = TRUE))}

To override the global option, pass the \code{options} as an argument:

\code{markdownToHTML(..., options = c('skip_images'))}

Description of all options:

\describe{

\item{\code{'skip_html'}}{ suppress output of all HTML tags in the document.}

\item{\code{'skip_style'}}{ suppress output of HTML style tags.}

\item{\code{'skip_images'}}{ suppress output of HTML image tags.}

\item{\code{'skip_links'}}{ suppress output of HTML anchor tags.}

\item{\code{'safelink'}}{ only create links for known url types, e.g. http,
ftp, http, etc.}

\item{\code{'toc'}}{ assigns an HTML id to each header of the form 'toc_%d'
where '%d' is replaced with the position of the header within the document
(starting at 0). The user must create the top level table of contents by
hand.}

\item{\code{'hard_wrap'}}{ adds an HTML br tag for every newline (excluding
trailing) found within a paragraph.}

\item{\code{'use_xhtml'}}{ create XHMTL 1.0 compliant HTML tags.}

\item{\code{'escape'}}{ escape all HTML found within the \emph{markdown}.
Overrides all of the \code{'skip_*'} options mentioned above.}

\item{\code{'smartypants'}}{ translates plain ASCII punctuation characters
into \emph{smart} typographic punctuation HTML entities. }

\item{\code{'fragment_only'}}{ eliminates the inclusion of any HTML header or
body tags, CSS, or Javascript components. }

\item{\code{'base64_images'}}{ Any local images linked with the
\code{'<img>'} tag to the output HTML will automatically be converted to
base64 and included along with output. }

\item{\code{'mathjax'}}{ includes appropriate Javascript libraries to render
math markup.}

\item{\code{'highlight_code'}}{ includes appropriate Javascript libraries to
highlight code chunks.}

}

See the EXAMPLES section to see the output of each option turned on or off.
}
\examples{
# List all available extensions:
markdownHTMLOptions()

# To turn on all HTML options globally:
options(markdown.HTML.options = markdownHTMLOptions())

# To turn off all HTML options globally:
options(markdown.HTML.options = NULL)

# To turn on package default HTML options globally:
options(markdown.HTML.options = markdownHTMLOptions(default = TRUE))
# HTML OPTIONS

# The following examples are short, so we allways add the HTML option
# 'fragment_only'
tOpt <- "fragment_only"

# skip_html example
mkd = "<style></style><img src=\"\"><a href=\"\">Hello</a>"
cat(markdownToHTML(text = mkd, options = c(tOpt)))
cat(markdownToHTML(text = mkd, options = c(tOpt, "skip_html")))

# skip_style example
cat(markdownToHTML(text = mkd, options = c(tOpt)))
cat(markdownToHTML(text = mkd, options = c(tOpt, "skip_style")))

# skip_images example
cat(markdownToHTML(text = mkd, options = c(tOpt)))
cat(markdownToHTML(text = mkd, options = c(tOpt, "skip_images")))

# skip_links example
cat(markdownToHTML(text = mkd, options = c(tOpt)))
cat(markdownToHTML(text = mkd, options = c(tOpt, "skip_links")))

# safelink example
cat(markdownToHTML(text = "[foo](foo://bar \"baz\")", options = c(tOpt)))
cat(markdownToHTML(text = "[foo](foo://bar \"baz\")", options = c(tOpt, "safelink")))

# toc example
mkd <- paste(c("# Header 1", "p1", "## Header 2", "p2"), collapse = "\\n")

cat(markdownToHTML(text = mkd, options = c(tOpt)))
cat(markdownToHTML(text = mkd, options = c(tOpt, "toc")))

# hard_wrap example
cat(markdownToHTML(text = "foo\\nbar\\n", options = c(tOpt)))
cat(markdownToHTML(text = "foo\\nbar\\n", options = c(tOpt, "hard_wrap")))

# use_xhtml example
cat(markdownToHTML(text = "foo\\nbar\\n", options = c(tOpt, "hard_wrap")))
cat(markdownToHTML(text = "foo\\nbar\\n", options = c(tOpt, "hard_wrap", "use_xhtml")))

# escape example
mkd = "<style></style><img src=\"\"><a href=\"\">Hello</a>"
cat(markdownToHTML(text = mkd, options = c(tOpt, "skip_html")))
# overrides all 'skip_*' options
cat(markdownToHTML(text = mkd, options = c(tOpt, "skip_html", "escape")))

# smartypants example
cat(markdownToHTML(text = "1/2 (c)", options = c(tOpt)))
cat(markdownToHTML(text = "1/2 (c)", options = c(tOpt, "smartypants")))

cat(smartypants(text = "1/2 (c)\\n"))
}
\seealso{
\link{markdownToHTML}
}

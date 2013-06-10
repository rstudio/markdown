\name{markdownToHTML}
\alias{markdownToHTML}
\title{Render markdown to HTML}
\usage{
markdownToHTML(file, output, text, options = getOption("markdown.HTML.options"), 
    extensions = getOption("markdown.extensions"), title = "", 
    stylesheet = getOption("markdown.HTML.stylesheet"), 
    header = getOption("markdown.HTML.header", ""), fragment.only = FALSE)
}
\arguments{
  \item{file}{a character string giving the pathname of the
  file to read from.  If it is omitted from the argument
  list, then it is presumed that the \code{text} argument
  will be used instead.}

  \item{output}{a character string giving the pathname of
  the file to write to.  If it is omitted, then it is
  presumed that the use expects the results returned as a
  \code{"raw"} vector.}

  \item{text}{a character vector containing the
  \emph{markdown} text to transform.}

  \item{options}{options that are passed to the renderer.
  see \code{\link{markdownHTMLOptions}}.}

  \item{extensions}{options that are passed to the
  \emph{markdown} engine. See
  \code{\link{markdownExtensions}}.}

  \item{title}{The HTML title.}

  \item{stylesheet}{either valid CSS or a file containing
  CSS. will be included in the output.}

  \item{header}{either valid HTML or a file containing HTML
  will be included in the header of the output.}

  \item{fragment.only}{Whether or not to produce an HTML
  fragment without the HTML header and body tags, CSS, and
  Javascript components.}
}
\value{
  \code{renderMarkdown} returns NULL invisibly when output
  is to a file, and a \code{"character"} vector otherwise.
}
\description{
  \code{markdownToHTML} transforms the \emph{markdown} text
  provided by the user in either the \code{file} or
  \code{text} variable. The HTML transformation is either
  written to the \code{output} file or returned to the user
  as a \code{"character"} vector.
}
\details{
  Three notable HTML options have been added to support
  collaborative reproducible research. They are as follows:

  \itemize{

  \item Latex math expressions enclosed by one of the block
  level syntaxes, $latex ... $ , $$ ... $$, or \[ ... \],
  or one of the inline syntaxes, $...$, or \( ... \), will
  be rendered in real-time by the MathJax Javascript
  library.

  \item \emph{R} code blocks enclosed between \verb{```r}
  and \verb{```} will automatically be syntax highlighted.

  \item Any local images linked using the <img> tag will be
  base64 encoded and included in the output HTML.

  }

  See the DETAILS section below and
  \code{\link{markdownHTMLOptions}} for more information.

  There are two basic modes to \code{markdownToHTML}
  determined by the value of the \code{fragment.only}
  argument:

  When \code{FALSE}, \code{markdownToHTML} creates
  well-formed stand-alone HTML pages complete with HTML
  header, title, and body tags. The default template used
  for this mode may be found here:

  system.file('resources/markdown.html',package='markdown')

  Also, \code{markdownToHTML} will automatically determine
  whether or not mathjax and R code highlighting are needed
  and will include the appropriate Javascript libraries in
  the output. Thus, there's no need to explicitly set the
  \code{'mathjax'} or \code{'highlight_code'} options (see
  \code{\link{markdownHTMLOptions}} for more details).

  When \code{fragment.only} is TRUE, nothing extra is
  added.
}
\examples{
print(markdownToHTML(text = "Hello World!"))
}
\seealso{
  \code{\link{markdownExtensions}},
  \code{\link{markdownHTMLOptions}},
  \code{\link{renderMarkdown}}.
}

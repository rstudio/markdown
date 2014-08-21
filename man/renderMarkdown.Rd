\name{renderMarkdown}
\alias{renderMarkdown}
\title{Render markdown to an HTML fragment}
\usage{
renderMarkdown(file, output = NULL, text = NULL, renderer = "HTML", 
    renderer.options = NULL, extensions = getOption("markdown.extensions"))
}
\arguments{
\item{renderer}{the name of the renderer that will be used to transform the
\code{file} or \code{text}.}

\item{renderer.options}{options that are passed to the renderer.  For
\code{HTML} renderer options see \code{\link{markdownHTMLOptions}}.}

\item{file}{a character string giving the pathname of the file to read from.
If it is omitted from the argument list, then it is presumed that the
\code{text} argument will be used instead.}

\item{output}{a character string giving the pathname of the file to write to.
If it is omitted (\code{NULL}), then it is presumed that the user expects
the results returned as a \code{character} vector.}

\item{text}{a character vector containing the \emph{markdown} text to
transform (each element of this vector is treated as a line in a file).}

\item{extensions}{options that are passed to the \emph{markdown} engine. See
\code{\link{markdownExtensions}}.}
}
\value{
\code{renderMarkdown} returns NULL invisibly when output is to a
  file, and either \code{character} or \code{raw} vector depending on the
  renderer output type.
}
\description{
\code{renderMarkdown} transforms the \emph{markdown} text provided by the
user in either the \code{file} or \code{text} variable. The transformation is
either written to the \code{output} file or returned to the user. The default
rendering target is "HTML".
}
\details{
\pkg{markdown} uses (and ships with) the popular Sundown library provided by
GitHub. C stubs are available to implement new renderers.
}
\examples{
renderMarkdown(text = "Hello World!")
}
\seealso{
\code{\link{markdownExtensions}}, \code{\link{markdownHTMLOptions}},
  \code{\link{markdownToHTML}}.

  For a description of the original \emph{markdown} version:

  \url{http://daringfireball.net/projects/markdown/}

  The original Sundown library on github:

  \url{https://github.com/vmg/sundown}

  C stubs for writing new renders are in inst/include/markdown_rstubs.[ch].
}

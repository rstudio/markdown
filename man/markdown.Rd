\docType{package}
\name{markdown}
\alias{markdown}
\alias{markdown-package}
\title{Markdown rendering for R}
\description{
\pkg{Markdown} is a plain-text formatting syntax that can be converted to
XHTML or other formats. This package provides R bindings to the Sundown
(\url{https://github.com/vmg/sundown}) markdown rendering library.
}
\details{
The R function \code{\link{markdownToHTML}} renders a markdown file to HTML
(respecting the specified \code{\link{markdownExtensions}} and
\code{\link{markdownHTMLOptions}}).

The package also exports the underlying Sundown C extension API which enables
creating and calling custom renderers using the \code{\link{renderMarkdown}}
function.

To learn more about markdown syntax see:

\url{http://en.wikipedia.org/wiki/Markdown}
}
\author{
JJ Allaire, Jeffrey Horner, Vicent Marti, and Natacha Porte

  Maintainer: Yihui Xie <xie@yihui.name>
}
\seealso{
\code{\link{markdownToHTML}} \code{\link{renderMarkdown}}
}
\keyword{package}

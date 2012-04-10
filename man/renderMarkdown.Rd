\name{renderMarkdown}
\alias{renderMarkdown}
\title{Render markdown}
\description{
   \code{renderMarkdown} transforms the \emph{markdown} text provided by the
   user in either the \code{file} or \code{text} variable. The transformation is
   either written to the \code{output} file or returned to the user as a 
   \code{"raw"} vector.

   The default rendering target is "HTML".
}
\usage{
renderMarkdown(file, output, text, renderer='HTML',
               renderer.options=NULL,
               extensions=getOption('markdown.extensions'))
}
\arguments{
   \item{file}{a character string giving the pathname of the file to
      read from. If it is omitted from the argument list, then it is
      presumed that the \code{text} argument will be used instead.}
   \item{output}{a character string giving the pathname of the file to
      write to. If it is omitted, then it is presumed that the user expects
      the results returned as a \code{"raw"} vector.}
   \item{text}{a character vector containing the \emph{markdown} text to
      transform.}
   \item{renderer}{the name of the renderer that will be used to transform
      the \code{file} or \code{text}.}
   \item{renderer.options}{
      options that are passed to the renderer. 
      For \code{HTML} renderer options see 
      \code{\link{markdownHTMLOptions}}.}
   \item{extensions}{
      options that are passed to the \emph{markdown}
      engine. See \code{\link{markdownExtensions}}.}
}
\value{
   \code{renderMarkdown} returns NULL invisibly when output is to a file, and
   a \code{"raw"} vector otherwise.
}
\details{
   \code{markdown} uses (and ships with) the popular Sundown library provided
   by GitHub. C stubs are available to implement new renderers.
}
\seealso{
   \code{\link{markdownExtensions}}, \code{\link{markdownHTMLOptions}},
   \code{\link{markdownToHTML}}.

   For a description of the original \emph{markdown} 
   version:

   \url{http://daringfireball.net/projects/markdown/}

   The original Sundown library on github:

   \url{https://github.com/tanoku/sundown}

   C stubs for writing new renders are in inst/include/markdown_rstubs.[ch].
}

\examples{

rawToChar(renderMarkdown(text='Hello World!'))

}
\keyword{programming}

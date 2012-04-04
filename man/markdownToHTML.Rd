\name{markdownToHTML}
\alias{markdownToHTML}
\title{Render markdown to HTML}
\description{
   \code{markdownToHTML} transforms the \emph{markdown} text provided by the
   user in either the \code{file} or \code{text} variable. The HTML 
   transformation is either written to the \code{output} file or returned to
   the user as a \code{"character"} vector.
}
\usage{
markdownToHTML(file, output, text, 
               options=getOption('markdown.HTML.options'),
               extensions=getOption('markdown.extensions'))
}
\arguments{
   \item{file}{a character string giving the pathname of the file to
      read from. If it is omitted from the argument list, then it is
      presumed that the \code{text} argument will be used instead.}
   \item{output}{a character string giving the pathname of the file to
      write to. If it is omitted, then it is presumed that the use expects
      the results returned as a \code{"raw"} vector.}
   \item{text}{a character vector containing the \emph{markdown} text to
      transform.}
   \item{options}{options that are passed to the renderer. 
      see \code{\link{markdownHTMLOptions}}.}
   \item{extensions}{options that are passed to the \emph{markdown} engine. See
      \code{\link{markdownExtensions}}.}
}
\value{
   \code{renderMarkdown} returns NULL invisibly when output is to a file, and
   a \code{"character"} vector otherwise.
}
\seealso{
   \code{\link{markdownExtensions}}, \code{\link{markdownHTMLOptions}},
   \code{\link{renderMarkdown}}.
}

\examples{

print(markdownToHTML(text='Hello World!'))

}
\keyword{programming}

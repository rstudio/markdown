\name{smartypants}
\alias{smartypants}
\title{smartypants: ASCII punctuation to HTML entities}
\description{
   \code{smartypants} transforms plain ASCII punctuation characters into
   \emph{smart} tyopgraphic punctuation HTML entities.
}
\usage{
smartypants(file, output, text)
}
\arguments{
   \item{file}{a character string giving the pathname of the file to
      read from. If it is omitted from the argument list, then it is
      presumed that the \code{text} argument will be used instead.}
   \item{output}{a character string giving the pathname of the file to
      write to. If it is omitted, then it is presumed that the user expects
      the results returned as a character string.}
   \item{text}{a character vector containing the \emph{markdown} text to
      transform.}
}
\value{
   \code{smartypants} returns NULL invisibly when output is to a file, and
   a character string otherwise.
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

cat(smartypants(text='1/2 (c)\n'))

}
\keyword{programming}

\name{markdownExtensions}
\alias{markdownExtensions}
\title{Markdown Exensions defined}
\description{
   \code{markdownExensions} returns a character vector listing all the extensions that are available in the \code{markdown} package.
}
\usage{
markdownExensions()
}
\arguments{
   None.
}
\details{
}
\value{
   A \code{"character"} vector listing all the extensions.
}
\examples{

# List all available extensions:
markdownExtensions()

# To turn on all markdown extensions:
\dontrun{options(markdown.extensions=markdownExtensions())}

}
\keyword{programming}

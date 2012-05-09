\name{registeredRenderers}
\alias{registeredRenderers}
\title{List of Registered Markdown Renderers}
\description{
   \code{registeredRenderers} returns a named character vector listing all the 
   registered renderers known to the \code{markdown} package.

   \code{markdown} allows up to seven renderers to be registered by users; HTML is provided by the package.

}

\usage{
registeredRenderers()
}

\arguments{
   None.
}
\value{

   A named \code{"character"} vector listing all available
   renderers. Vector value contain renderer names, and named values
   contain the renderer output type, either \code{"character"} or
   \code{"raw"}.

}

\seealso{
   \link{markdownToHTML}, \link{rendererOutputType}
}

\examples{

# List all available renderers
registeredRenderers()

}
\keyword{programming}

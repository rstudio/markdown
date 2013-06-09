\name{registeredRenderers}
\alias{registeredRenderers}
\title{List of Registered Markdown Renderers}
\usage{
registeredRenderers()
}
\value{
  A named \code{"character"} vector listing all available
  renderers.  Vector value contain renderer names, and
  named values contain the renderer output type, either
  \code{"character"} or \code{"raw"}.
}
\description{
  \code{registeredRenderers} returns a named character
  vector listing all the registered renderers known to the
  \pkg{markdown} package. \pkg{markdown} allows up to seven
  renderers to be registered by users; HTML is provided by
  the package.
}
\examples{
# List all available renderers
registeredRenderers()
}
\seealso{
  \link{markdownToHTML}, \link{rendererOutputType}
}

\name{rendererOutputType}
\alias{rendererOutputType}
\title{Fetch the Renderer Output Type}
\usage{
rendererOutputType(name)
}
\arguments{
  \item{name}{a character string naming the renderer.}
}
\value{
  The character string with a value of either
  \code{character} or \code{raw}.
}
\description{
  \pkg{markdown} allows up to seven renderers to be
  registered by users, and each must provide the type of
  output returned, either \code{character} or \code{raw}
  for binary output. HTML is provided by the package and
  outputs \code{character}.
}
\examples{
# List all available renderers
rendererOutputType("HTML")
}
\seealso{
  \link{markdownToHTML}, \link{registeredRenderers}
}

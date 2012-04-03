\name{markdownHTMLOptions}
\alias{markdownHTMLOptions}
\title{Options for Markdown to HTML Renderer}
\description{
   \code{markdownHTMLOptions} returns a character vector listing all the extensions that are available for the HTML renderer in the \code{markdown} package.

   They are all OFF by default.
}
\usage{
markdownHTMLOptions()
}
\arguments{
   None.
}
\details{
   The HTML renderer provides several options described below. To turn
   these on globally in the \emph{markdown} package, simply place some or
   all of them in a character vector and assign to the global option
   \code{markdown.HTML.options} like so:

   \code{options(markdown.HTML.options=markdownHTMLOptions())}

   To override the global option, pass the \code{options} as an argument:

   \code{markdownToHTML(...,options=c('skip_images'))}

   Description of all options:

   \describe{
      \item{\code{'skip_html'}}{}
      \item{\code{'skip_style'}}{}
      \item{\code{'skip_images'}}{}
      \item{\code{'skip_links'}}{}
      \item{\code{'expand_tabs'}}{}
      \item{\code{'safelink'}}{}
      \item{\code{'toc'}}{}
      \item{\code{'hard_wrap'}}{}
      \item{\code{'use_xhtml'}}{}
      \item{\code{'escape'}}{}
   }

   See the EXAMPLES section to see the output of each option turned on
   or off.
}

\value{
   A \code{"character"} vector listing all available options.
}

\seealso{
   \link{markdownToHTML}   
}

\examples{

# List all available extensions:
markdownToHTML()

\dontrun{
# To turn on all HTML options globally:
options(markdown.HTML.options=markdownHTMLOptions())

# To turn off all HTML options globally:
options(markdown.HTML.options=NULL)

}

}
\keyword{programming}

\name{markdownHTMLOptions}
\alias{markdownHTMLOptions}
\title{Markdown HTML rendering options}
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
      \item{\code{'skip_html'}}{
         suppress output of all HTML tags in the document.}

      \item{\code{'skip_style'}}{
         suppress output of HTML style tags.}

      \item{\code{'skip_images'}}{
         suppress output of HTML image tags.}

      \item{\code{'skip_links'}}{
         supress output of HTML achor tags.}

      \item{\code{'safelink'}}{
         only create links for known url types, e.g. http, ftp, http, etc.}

      \item{\code{'toc'}}{
         assigns an HTML id to each header of the form 'toc_%d'
         where '%d' is replaced with the position of the header within
         the document (starting at 0). The user must create the top level
         table of contents by hand.}

      \item{\code{'hard_wrap'}}{
         adds an HTML br tag for every newline (excluding trailing) found
         within a paragraph.}
      \item{\code{'use_xhtml'}}{
         create XHMTL 1.0 compliant HTML tags.}
      \item{\code{'escape'}}{
         escape all HTML found within the \emph{markdown}. Overrides all
         of the \code{'skip_*'} options mentioned above.}
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
markdownHTMLOptions()

\dontrun{
# To turn on all HTML options globally:
options(markdown.HTML.options=markdownHTMLOptions())

# To turn off all HTML options globally:
options(markdown.HTML.options=NULL)
}

# skip_html example
mkd='<style></style><img src=""><a href="">Hello</a>'
cat(markdownToHTML(text=mkd,options=c()))
cat(markdownToHTML(text=mkd,options=c('skip_html')))

# skip_style example
cat(markdownToHTML(text=mkd,options=c()))
cat(markdownToHTML(text=mkd,options=c('skip_style')))

# skip_images example
cat(markdownToHTML(text=mkd,options=c()))
cat(markdownToHTML(text=mkd,options=c('skip_images')))

# skip_links example
cat(markdownToHTML(text=mkd,options=c()))
cat(markdownToHTML(text=mkd,options=c('skip_links')))

# safelink example
cat(markdownToHTML(text='[foo](foo://bar "baz")',options=c('')))
cat(markdownToHTML(text='[foo](foo://bar "baz")',options=c('safelink')))

# toc example
mkd <- '
# Header 1
p1
## Header 2
p2
'
cat(markdownToHTML(text=mkd,options=c()))
cat(markdownToHTML(text=mkd,options=c('toc')))

# hard_wrap example
cat(markdownToHTML(text='foo\nbar\n',options=c('')))
cat(markdownToHTML(text='foo\nbar\n',options=c('hard_wrap')))

# use_xhtml example
cat(markdownToHTML(text='foo\nbar\n',options=c('hard_wrap')))
cat(markdownToHTML(text='foo\nbar\n',options=c('hard_wrap','use_xhtml')))

# escape example
mkd='<style></style><img src=""><a href="">Hello</a>'
cat(markdownToHTML(text=mkd,options=c('skip_html')))
# overrides all 'skip_*' options
cat(markdownToHTML(text=mkd,options=c('skip_html','escape')))

rm(mkd)

}
\keyword{programming}

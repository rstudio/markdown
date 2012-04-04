\name{markdownExtensions}
\alias{markdownExtensions}
\title{Markdown exensions}
\description{
   \code{markdownExensions} returns a character vector listing all the extensions that are available in the \code{markdown} package.

   They are all ON by default.
}
\usage{
markdownExtensions()
}
\arguments{
   None.
}
\details{
   The \emph{Sundown} library (upon which \emph{markdown} is built) has
   optional support for several extensions described below. To turn
   these on globally in the \emph{markdown} package, simply place some or
   all of them in a character vector and assign to the global option
   \code{markdown.extensions} like so:

   \code{options(markdown.extensions=markdownExtensions())}

   To override the global option, pass the \code{extensions} as an argument
   to one of the render functions, e.g.:

   \code{markdownToHTML(...,extensions=c('no_intra_emphasis'))}

   Description of all extensions:

   \describe{
      \item{\code{'no_intra_emphasis'}}{
         skip markdown embedded in words.
      }
       
      \item{\code{'tables'}}{
         rewrites the following table with the HTML \emph{table} tag
         rather than verbatim:
         \preformatted{
First Header  | Second Header
------------- | -------------
Content Cell  | Content Cell
Content Cell  | Content Cell
         }
      }

      \item{\code{'fenced_code'}}{ treat text as verbim when surrounded with
         begin and ending lines with three ~ or \emph{`} characters.
      }

      \item{\code{'autolink'}}{
         create HTML links from urls and email addresses.
      }

      \item{\code{'strikethrough'}}{
         create strikethrouhs by surrounding text with ~~.
      }

      \item{\code{'lax_html_blocks'}}{
         allow HTML tags inside paragraphs without being surrounded 
         by newlines.
      }

      \item{\code{'space_headers'}}{
         add a space between header hashes and the header itself.
      }

      \item{\code{'superscript'}}{
         translate ^ and subsequent text into HTML superscript.
      }

   }

   See the EXAMPLES section to see the output of each extension turned on
   or off.
}

\value{
   A \code{"character"} vector listing all available extensions.
}

\seealso{
   \link{markdownHTMLOptions}   
}
\examples{

# List all available extensions:
markdownExtensions()

\dontrun{
# To turn on all markdown extensions globally:
options(markdown.extensions=markdownExtensions())

# To turn off all markdown extensions globally:
options(markdown.extensions=NULL)

}

# no_intra_emphasis example
cat(markdownToHTML(text='foo_bar_function',extensions=c()))
cat(markdownToHTML(text='foo_bar_function',
                   extensions=c('no_intra_emphasis')))

# tables example (need 4 spaces at beginning of line here)
cat(markdownToHTML(text="
    First Header  | Second Header
    ------------- | -------------
    Content Cell  | Content Cell
    Content Cell  | Content Cell
",extensions=c()))

# but not here
cat(markdownToHTML(text="
First Header  | Second Header
------------- | -------------
Content Cell  | Content Cell
Content Cell  | Content Cell
",extensions=c('tables')))

# fenced_code example (need at lest three leading ~ or `)
cat(markdownToHTML(text="
~~~
preformated text here without having to indent
first line.
~~~
",extensions=c()))

cat(markdownToHTML(text="
```
preformated text here without having to indent
first line.
```
",extensions=c('fenced_code')))

# autolink example
cat(markdownToHTML(text="http://www.r-project.org/",extensions=c()))
cat(markdownToHTML(text="http://www.r-project.org/",
                   extensions=c('autolink')))

# strikethrough example
cat(markdownToHTML(text="~~awesome~~",extensions=c()))
cat(markdownToHTML(text="~~awesome~~",extensions=c('strikethrough')))

# lax_html_blocks
cat(markdownToHTML(text="
Embedding html without surrounding with empty newline.
<div>_markdown_</div>
extra text.
",extensions=c('')))
cat(markdownToHTML(text="
Embedding html without surrounding with empty newline.
<div>_markdown_</div>
extra text.
",extensions=c('lax_html_blocks')))

# space_headers example
cat(markdownToHTML(text="
#A Header
even though there is no space between # and A.
",extensions=c('')))
cat(markdownToHTML(text="
#Not A Header
because there is no space between # and N.
",extensions=c('space_headers')))

# superscript example
cat(markdownToHTML(text="2^10",extensions=c()))
cat(markdownToHTML(text="2^10",extensions=c('superscript')))

}
\keyword{programming}

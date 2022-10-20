# renderMarkdown.R
#
# Copyright (C) 2009-2022 by RStudio, PBC
#
# This program is licensed to you under the terms of version 2 of the
# GNU General Public License. This program is distributed WITHOUT ANY
# EXPRESS OR IMPLIED WARRANTY, INCLUDING THOSE OF NON-INFRINGEMENT,
# MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. Please refer to the
# GPL (https://www.gnu.org/licenses/gpl-2.0.txt) for more details.


#' List of Registered Markdown Renderers
#'
#' \code{registeredRenderers} returns a named character vector listing all the
#' registered renderers known to the \pkg{markdown} package.
#' @return A named \code{character} vector listing all available renderers.
#'   Vector value contain renderer names, and named values contain the renderer
#'   output type, either \code{character} or \code{raw}.
#' @seealso \link{markdownToHTML}, \link{rendererOutputType}
#' @export
#' @keywords internal
#' @examples
#' # List all available renderers
#' registeredRenderers()
registeredRenderers <- function() c(character = 'HTML')


#' Testing for existence of a Markdown renderer
#'
#' \code{rendererExists} determines whether or not a certain renderer exists in
#' the Markdown library.
#' @param name name of renderer.
#' @return \code{TRUE} or \code{FALSE} for whether or not the renderer exists.
#' @export
#' @keywords internal
#' @examples rendererExists("HTML")
rendererExists <- function(name) name[1] %in% registeredRenderers()


#' Render Markdown to an HTML fragment
#'
#' \code{renderMarkdown} transforms the \emph{Markdown} text provided by the
#' user in either the \code{file} or \code{text} variable. The transformation is
#' either written to the \code{output} file or returned to the user. The default
#' rendering target is "HTML".
#' @inheritParams markdownToHTML
#' @param renderer the name of the renderer that will be used to transform the
#'   \code{file} or \code{text}.
#' @param renderer.options options that are passed to the renderer.  For
#'   \code{HTML} renderer options see \code{\link{markdownHTMLOptions}}.
#' @return \code{renderMarkdown} returns NULL invisibly when output is to a
#'   file, and either \code{character} (with the UTF-8 encoding) or \code{raw}
#'   vector depending on the renderer output type.
#' @seealso \code{\link{markdownExtensions}}, \code{\link{markdownHTMLOptions}},
#'   \code{\link{markdownToHTML}}.
#'
#'   For a description of the original \emph{Markdown} version:
#'   \url{https://daringfireball.net/projects/markdown/}
#' @export
#' @examples
#' (renderMarkdown(text = "Hello World!"))
#' # a few corner cases
#' (renderMarkdown(text = character(0)))
#' (renderMarkdown(text = ''))
renderMarkdown <- function(
  file, output = NULL, text = NULL, renderer = 'HTML', renderer.options = NULL,
  extensions = getOption('markdown.extensions'), encoding = 'UTF-8'
) {

  if (!rendererExists(renderer))
    stop("Renderer '", renderer, "' is not available.!")

  # Input from either a file or character vector
  if (!is.character(text)) {
    # If input is file, assume the encoding is UTF-8.
    text <- xfun::read_utf8(file, error = TRUE)
  }
  text <- enc2utf8(text)
  if (length(text) > 1) text <- paste(text, collapse = '\n')
  file <- NULL

  # Options
  if (is.null(renderer.options))
    renderer.options <- getOption(paste('markdown', renderer, 'options', sep = '.'))

  # HTML options must be a character vector.
  if (renderer == 'HTML') {
    if (!is.null(renderer.options) && !is.character(renderer.options))
      stop('HTML options must be a character vector')
  }

  if (length(text) == 0 || text == '') {
     if (is.null(output)) return(invisible(character(length(text))))
     file.create(output)
     return()
  }
  ret <- .Call(rmd_render_markdown,
               file, output, text, renderer, renderer.options, extensions)

  invisible(ret)
}

.b64EncodeFile <- function(inFile) {
  fileSize <- file.info(inFile)$size

  if (fileSize <= 0) {
    warning(inFile, 'is empty!')
    return(inFile)
  }
  paste( 'data:', mime::guess_type(inFile), ';base64,',
         .Call(rmd_b64encode_data, readBin(inFile, 'raw', n = fileSize)),
         sep = '')
}

#' @importFrom utils URLdecode
.b64EncodeImages <- function(html) {
  if (length(html) == 0) return(html)
  reg <- "<\\s*[Ii][Mm][Gg]\\s+[Ss][Rr][Cc]\\s*=\\s*[\"']([^\"']+)[\"']"
  m <- gregexpr(reg, html, perl = TRUE)
  if (m[[1]][1] != -1) {
    .b64EncodeImgSrc <- function(imgSrc) {
      src <- sub(reg, '\\1', imgSrc)
      # already base64 encoded?
      if (grepl('^data:.+;base64,.+', src)) return(imgSrc)
      inFile <- URLdecode(src)
      if (length(inFile) && file.exists(inFile))
        imgSrc <- sub(src, .b64EncodeFile(inFile), imgSrc, fixed = TRUE)

      imgSrc
    }
    regmatches(html, m) <- list(unlist(lapply(regmatches(html, m)[[1]], .b64EncodeImgSrc)))
  }

  html
}


.mathJax <- local({
  js <- NULL

  function(embed=FALSE, force=FALSE) {
    if (!embed)
      return(paste(readLines(system.file(
        'resources', 'mathjax.html', package = 'markdown'
      )), collapse = '\n'))

    url <- 'https://mathjax.rstudio.com/latest/MathJax.js?config=TeX-MML-AM_CHTML'

    # Insert or link to MathJax script?
    html <- c('<!-- MathJax scripts -->', if (embed) {
      # Already in cache?
      if (force || is.null(js)) {
        js <<- readLines(url, warn=FALSE)
      }
      c('<script>', js)
    } else {
      sprintf('<script src="%s" async>', url)
    }, '</script>')

    paste(html, collapse="\n")
  }
})

.requiresMathJax <- function(html) {
  regs <- c('\\\\\\(([\\s\\S]+?)\\\\\\)', '\\\\\\[([\\s\\S]+?)\\\\\\]')
  for (i in regs) if (any(grepl(i, html, perl = TRUE))) return(TRUE)
  FALSE
}

.requiresHighlighting <- function(html) any(grepl('<pre><code class="r"', html))


#' Render Markdown to HTML
#'
#' \code{markdownToHTML} transforms the \emph{Markdown} text provided by the
#' user in either the \code{file} or \code{text} variable. The HTML
#' transformation is either written to the \code{output} file or returned to the
#' user as a \code{character} vector.
#'
#' Three notable HTML options have been added to support collaborative
#' reproducible research. They are as follows:
#'
#' \itemize{
#'
#' \item Latex math expressions enclosed by one of the block level syntaxes,
#' $latex ... $ , $$ ... $$, or \[ ... \], or one of the inline syntaxes, $...$,
#' or \( ... \), will be rendered in real-time by the MathJax Javascript
#' library.
#'
#' \item \emph{R} code blocks enclosed between \verb{```r} and \verb{```} will
#' automatically be syntax highlighted.
#'
#' \item Any local images linked using the <img> tag will be base64 encoded and
#' included in the output HTML.
#'
#' }
#'
#' See the DETAILS section below and \code{\link{markdownHTMLOptions}} for more
#' information.
#'
#' There are two basic modes to \code{markdownToHTML} determined by the value of
#' the \code{fragment.only} argument:
#'
#' When \code{FALSE}, \code{markdownToHTML} creates well-formed stand-alone HTML
#' pages complete with HTML header, title, and body tags. The default template
#' used for this mode may be found here:
#'
#' \code{system.file('resources', 'markdown.html', package = 'markdown')}
#'
#' Also, \code{markdownToHTML} will automatically determine whether or not
#' mathjax and R code highlighting are needed and will include the appropriate
#' Javascript libraries in the output. Thus, there's no need to explicitly set
#' the \code{'mathjax'} or \code{'highlight_code'} options (see
#' \code{\link{markdownHTMLOptions}} for more details).
#'
#' When \code{fragment.only} is TRUE, nothing extra is added.
#'
#' @param file a character string giving the pathname of the file to read from.
#'   If it is omitted from the argument list, then it is presumed that the
#'   \code{text} argument will be used instead.
#' @param output a character string giving the pathname of the file to write to.
#'   If it is omitted (\code{NULL}), then it is presumed that the user expects
#'   the results returned as a \code{character} vector.
#' @param text a character vector containing the \emph{Markdown} text to
#'   transform (each element of this vector is treated as a line in a file).
#' @param options options that are passed to the renderer.  see
#'   \code{\link{markdownHTMLOptions}}.
#' @param extensions options that are passed to the \emph{Markdown} engine. See
#'   \code{\link{markdownExtensions}}.
#' @param title The HTML title.
#' @param stylesheet either valid CSS or a file containing CSS. will be included
#'   in the output.
#' @param header either valid HTML or a file containing HTML will be included in
#'   the header of the output.
#' @param template an HTML file used as template.
#' @param fragment.only Whether or not to produce an HTML fragment without the
#'   HTML header and body tags, CSS, and Javascript components.
#' @param encoding ignored (always assumes the file is encoded in UTF-8).
#' @return Invisible \code{NULL} when output is to a file, and a character
#'   vector otherwise.
#' @seealso \code{\link{markdownExtensions}}, \code{\link{markdownHTMLOptions}},
#'   \code{\link{renderMarkdown}}.
#' @export
#' @examples
#' (markdownToHTML(text = "Hello World!", fragment.only = TRUE))
#' (markdownToHTML(file = NULL, text = "_text_ will override _file_",
#'   fragment.only = TRUE))
#' # write HTML to an output file
#' markdownToHTML(text = "_Hello_, **World**!", output = tempfile())
markdownToHTML <- function(
  file, output = NULL, text = NULL, options = getOption('markdown.HTML.options'),
  extensions = getOption('markdown.extensions'),
  title = '',
  stylesheet = getOption('markdown.HTML.stylesheet'),
  header = getOption('markdown.HTML.header'),
  template = getOption('markdown.HTML.template'),
  fragment.only = FALSE,
  encoding = 'UTF-8'
) {
  options <- normalizeOptions(options)
  if (fragment.only) options[['fragment_only']] <- TRUE

  ret <- renderMarkdown(
    file, output = NULL, text, renderer = 'HTML',
    renderer.options = options, extensions = extensions
  )

  if (isTRUE(options[['base64_images']])) {
    filedir <- if (!missing(file) && is.character(file) && file.exists(file)) {
      dirname(file)
    } else '.'
    ret <- local({
      oldwd <- setwd(filedir)
      on.exit(setwd(oldwd))
      .b64EncodeImages(ret)
    })
  }

  if (!isTRUE(options[['fragment_only']])) {
    if (is.null(template))
      template <- system.file('resources', 'markdown.html', package = 'markdown')
    html <- paste(readLines(template), collapse = '\n')
    html <- sub('#!html_output#', if (length(ret)) ret else '', html, fixed = TRUE)

    if (is.character(stylesheet)) {
      html <- sub('#!markdown_css#', option2char(stylesheet), html, fixed = TRUE)
    } else {
      warning('stylesheet must either be valid CSS or a file containing CSS!')
    }

    html <- sub('#!header#', option2char(header), html, fixed = TRUE)

    if (!is.character(title) || title == '') {
      # Guess title
      m <- regexpr('<[Hh][1-6].*?>(.*)</[Hh][1-6].*?>', html, perl = TRUE)
      if (m > -1) {
        title <- regmatches(html, m)
        title <- sub('<[Hh][1-6].*?>', '', title)
        title <- sub('</[Hh][1-6].*?>', '', title)
      } else {
        title <- ''
      }
    }

    # Need to scrub title more, e.g. strip html, etc.
    html <- sub('#!title#', title, html, fixed = TRUE)

    mathjax <- if (isTRUE(options[['mathjax']]) && .requiresMathJax(html)) {
      .mathJax(embed = isTRUE(options[['mathjax_embed']]))
    } else ''
    html <- sub('#!mathjax#', mathjax, html, fixed = TRUE)

    highlight <- if (isTRUE(options[['highlight_code']]) && .requiresHighlighting(html)) {
      xfun::file_string(system.file('resources', 'r_highlight.html', package = 'markdown'))
    } else ''
    html <- sub('#!r_highlight#', highlight, html, fixed = TRUE)

    ret <- html
  }

  ret <- if (is.character(output)) xfun::write_utf8(ret, output) else enc2utf8(ret)

  invisible(ret)
}

# from an option to an appropriate character string of CSS/header/...
option2char <- function(x) {
  if (!is.character(x)) return('')
  paste(if (length(x) == 1 && file.exists(x)) readLines(x) else x, collapse = '\n')
}

#' Convert some ASCII strings to HTML entities
#'
#' Transform ASCII strings \verb{(c)} (copyright), \verb{(r)} (registered
#' trademark), \verb{(tm)} (trademark), and fractions \verb{n/m} into
#' \emph{smart} typographic HTML entities.
#' @param file Path to an input file. If not provided, it is presumed that the
#'   \code{text} argument will be used instead.
#' @param output Output file path. If not character, the results will be
#'   returned as a character vector.
#' @param text A character vector containing the \emph{Markdown} text to
#'   transform.
#' @return Invisible \code{NULL} when output is to a file, and a character
#'   vector otherwise.
#' @seealso \code{\link{markdownExtensions}}, \code{\link{markdownHTMLOptions}},
#'   \code{\link{markdownToHTML}}.
#' @export
#' @examples
#' cat(smartypants(text = "1/2 (c)\n"))
smartypants <- function(file, output = NULL, text = xfun::read_utf8(file)) {
  text <- xfun::split_lines(text)
  i <- xfun::prose_index(text)
  x <- text[i]
  r <- '(?<!`)\\((c|r|tm)\\)|(\\d+/\\d+)(?!`)'
  m <- gregexpr(r, x, perl = TRUE)
  regmatches(x, m) <- lapply(regmatches(x, m), function(z) {
    y <- pants[z]
    i <- is.na(y)
    y[i] <- z[i]
    y
  })
  text[i] <- x
  if (is.character(output)) xfun::write_utf8(text, output) else text
}

# Represent some fractions with HTML entities
fracs <- local({
  k <- c(2, 3, 5)
  z <- list()
  for (i in 2:10) {
    for (j in 1:i) {
      if (j > 1 && (i %% j == 0 || any(i == c(7, 9, 10)))) next
      if (any((i %% k == 0) & (j %% k == 0))) next
      x <- paste0(j, '/', i)
      y <- if (j > 1 || i < 9) sprintf('&frac%d%d;', j, i) else {
        if (i == 9) '&#8529;' else if (i == 10) '&#8530;'
      }
      z[[x]] <- y
    }
  }
  z
})

pants <- c(unlist(fracs), c('(c)' = '&copy;', '(r)' = '&reg;', '(tm)' = '&trade;'))

# Markdown extensions.
#
# To turn on all extensions:
#
# options(markdown.extensions = markdownExtensions())
#
# To turn off all extensions:
#
# options(markdown.extensions = c())
#


#' Markdown extensions
#'
#' \code{markdownExtensions} returns a character vector listing all the
#' extensions that are available in the \pkg{markdown} package.
#'
#' They are all ON by default.
#'
#' The \pkg{Sundown} library (upon which \pkg{markdown} is built) has optional
#' support for several extensions described below. To turn these on globally in
#' the \pkg{markdown} package, simply place some or all of them in a character
#' vector and assign to the global option \code{markdown.extensions} like so:
#'
#' \code{options(markdown.extensions = markdownExtensions())}
#'
#' To override the global option, pass the \code{extensions} as an argument to
#' one of the render functions, e.g.:
#'
#' \code{markdownToHTML(..., extensions = c('no_intra_emphasis'))}
#'
#' Description of all extensions:
#'
#' \describe{
#'
#' \item{\code{'no_intra_emphasis'}}{ skip markdown embedded in words.  }
#'
#' \item{\code{'tables'}}{ create HTML tables (see Examples). }
#'
#' \item{\code{'fenced_code'}}{ treat text as verbatim when surrounded with
#' begin and ending lines with three ~ or \emph{`} characters.  }
#'
#' \item{\code{'autolink'}}{ create HTML links from urls and email addresses. }
#'
#' \item{\code{'strikethrough'}}{ create strikethroughs by surrounding text with
#' ~~.  }
#'
#' \item{\code{'lax_spacing'}}{ allow HTML tags inside paragraphs without being
#' surrounded by newlines.  }
#'
#' \item{\code{'space_headers'}}{ add a space between header hashes and the
#' header itself.  }
#'
#' \item{\code{'superscript'}}{ translate ^ and subsequent text into HTML
#' superscript. }
#'
#' \item{\code{'latex_math'}}{ transforms all math equations into syntactically
#' correct MathJax equations.  }
#'
#' }
#'
#' See the EXAMPLES section to see the output of each extension turned on or
#' off.
#' @return A \code{character} vector listing all available extensions.
#' @seealso \link{markdownHTMLOptions}
#' @export markdownExtensions
#' @examples
#' # List all available extensions:
#' markdownExtensions()
#'
#' # To turn on all Markdown extensions globally:
#' options(markdown.extensions = markdownExtensions())
#'
#' # To turn off all Markdown extensions globally:
#' options(markdown.extensions = NULL)
#'
#' @example inst/examples/markdownExtensions.R
markdownExtensions <- function()
  c('no_intra_emphasis', 'tables', 'fenced_code', 'autolink', 'strikethrough',
    'lax_spacing', 'space_headers', 'superscript', 'latex_math')

# HTML renderer options.
#
# To turn on all options:
#
# options(markdown.HTML.options = markdownHTMLOptions())
#
# To turn on default options:
#
# options(markdown.HTML.options = markdownHTMLOptions(defaults = TRUE))
#
# To turn off all options:
#
# options(markdown.HTML.options = c())
#


#' Markdown HTML rendering options
#'
#' \code{markdownHTMLOptions} returns a character vector listing all the options
#' that are available for the HTML renderer in the \pkg{markdown} package. As a
#' convenience, the package default options were chosen to render well-formed
#' stand-alone HTML pages when using \code{\link{markdownToHTML}()}. The default
#' options are \code{'smartypants'}, \code{'base64_images'},
#' \code{'mathjax'}, and \code{'highlight_code'}.
#'
#' The HTML renderer provides several options described below. To turn these on
#' globally in the \pkg{markdown} package, simply place some or all of them in a
#' character vector and assign to the global option \code{markdown.HTML.options}.
#'
#' Description of all options:
#'
#' \describe{
#'
#' \item{\code{'toc'}}{ assigns an HTML id to each header of the form 'toc_%d'
#' where '%d' is replaced with the position of the header within the document
#' (starting at 0), and creates the table of contents.}
#'
#' \item{\code{'hardbreaks'}}{ adds an HTML br tag for every newline (excluding
#' trailing) found within a paragraph.}
#'
#' \item{\code{'smartypants'}}{ translates plain ASCII punctuation characters
#' into \emph{smart} typographic punctuation HTML entities. }
#'
#' \item{\code{'fragment_only'}}{ eliminates the inclusion of any HTML header or
#' body tags, CSS, or Javascript components. }
#'
#' \item{\code{'base64_images'}}{ Any local images linked with the
#' \code{'<img>'} tag to the output HTML will automatically be converted to
#' base64 and included along with output. }
#'
#' \item{\code{'mathjax'}}{ includes appropriate Javascript libraries to render
#' math markup.}
#'
#' \item{\code{'highlight_code'}}{ includes appropriate Javascript libraries to
#' highlight code chunks.}
#'
#' }
#'
#' See the EXAMPLES section to see the output of each option turned on or off.
#' @param defaults If \code{TRUE}, then only the default options are returned.
#'   Otherwise all options are returned.
#' @return A \code{character} vector listing either all available options or
#'   just the default options.
#' @seealso \link{markdownToHTML}
#' @export
#' @examples
#' # List all available extensions:
#' markdownHTMLOptions()
#'
#' # To turn on all HTML options globally:
#' options(markdown.HTML.options = markdownHTMLOptions())
#'
#' # To turn off all HTML options globally:
#' options(markdown.HTML.options = NULL)
#'
#' # To turn on one option globally:
#' options(markdown.HTML.options = 'smartypants')
#'
#' # To turn on package default HTML options globally:
#' options(markdown.HTML.options = markdownHTMLOptions(defaults = TRUE))
#'
#' @example inst/examples/HTMLOptions.R
markdownHTMLOptions <- function(defaults = FALSE) {
  sort(c(
    c('smart', 'smartypants', 'base64_images', 'mathjax', 'highlight_code'),
    if (!defaults) c('toc', 'fragment_only', 'hardbreaks')
  ))
}

#' @import stats
normalizeOptions <- function(x) {
  if (is.character(x)) x <- as.list(setNames(rep(TRUE, length(x)), x))
  n = names(x)
  n[n == 'hard_wrap'] = 'hardbreaks'
  names(x) = n
  as.list(x)
}

.onLoad <- function(libname, pkgname) {

  if (is.null(getOption('markdown.extensions')))
    options(markdown.extensions = markdownExtensions())

  if (is.null(getOption('markdown.HTML.options')))
    options(markdown.HTML.options = markdownHTMLOptions(defaults = TRUE))

  if (is.null(getOption('markdown.HTML.stylesheet'))) {
    sheet <- system.file('resources', 'markdown.css', package = 'markdown')
    options(markdown.HTML.stylesheet = sheet)
  }
}

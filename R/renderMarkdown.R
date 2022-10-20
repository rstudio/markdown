# Copyright (C) 2009-2022 by RStudio, PBC
#
# This program is licensed to you under the terms of version 2 of the
# GNU General Public License. This program is distributed WITHOUT ANY
# EXPRESS OR IMPLIED WARRANTY, INCLUDING THOSE OF NON-INFRINGEMENT,
# MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. Please refer to the
# GPL (https://www.gnu.org/licenses/gpl-2.0.txt) for more details.


#' Render Markdown to an output format
#'
#' Render Markdown to an output format via the \pkg{commonmark} package.
#' @param file Path to an input file. If not provided, it is presumed that the
#'   \code{text} argument will be used instead.
#' @param output Output file path. If not character, the results will be
#'   returned as a character vector.
#' @param text A character vector of the Markdown text. By default, it is read
#'   from \code{file}.
#' @param renderer An output format supported by \pkg{commonmark}, e.g.,
#'   \code{'html'}, \code{'man'}, and \code{'text'}, etc. See the
#'   \code{\link[commonmark:commonmark]{markdown_*}} functions in
#'   \pkg{commonmark}.
#' @param options A list of options to be passed to the renderer. See
#'   \code{\link{markdownOptions}()} for all possible options.
#' @param extensions A character vector of Markdown extensions See
#'   \code{\link{markdownExtensions}()}.
#' @return Invisible \code{NULL} when output is to a file, otherwise a character
#'   vector.
#' @seealso The spec of GitHub Flavored Markdown:
#'   \url{https://github.github.com/gfm/}
#' @import utils
#' @export
#' @examples
#' renderMarkdown(text = "Hello World!")
#' # a few corner cases
#' renderMarkdown(text = character(0))
#' renderMarkdown(text = '')
renderMarkdown = function(
  file, output = NULL, text = NULL, renderer = c('html', 'latex'),
  options = NULL, extensions = markdownExtensions()
) {
  if (is.null(text)) text = xfun::read_utf8(file)

  renderer = renderer[1]

  render = tryCatch(
    getFromNamespace(paste0('markdown_', tolower(renderer)), 'commonmark'),
    error = function(e) {
      stop("Renderer '", renderer, "' is not available in commonmark.")
    }
  )

  if (is.null(options))
    options = get_option(sprintf('markdown.%s.options', renderer))
  options = normalizeOptions(options)
  options$extensions = intersect(extensions, commonmark::list_extensions())

  ret = do.call(render, c(
    list(text = text),
    options[intersect(names(formals(render)), names(options))]
  ))
  if (is.character(output)) xfun::write_utf8(ret, output) else ret
}

# Get an option using a case-insensitive name
get_option = function(name) {
  x = options()
  i = match(tolower(name), tolower(names(x)))
  x[[i]]
}

#' @importFrom utils URLdecode
.b64EncodeImages = function(html) {
  if (length(html) == 0) return(html)
  reg = '<img\\s+src\\s*=\\s*"([^"]+)"'
  m = gregexpr(reg, html, ignore.case = TRUE)
  regmatches(html, m) = lapply(regmatches(html, m), function(x) {
    src = sub(reg, '\\1', x)
    # skip images already base64 encoded
    for (i in grep('^data:.+;base64,.+', src, invert = TRUE)) {
      if (file.exists(f <- URLdecode(src[i]))) x[i] = sub(
        src[i], xfun::base64_uri(f, mime::guess_type(f)), x[i], fixed = TRUE
      )
    }
    x
  })
  html
}


.mathJax = local({
  js = NULL

  function(embed=FALSE, force=FALSE) {
    if (!embed)
      return(xfun::file_string(pkg_file('resources', 'mathjax.html')))

    url = 'https://mathjax.rstudio.com/latest/MathJax.js?config=TeX-MML-AM_CHTML'

    # Insert or link to MathJax script?
    html = c('<!-- MathJax scripts -->', if (embed) {
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

.requiresMathJax = function(html) {
  regs = c('\\\\\\(([\\s\\S]+?)\\\\\\)', '\\\\\\[([\\s\\S]+?)\\\\\\]')
  for (i in regs) if (any(grepl(i, html, perl = TRUE))) return(TRUE)
  FALSE
}

.requiresHighlighting = function(html) any(grepl('<pre><code class="r"', html))


#' Render Markdown to HTML
#'
#' \code{markdownToHTML} transforms the Markdown text provided by the
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
#' @inheritParams renderMarkdown
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
markdownToHTML = function(
  file, output = NULL, text = NULL, options = getOption('markdown.HTML.options'),
  extensions = getOption('markdown.extensions'),
  title = '',
  stylesheet = getOption('markdown.HTML.stylesheet'),
  header = getOption('markdown.HTML.header'),
  template = getOption('markdown.HTML.template'),
  fragment.only = FALSE,
  encoding = 'UTF-8'
) {
  options = normalizeOptions(options)
  if (fragment.only) options[['fragment_only']] = TRUE

  ret = renderMarkdown(file, NULL, text, options, extensions, renderer = 'html')

  if (isTRUE(options[['base64_images']])) {
    filedir = if (!missing(file) && is.character(file) && file.exists(file)) {
      dirname(file)
    } else '.'
    ret = xfun::in_dir(filedir, .b64EncodeImages(ret))
  }

  if (!isTRUE(options[['fragment_only']])) {
    if (is.null(template))
      template = pkg_file('resources', 'markdown.html')
    html = xfun::file_string(template)
    html = sub('#!html_output#', if (length(ret)) ret else '', html, fixed = TRUE)

    if (is.character(stylesheet)) {
      html = sub('#!markdown_css#', option2char(stylesheet), html, fixed = TRUE)
    } else {
      warning('stylesheet must either be valid CSS or a file containing CSS!')
    }

    html = sub('#!header#', option2char(header), html, fixed = TRUE)

    if (!is.character(title) || title == '') {
      # Guess title
      m = regexpr('<[Hh][1-6].*?>(.*)</[Hh][1-6].*?>', html, perl = TRUE)
      if (m > -1) {
        title = regmatches(html, m)
        title = sub('<[Hh][1-6].*?>', '', title)
        title = sub('</[Hh][1-6].*?>', '', title)
      } else {
        title = ''
      }
    }

    # Need to scrub title more, e.g. strip html, etc.
    html = sub('#!title#', title, html, fixed = TRUE)

    mathjax = if (isTRUE(options[['mathjax']]) && .requiresMathJax(html)) {
      .mathJax(embed = isTRUE(options[['mathjax_embed']]))
    } else ''
    html = sub('#!mathjax#', mathjax, html, fixed = TRUE)

    highlight = if (isTRUE(options[['highlight_code']]) && .requiresHighlighting(html)) {
      xfun::file_string(pkg_file('resources', 'r_highlight.html'))
    } else ''
    html = sub('#!r_highlight#', highlight, html, fixed = TRUE)

    ret = html
  }

  ret = if (is.character(output)) xfun::write_utf8(ret, output) else enc2utf8(ret)

  invisible(ret)
}

# from an option to an appropriate character string of CSS/header/...
option2char = function(x) {
  if (!is.character(x)) return('')
  paste(if (length(x) == 1 && file.exists(x)) readLines(x) else x, collapse = '\n')
}

#' Convert some ASCII strings to HTML entities
#'
#' Transform ASCII strings \verb{(c)} (copyright), \verb{(r)} (registered
#' trademark), \verb{(tm)} (trademark), and fractions \verb{n/m} into
#' \emph{smart} typographic HTML entities.
#' @inheritParams renderMarkdown
#' @return Invisible \code{NULL} when output is to a file, and a character
#'   vector otherwise.
#' @seealso \code{\link{markdownExtensions}}, \code{\link{markdownHTMLOptions}},
#'   \code{\link{markdownToHTML}}.
#' @export
#' @examples
#' cat(smartypants(text = "1/2 (c)\n"))
smartypants = function(file, output = NULL, text = xfun::read_utf8(file)) {
  text = xfun::split_lines(text)
  i = xfun::prose_index(text)
  x = text[i]
  r = '(?<!`)\\((c|r|tm)\\)|(\\d+/\\d+)(?!`)'
  m = gregexpr(r, x, perl = TRUE)
  regmatches(x, m) = lapply(regmatches(x, m), function(z) {
    y = pants[z]
    i = is.na(y)
    y[i] = z[i]
    y
  })
  text[i] = x
  if (is.character(output)) xfun::write_utf8(text, output) else text
}

# Represent some fractions with HTML entities
fracs = local({
  k = c(2, 3, 5)
  z = list()
  for (i in 2:10) {
    for (j in 1:i) {
      if (j > 1 && (i %% j == 0 || any(i == c(7, 9, 10)))) next
      if (any((i %% k == 0) & (j %% k == 0))) next
      x = paste0(j, '/', i)
      y = if (j > 1 || i < 9) sprintf('&frac%d%d;', j, i) else {
        if (i == 9) '&#8529;' else if (i == 10) '&#8530;'
      }
      z[[x]] = y
    }
  }
  z
})

pants = c(unlist(fracs), c('(c)' = '&copy;', '(r)' = '&reg;', '(tm)' = '&trade;'))

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
#' List all available Markdown extensions.
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
#' \item{\code{'tables'}}{ create HTML tables (see Examples). }
#'
#' \item{\code{'autolink'}}{ create HTML links from urls and email addresses. }
#'
#' \item{\code{'strikethrough'}}{ create strikethroughs by surrounding text with
#' ~~.  }
#'
#' \item{\code{'superscript'}}{ translate ^ and subsequent text into HTML
#' superscript. }
#'
#' \item{\code{'latex_math'}}{ transforms all math equations into syntactically
#' correct MathJax equations.  }
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
markdownExtensions = function(default = TRUE) {
  setdiff(
    c(commonmark::list_extensions(), 'superscript', 'subscript', 'latex_math'),
    if (default) 'tagfiler'
  )
}

#' Markdown rendering options
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
#' @param default If \code{TRUE}, only the default options are returned.
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
#' options(markdown.HTML.options = markdownHTMLOptions(default = TRUE))
#'
#' @example inst/examples/render-options.R
markdownOptions = function(default = FALSE) {
  sort(c(
    'smart', 'smartypants', 'base64_images', 'mathjax', 'highlight_code',
    if (!default) c('toc', 'fragment_only', 'hardbreaks')
  ))
}

#' @rdname markdownOptions
#' @export
markdownHTMLOptions = markdownOptions

#' @import stats
normalizeOptions = function(x) {
  if (is.character(x)) x = as.list(setNames(rep(TRUE, length(x)), x))
  n = names(x)
  n[n == 'hard_wrap'] = 'hardbreaks'
  names(x) = n
  as.list(x)
}

pkg_file = function(...) system.file(..., package = 'markdown', mustWork = TRUE)

.onLoad = function(libname, pkgname) {
  if (is.null(getOption('markdown.HTML.options')))
    options(markdown.HTML.options = markdownOptions(default = TRUE))

  if (is.null(getOption('markdown.HTML.stylesheet'))) {
    sheet = pkg_file('resources', 'markdown.css')
    options(markdown.HTML.stylesheet = sheet)
  }
}

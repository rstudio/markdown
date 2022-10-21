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
#' @return Invisible \code{NULL} when output is to a file, otherwise a character
#'   vector.
#' @seealso The spec of GitHub Flavored Markdown:
#'   \url{https://github.github.com/gfm/}
#' @import utils
#' @export
#' @examples
#' renderMarkdown(text = "Hello _World_!")
#' # a few corner cases
#' renderMarkdown(text = character(0))
#' renderMarkdown(text = '')
renderMarkdown = function(
  file, output = NULL, text = NULL, renderer = c('html', 'latex'), options = NULL
) {
  if (is.null(text)) text = xfun::read_utf8(file)

  renderer = renderer[1]

  render = tryCatch(
    getFromNamespace(paste0('markdown_', tolower(renderer)), 'commonmark'),
    error = function(e) {
      stop("Renderer '", renderer, "' is not available in commonmark.")
    }
  )

  options = normalizeOptions(options, renderer)
  options$extensions = intersect(
    names(Filter(isTRUE, options)), commonmark::list_extensions()
  )

  ret = do.call(render, c(
    list(text = text),
    options[intersect(names(formals(render)), names(options))]
  ))
  if (is.character(output)) xfun::write_utf8(ret, output) else ret
}

# Get an option using a case-insensitive name
get_option = function(name, default = NULL) {
  x = options()
  i = match(tolower(name), tolower(names(x)))
  if (is.na(i)) default else x[[i]]
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
#' Render Markdown to HTML with optional customization (e.g., custom stylesheets
#' or template).
#' @inheritParams renderMarkdown
#' @param title The HTML title.
#' @param css Either a vector of CSS code or a file containing CSS to be
#'   included in the output. The default value is
#'   \code{getOption('markdown.html.css', markdown:::pkg_file('resources',
#'   'markdown.css'))}, i.e., it can be set via the global option
#'   \code{markdown.html.css}.
#' @param header Either a vector of HTML code or a file containing HTML to be
#'   included in the header of the output. The default value is
#'   \code{getOption('markdown.html.header')}.
#' @param template An HTML template file. The default value is
#'   \code{getOption('markdown.html.template', markdown:::pkg_file('resources',
#'   'markdown.html'))}.
#' @param encoding Ignored (always assumes the file is encoded in UTF-8).
#' @param ... Unused but for backward-compatibility with previous versions of
#'   \pkg{markdown}.
#' @return Invisible \code{NULL} when output is to a file, and a character
#'   vector otherwise.
#' @seealso \code{\link{renderMarkdown}()}
#' @export
#' @examples
#' (markdownToHTML(text = 'Hello _World_!', options = '+fragment_only'))
#' # write HTML to an output file
#' markdownToHTML(text = '_Hello_, **World**!', output = tempfile())
markdownToHTML = function(
  file, output = NULL, text = NULL, options = NULL, title = '', css = NULL,
  header = NULL, template = NULL, encoding = 'UTF-8', ...
) {

  options = normalizeOptions(options, 'html')
  ret = renderMarkdown(file, NULL, text, 'html', options)
  extra = list(...)
  if (is.null(css)) {
    # TODO: deprecate the 'stylesheet' argument in future
    css = extra[['stylesheet']]
    if (is.character(css)) warning(
      "The 'stylesheet' argument has been renamed to 'css' in markdown::markdownToHTML()"
    )
  }
  if (is.logical(extra[['fragment.only']])) {
    options[['fragment_only']] = extra[['fragment.only']]
    warning(
      "The 'fragment.only' argument has been deprecated. ",
      "Please use the argument `options = 'fragment_only'` instead."
    )
  }

  if (isTRUE(options[['base64_images']])) {
    filedir = if (!missing(file) && is.character(file) && file.exists(file)) {
      dirname(file)
    } else '.'
    ret = xfun::in_dir(filedir, .b64EncodeImages(ret))
  }

  if (!isTRUE(options[['fragment_only']])) {
    if (is.null(template)) template = get_option(
      'markdown.html.template', pkg_file('resources', 'markdown.html')
    )
    html = xfun::file_string(template)
    html = sub('#!html_output#', if (length(ret)) ret else '', html, fixed = TRUE)

    if (is.null(css))
      css = get_option('markdown.html.css', pkg_file('resources', 'markdown.css'))
    if (is.character(css)) {
      html = sub('#!markdown_css#', option2char(css), html, fixed = TRUE)
    } else {
      warning("The 'css' argument must be character!")
    }

    header = get_option('markdown.html.header', header)
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


#' Markdown rendering options
#'
#' A list of all available options to control Markdown rendering. Options that
#' are enabled by default are marked by a \code{+} prefix, and those disabled by
#' default are marked by \code{-}.
#'
#' Description of all options:
#'
#' \describe{
#'
#' \item{\code{base64_images}}{Embed local images in the HTML output with base64
#' encoding.}
#'
#' \item{\code{fragment_only}}{Generate a full (HTML/LaTeX) document or only a
#' fragment of the body.}
#'
#' \item{\code{highlight_code}}{Includes JavaScript libraries to syntax
#' highlight code blocks.}
#'
#' \item{\code{latex_math}}{Identify LaTeX math expressions in pairs of single
#' or double dollar signs, and transform them so that they could be correctly
#' rendered by MathJax (HTML output) or LaTeX.}
#'
#' \item{\code{mathjax}}{Include MathJax library in HTML output.}
#'
#' \item{\code{smartypants}}{Translate certain ASCII strings into smart
#' typographic characters (see \code{\link{smartypants}()}.}
#'
#' \item{\code{superscript}}{Translate strings between two carets into
#' superscripts, e.g., \verb{text^foo^} to \verb{text<sup>foo</sup>}.}
#'
#' \item{\code{subscript}}{Translate strings between two tildes into subscripts,
#' e.g., \verb{text~foo~} to \verb{text<sub>foo</sub>}.}
#'
#' \item{\code{toc}}{Generate a table of contents from section headers.}
#'
#' }
#'
#' Options not described above can be found on the help pages of
#' \pkg{commonmark}, e.g., the \code{hardbreaks} option is for the
#' \code{hardbreaks} argument of
#' \code{\link[commonmark:commonmark]{markdown_*}()} functions, and the
#' \code{table} option is for the \code{table} extension in \pkg{commonmark}'s
#' extensions (\code{commonmark::\link[commonmark]{list_extensions}()}).
#'
#' See the Examples section of \code{\link{renderMarkdown}()} to see the output
#' of each option turned on or off.
#' @return A character vector of all available options.
#' @export
#' @examples
#' # List all available options
#' markdown::markdownOptions()
#'
#' # Turn on/off some options globally for HTML output
#' options(markdown.html.options = '+fragment_only+toc-smartypants')
#'
#' @example inst/examples/render-options.R
markdownOptions = function() {
  # options enabled by default
  x1 = c(
    'smart', 'smartypants', 'base64_images', 'mathjax', 'highlight_code',
    'superscript', 'subscript', 'latex_math',
    setdiff(commonmark::list_extensions(), 'tagfilter')
  )
  # options disabled by default
  x2 = c('toc', 'fragment_only', 'hardbreaks', 'tagfilter')
  sort(c(paste0('+', x1), paste0('-', x2)))
}

normalizeOptions = function(x, format = 'html') {
  g = get_option(sprintf('markdown.%s.options', format))
  x = option2list(x)
  n = names(x)
  n[n == 'hard_wrap'] = 'hardbreaks'
  n[n == 'tables'] = 'table'
  names(x) = n
  # default options
  d = option2list(markdownOptions())
  g = option2list(g)
  d[names(g)] = g  # merge global options() into default options
  d[n] = x  # then merge user-provided options
  d
}

#' @import stats
namedBool = function(x, val = TRUE) as.list(setNames(rep(val, length(x)), x))

# turn '+a-b c' to list(a = TRUE, b = FALSE, c = TRUE)
option2list = function(x) {
  if (!is.character(x)) return(as.list(x))
  x = unlist(strsplit(x, '\\b(?=[+-])', perl = TRUE))
  x = unlist(strsplit(x, '\\s+'))
  x = setdiff(x, '')
  i = grepl('^-', x)
  c(namedBool(sub('^[-]', '', x[i]), FALSE), namedBool(sub('^[+]', '', x[!i])))
}

pkg_file = function(...) system.file(..., package = 'markdown', mustWork = TRUE)

#' Render Markdown to an output format
#'
#' Render Markdown to an output format via the \pkg{commonmark} package. The
#' function \code{mark_html()} is a shorthand of \code{mark(format = 'html')},
#' and \code{mark_latex()} is a shorthand of \code{mark(format = 'latex')}.
#'
#' Supported variables in metadata for both HTML and HTML templates (the string
#' \code{FORMAT} below is the output format name, i.e., \code{html} or
#' \code{latex}):
#'
#' \describe{
#'
#' \item{\code{header-includes}, \code{include-before},
#' \code{include-after}}{Either a vector of code (HTML/LaTeX) or a code file to
#' be included in the header, before the body, or after the body of the output.
#' For \code{header-include}, the default value is taken from
#' \code{getOption('markdown.FORMAT.header')} if not provided in \code{meta}.}
#'
#' \item{\code{title}}{The document title.}
#'
#' }
#'
#' Variables for the HTML template:
#'
#' \describe{
#'
#' \item{\code{css}}{Either a vector of CSS code or a file containing CSS to be
#' included in the output. The default value is
#' \code{getOption('markdown.html.css', markdown:::pkg_file('resources',
#' 'markdown.css'))}, i.e., it can be set via the global option
#' \code{markdown.html.css}.}
#'
#' \item{\code{highlight}}{JavaScript code for syntax-highlighting code blocks.
#' By default, the highlight.js library is used.}
#'
#' \item{\code{math}}{JavaScript code for rendering LaTeX math. By default,
#' MathJax is used.}
#'
#' }
#'
#' Variables for the LaTeX template:
#'
#' \describe{
#'
#' \item{\code{classoption}}{A string containing options for the document
#' class.}
#'
#' \item{\code{documentclass}}{The document class (by default,
#' \code{'article'}).}
#'
#' }
#'
#' Note that you can use either underscores or hyphens in the variable names.
#' Underscores will be normalized to hyphens internally, e.g.,
#' \code{header_includes} will be converted to \code{header-includes}. This
#' means if you use a custom template, you must use hyphens instead of
#' underscores as separators in variable names in the template.
#' @param file Path to an input file. If not provided, it is presumed that the
#'   \code{text} argument will be used instead. This argument can also take a
#'   character vector of Markdown text directly. To avoid ambiguity in the
#'   latter case, a single character string input will be treated as a file only
#'   when the file exists or it has a file extension. If a string happens to
#'   have a \dQuote{file extension} and should be treated as Markdown text
#'   instead, wrap it in \code{I()}.
#' @param output Output file path. If not character, the results will be
#'   returned as a character vector.
#' @param text A character vector of the Markdown text. By default, it is read
#'   from \code{file}.
#' @param format An output format supported by \pkg{commonmark}, e.g.,
#'   \code{'html'}, \code{'man'}, and \code{'text'}, etc. See the
#'   \code{\link[commonmark:commonmark]{markdown_*}} renderers in
#'   \pkg{commonmark}.
#' @param options Options to be passed to the renderer. See
#'   \code{\link{markdown_options}()} for all possible options. This argument
#'   can take either a character vector of the form \code{"+option1
#'   option2-option3"} (use \code{+} or a space to enable an option, and
#'   \code{-} to disable an option), or a list of the form \code{list(option1 =
#'   value1, option2 = value2, ...)}. A string \code{"+option1"} is equivalent
#'   to \code{list(option1 = TRUE)}, and \code{"-option2"} means
#'   \code{list(option2 = FALSE)}. Options that do not take logical values must
#'   be specified via a list, e.g., \code{list(width = 30)}.
#' @param template Path to a template file. The default value is
#'   \code{getOption('markdown.FORMAT.template',
#'   markdown:::pkg_file('resources', 'markdown.FORMAT'))} where \code{FORMAT}
#'   is the output format name (\code{html} or \code{latex}). It can also take a
#'   logical value: \code{TRUE} means to use the default template, and
#'   \code{FALSE} means to generate only a fragment without using any template.
#' @param meta A named list of metadata. Elements in the metadata will be used
#'   to fill out the template by their names and values, e.g., \code{list(title
#'   = ...)} will replace the \code{$title$} variable in the template. See the
#'   \sQuote{Details} section for supported variables.
#' @return Invisible \code{NULL} when output is to a file, otherwise a character
#'   vector of the rendered output.
#' @seealso The spec of GitHub Flavored Markdown:
#'   \url{https://github.github.com/gfm/}
#' @import utils
#' @export
#' @examples
#' library(markdown)
#' mark(c('Hello _World_!', '', 'Welcome to **markdown**.'))
#' # a few corner cases
#' mark(character(0))
#' mark('')
#' # if input looks like file but should be treated as text, use I()
#' mark(I('This is *not* a file.md'))
#' # that's equivalent to
#' mark(text = 'This is *not* a file.md')
mark = function(
  file = NULL, output = NULL, text = NULL, format = c('html', 'latex'),
  options = NULL, template = FALSE, meta = list()
) {
  if (is.null(text)) {
    if (!is.character(file)) stop("Either 'file' or 'text' must be provided.")
    text = if (is_file(file)) xfun::read_utf8(file) else file
  }
  text = xfun::split_lines(text)

  part = split_yaml(text); yaml = part$yaml; text = part$body
  format = format[1]
  # title/author/date can be provided as top-level YAML options
  meta = merge_list(
    yaml[intersect(names(yaml), c('title', 'author', 'date'))],
    format_meta(yaml, format),
    meta
  )

  render = tryCatch(
    getFromNamespace(paste0('markdown_', tolower(format)), 'commonmark'),
    error = function(e) {
      stop("Output format '", format, "' is not supported in commonmark.")
    }
  )

  options = normalize_options(options, format)
  options$extensions = intersect(
    names(Filter(isTRUE, options)), commonmark::list_extensions()
  )

  if (isTRUE(options[['smartypants']])) text = smartypants(text)

  # test if a feature needs to be enabled
  test_feature = function(name, pattern) {
    isTRUE(options[[name]]) && format %in% c('html', 'latex') &&
      length(grep(pattern, text, perl = TRUE))
  }

  # protect $ $ and $$ $$ math expressions for html/latex output
  if (has_math <- test_feature('latex_math', '[$]')) {
    id = id_string(text); maths = NULL
    text = protect_math(text, id)
    # temporarily replace math expressions with tokens and restore them later;
    # no need to do this for html output because we need special HTML characters
    # like &<> in math expressions to be converted to entities, but shouldn't
    # convert them for latex output
    if (format == 'latex') {
      text = paste(text, collapse = '\n')
      text = match_replace(text, sprintf('`%s.{3,}?%s`', id, id), function(x) {
        maths <<- c(maths, gsub(sprintf('`%s|%s`', id, id), '', x))
        # replace math with !id-n-id! where n is the index of the math
        sprintf('!%s-%d-%s!', id, length(maths) + seq_along(x), id)
      })
      text = xfun::split_lines(text)
    }
  }

  p = NULL  # indices of prose
  # superscript and subscript; for now, we allow only characters alnum|*|(|) for
  # script text but can consider changing this rule upon users' request
  r2 = '(?<=[[:graph:]])\\^([[:alnum:]*()]+?)\\^'
  if (has_sup <- test_feature('superscript', r2)) {
    id2 = id_string(text)
    p = xfun::prose_index(text)
    text[p] = match_replace(text[p], r2, perl = TRUE, function(x) {
      # place superscripts inside !id...id!
      x = gsub('^\\^|\\^$', id2, x)
      sprintf('!%s!', x)
    })
  }
  r3 = '(?<![~[:space:]])~([[:alnum:]*()]+?)~'
  if (has_sub <- test_feature('subscript', r3)) {
    id3 = id_string(text)
    if (is.null(p)) p = xfun::prose_index(text)
    text[p]= match_replace(text[p], r3, perl = TRUE, function(x) {
      # place subscripts inside !id...id!
      x = gsub('^~|~$', id3, x)
      sprintf('!%s!', x)
    })
  }
  # disallow single tilde for <del> (I think it is an aweful idea in GFM's
  # strikethrough extension to allow both single and double tilde for <del>)
  if (is.null(p)) p = xfun::prose_index(text)
  text[p] = match_replace(text[p], r3, perl = TRUE, function(x) {
    gsub('^~|~$', '\\\\~', x)
  })

  # TODO: protect ```{=latex} content, and support code highlighting for latex

  ret = do.call(render, c(
    list(text = text),
    options[intersect(names(formals(render)), names(options))]
  ))

  if (format == 'html') {
    ret = tweak_html(ret, text)
    if (has_math) {
      ret = gsub(sprintf('<code>%s(.{5,}?)%s</code>', id, id), '\\1', ret)
      # `\(math\)` may fail to render to <code>\(math\)</code> when backticks
      # are inside HTML tags, e.g., commonmark::markdown_html('<p>`a`</p>')
      ret = gsub(sprintf('`%s\\\\\\((.+?)\\\\\\)%s`', id, id), '$\\1$', ret)
    }
    if (has_sup)
      ret = gsub(sprintf('!%s(.+?)%s!', id2, id2), '<sup>\\1</sup>', ret)
    if (has_sub)
      ret = gsub(sprintf('!%s(.+?)%s!', id3, id3), '<sub>\\1</sub>', ret)
    # restore raw html content from ```{=html}
    r4 = '<pre><code class="language-\\{=html}">((.|\n)+?)</code></pre>'
    ret = match_replace(ret, r4, perl = TRUE, function(x) {
      x = gsub(r4, '\\1', x, perl = TRUE)
      restore_html(x)
    })
    if (isTRUE(options[['toc']])) ret = paste(
      c(build_toc(ret, options[['toc_depth']]), ret), collapse = '\n'
    )
    if (isTRUE(options[['base64_images']])) ret = xfun::in_dir(
      if (is_file(file, TRUE)) dirname(file) else '.', .b64EncodeImages(ret)
    )
  } else if (format == 'latex') {
    if (has_math) {
      m = gregexpr(sprintf('!%s-(\\d+)-%s!', id, id), ret)
      regmatches(ret, m) = lapply(regmatches(ret, m), function(x) {
        if (length(maths) != length(x)) warning(
          'LaTeX math expressions cannot be restored correctly (expected ',
          length(maths), ' expressions but found ', length(x), ' in the output).'
        )
        maths
      })
    }
    if (has_sup)
      ret = gsub(sprintf('!%s(.+?)%s!', id2, id2), '\\\\textsuperscript{\\1}', ret)
    if (has_sub)
      ret = gsub(sprintf('!%s(.+?)%s!', id3, id3), '\\\\textsubscript{\\1}', ret)
    # TODO: if \title{} is empty, remove \maketitle
    # fix horizontal rules from --- (\linethickness doesn't work)
    ret = gsub('{\\linethickness}', '{1pt}', ret, fixed = TRUE)
    ret = redefine_level(ret, options[['top_level']])
  }

  meta$body = ret
  # use the template (if provided) to create a standalone document
  ret = build_output(format, options, template, meta)

  if (is.character(output)) xfun::write_utf8(ret, output) else ret
}

#' @rdname mark
#' @param ... For \code{mark_latex()}, arguments to be passed to
#'   \code{mark()}. For \code{mark_html()}, additional arguments
#'   for backward-compatibility with previous versions of \pkg{markdown}. These
#'   are no longer recommended. For example, the \code{stylesheet} argument
#'   should be replaced by the \code{css} variable in \code{meta}, and the
#'   \code{fragment.only = TRUE} argument should be specified via \code{options
#'   = '-standalone'} instead.
#' @export
#' @examples
#'
#' mark_html('Hello _World_!', options = '-standalone')
#' # write HTML to an output file
#' mark_html('_Hello_, **World**!', output = tempfile())
mark_html = function(
  file = NULL, output = NULL, text = NULL, options = NULL,
  template = NULL, meta = list(), ...
) {
  # for backward-compatibility of arguments `stylesheet`, `title`, `header`, etc.
  extra = list(...)
  # fragment_only -> !standalone (TODO: may drop fragment_only in future)
  if (isTRUE(extra[['fragment.only']]) ||
      (is.character(options) && 'fragment_only' %in% options)) template = FALSE
  css = meta[['css']] %||% extra[['stylesheet']] %||% get_option(
    c('markdown.html.css', 'markdown.html.stylesheet'),
    pkg_file('resources', 'markdown.css')
  )
  meta = normalize_meta(meta)
  title = meta[['title']] %||% extra[['title']]
  header = meta[['header-includes']] %||% extra[['header']] %||%
    get_option('markdown.html.header')

  mark(
    file, output, text, 'html', options, template, merge_list(meta, list(
      css = css, title = title, `header-includes` = header
    ))
  )
}

#' @export
#' @rdname mark
mark_latex = function(...) {
  mark(..., format = 'latex')
}

# insert body and meta variables into a template
build_output = function(format, options, template, meta) {
  if (!isTRUE(options[['standalone']]) || !format %in% c('html', 'latex') ||
      xfun::isFALSE(template)) return(meta$body)
  # TODO: clean up the default HTML template (e.g., use highlight.js from CDN)
  if (is.null(template) || isTRUE(template)) template = get_option(
    sprintf('markdown.%s.template', format),
    pkg_file('resources', sprintf('markdown.%s', format))
  )
  tpl = one_string(template)
  meta = normalize_meta(meta)
  if (format == 'html') {
    b = meta$body
    if (is.null(meta[['title']])) meta$title = first_header(b)
    if (is.null(length(meta[['math']])))
      meta$math = if (isTRUE(options[['mathjax']]) && .requiresMathJax(b)) {
        .mathJax(embed = isTRUE(options[['mathjax_embed']]))
      }
    if (is.null(meta[['highlight']]))
      meta$highlight = if (isTRUE(options[['highlight_code']]) && .requiresHighlight(b)) {
        pkg_file('resources', 'r_highlight.html')
      }
    tpl = tpl_html(tpl)
  }
  # find all variables in the template
  vars = unlist(regmatches(tpl, gregexpr('[$][-[:alnum:]]+[$]', tpl)))
  # insert $body$ at last in case the body contain any $variables$ accidentally
  if (!is.na(i <- match('$body$', vars))) vars = c(vars[-i], vars[i])
  for (v in vars) {
    tpl = sub_var(tpl, v, meta[[gsub('[$]', '', v)]])
  }
  tpl
}

# fix variable names for backward-compatibility
tpl_html = function(x) {
  x = sub_var(x, '#!markdown_css#', '$css$')
  x = sub_var(x, '#!header#', '$header-includes$')
  x = sub_var(x, '#!title#', '$title$')
  x = sub_var(x, '#!mathjax#', '$math$')
  x = sub_var(x, '#!r_highlight#', '$highlight$')
  x = sub_var(x, '#!html_output#', '$body$')
  x
}

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
#' \item{\code{highlight_code}}{Includes JavaScript libraries to syntax
#' highlight code blocks.}
#'
#' \item{\code{latex_math}}{Identify LaTeX math expressions in pairs of single
#' or double dollar signs, and transform them so that they could be correctly
#' rendered by MathJax (HTML output) or LaTeX.}
#'
#' \item{\code{mathjax}}{Include MathJax library in HTML output.}
#'
#' \item{\code{mathjax_embed}}{Whether to download and embed the MathJax library
#' in HTML output.}
#'
#' \item{\code{smartypants}}{Translate certain ASCII strings into smart
#' typographic characters (see \code{\link{smartypants}()}.}
#'
#' \item{\code{standalone}}{Generate a full (HTML/LaTeX) document or only a
#' fragment of the body.}
#'
#' \item{\code{superscript}}{Translate strings between two carets into
#' superscripts, e.g., \verb{text^foo^} to \verb{text<sup>foo</sup>}.}
#'
#' \item{\code{subscript}}{Translate strings between two tildes into subscripts,
#' e.g., \verb{text~foo~} to \verb{text<sub>foo</sub>}.}
#'
#' \item{\code{toc}}{Generate a table of contents from section headers.}
#'
#' \item{\code{toc_depth}}{The number of section levels to include in the table
#' of contents (\code{3} by default).}
#'
#' \item{\code{top_level}}{The desired type of the top-level headings in LaTeX
#' output. Possible values are \code{'chapter'} and \code{'part'}. For example,
#' if \code{top_level = 'chapter'}, \code{# header} will be rendered to
#' \verb{\chapter{header}} instead of the default \verb{\section{header}}.}
#'
#' }
#'
#' Options not described above can be found on the help pages of
#' \pkg{commonmark}, e.g., the \code{hardbreaks} option is for the
#' \code{hardbreaks} argument of
#' \code{\link[commonmark:commonmark]{markdown_*}()} functions, and the
#' \code{table} option is for the \code{table} extension in \pkg{commonmark}'s
#' extensions (\code{commonmark::\link[commonmark]{list_extensions}()}).
#' @return A character vector of all available options.
#' @export
#' @examples
#' # List all available options
#' markdown::markdown_options()
#'
#' # Turn on/off some options globally for HTML output
#' options(markdown.html.options = '+toc-smartypants-standalone')
#'
#' @example inst/examples/render-options.R
markdown_options = function() {
  # options enabled by default
  x1 = c(
    'smart', 'smartypants', 'base64_images', 'mathjax', 'highlight_code',
    'superscript', 'subscript', 'latex_math', 'standalone',
    setdiff(commonmark::list_extensions(), 'tagfilter')
  )
  # options disabled by default
  x2 = c('toc', 'hardbreaks', 'tagfilter', 'mathjax_embed')
  # TODO: remove this hack after https://github.com/kiernann/gluedown/pull/29
  if (xfun::check_old_package('gluedown', '1.0.4')) {
    x1 = setdiff(x1, c('tasklist', 'smart'))
    x2 = c(x2, c('tasklist', 'smart'))
  }
  sort(c(paste0('+', x1), paste0('-', x2)))
}

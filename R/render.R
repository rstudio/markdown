#' Render Markdown to an output format
#'
#' This is a wrapper function based on [litedown::mark()]. You should use
#' `litedown::mark()` directly.
#' @param file,output,text,options,meta Passed to [litedown::mark()].
#' @param format Output format name.
#' @param template Whether to use a built-in template, or path to a custom
#'   template.
#' @import utils
#' @export
mark = function(
  file = NULL, output = NULL, text = NULL, format = c('html', 'latex'),
  options = NULL, template = FALSE, meta = list()
) {
  format = format[1]
  opts = options(stats::setNames(
    list(get_option(sprintf('markdown.%s.template', format), template)),
    sprintf('litedown.%s.template', format)
  ))
  on.exit(options(opts), add = TRUE)
  if (is.null(output)) output = format
  litedown::mark(file, output, text, options, meta)
}

#' @rdname mark
#' @param ... Arguments to be passed to `mark()`.
#' @export
#' @examples
#'
#' mark_html('Hello _World_!', template = FALSE)
#' # write HTML to an output file
#' mark_html('_Hello_, **World**!', output = tempfile())
mark_html = function(..., template = TRUE) {
  mark(..., format = 'html', template = template)
}

#' @export
#' @rdname mark
#' @examples
#'
#' mark_latex('Hello _World_!', template = FALSE)
mark_latex = function(..., template = TRUE) {
  mark(..., format = 'latex', template = template)
}

#' Markdown rendering options
#'
#' A wrapper function of [litedown::markdown_options()].
#' @export
markdown_options = function() litedown::markdown_options()

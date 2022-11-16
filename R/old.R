#' Functions in \pkg{markdown} before \code{v1.3}.
#'
#' These functions are kept in this package for backward-compatibility. They
#' will not be removed in the foreseeable future, although we recommend that you
#' use their new names instead: \code{renderMarkdown()} has become
#' \code{\link{mark}()}, and \code{markdownToHTML()} has become
#' \code{\link{mark_html}()}.
#' @param ...,options,template Arguments to be passed to new functions.
#' @param title,stylesheet,header Arguments to be passed to \code{meta =
#'   list(title = , css = , `header-includes` = )}, which is passed to
#'   \code{mark_html()}.
#' @param fragment.only Whether to generate a fragment or a full HTML document.
#' @param encoding Ignored.
#' @export
#' @keywords internal
renderMarkdown = function(...) mark(...)

#' @rdname renderMarkdown
#' @export
markdownToHTML = function(
  ..., options = getOption('markdown.HTML.options'),
  title = NULL, stylesheet = getOption('markdown.HTML.stylesheet'),
  header = getOption('markdown.HTML.header'),
  template = getOption('markdown.HTML.template', TRUE),
  fragment.only = FALSE, encoding = 'UTF-8'
) {
  if (fragment.only || 'fragment_only' %in% options) template = FALSE
  meta = list()
  meta$css = stylesheet
  meta$title = title
  meta$`header-includes` = header
  mark_html(..., options = options, template = template, meta = meta)
}

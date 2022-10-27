#' Functions in \pkg{markdown} before \code{v1.3}.
#'
#' These functions are kept in this package for backward-compatibility. They
#' will not be removed in the foreseeable future, although we recommend that you
#' use their new names instead: \code{renderMarkdown()} has become
#' \code{\link{mark}()}, and \code{markdownToHTML()} has become
#' \code{\link{mark_html}()}.
#' @param ... Arguments to be passed to new functions.
#' @export
#' @keywords internal
renderMarkdown = function(...) mark(...)

#' @rdname renderMarkdown
#' @export
markdownToHTML = function(...) mark_html(...)

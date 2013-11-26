#' Markdown rendering for R
#'
#' \pkg{Markdown} is a plain-text formatting syntax that can be converted to
#' XHTML or other formats. This package provides R bindings to the Sundown
#' (\url{https://github.com/vmg/sundown}) markdown rendering library.
#'
#' The R function \code{\link{markdownToHTML}} renders a markdown file to HTML
#' (respecting the specified \code{\link{markdownExtensions}} and
#' \code{\link{markdownHTMLOptions}}).
#'
#' The package also exports the underlying Sundown C extension API which enables
#' creating and calling custom renderers using the \code{\link{renderMarkdown}}
#' function.
#'
#' To learn more about markdown syntax see:
#'
#' \url{http://en.wikipedia.org/wiki/Markdown}
#' @name markdown
#' @docType package
#' @author JJ Allaire, Jeffrey Horner, Vicent Marti, and Natacha Porte
#'
#'   Maintainer: Yihui Xie <xie@@yihui.name>
#' @seealso \code{\link{markdownToHTML}} \code{\link{renderMarkdown}}
#' @useDynLib markdown, .registration = TRUE
#' @keywords package
NULL

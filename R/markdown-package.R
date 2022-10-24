#' Markdown rendering for R
#'
#' \pkg{Markdown} is a plain-text formatting syntax that can be converted to
#' XHTML or other formats. This package provides wrapper functions (mainly
#' \code{\link{renderMarkdown}()}) based on the \pkg{commonmark} package.
#' @name markdown
#' @docType package
#' @seealso \code{\link{renderMarkdown}()}, \code{\link{markdownToHTML}()}
#' @keywords package
NULL

CHARS = c(letters, LETTERS, 0:9, '!', ',', '/', ':', ';', '=', '@')

# generate a random string that is not present in provided text
id_string = function(text, lens = c(2:10, 20), times = 20) {
  for (i in lens) {
    for (j in seq_len(times)) {
      id = paste(sample(CHARS, i, replace = TRUE), collapse = '')
      if (length(grep(id, text, fixed = TRUE)) == 0) return(id)
    }
  }
  # failure should be very rare
  stop('Failed to generate a unique ID string. You may try again.')
}

# a shorthand for gregexpr() and regmatches()
match_replace = function(x, pattern, replace = identity, ...) {
  m = gregexpr(pattern, x, ...)
  regmatches(x, m) = lapply(regmatches(x, m), replace)
  x
}

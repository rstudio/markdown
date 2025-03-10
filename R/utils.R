#' Convert some ASCII strings to HTML entities
#'
#' Transform ASCII strings `(c)` (copyright), `(r)` (registered trademark),
#' `(tm)` (trademark), and fractions `n/m` into *smart* typographic HTML
#' entities.
#' @param text A character vector of the Markdown text.
#' @return A character vector of the transformed text.
#' @export
#' @examples
#' cat(smartypants("1/2 (c)\n"))
smartypants = function(text) litedown:::smartypants(text)

# get an option using a case-insensitive name
get_option = function(name, default = NULL) {
  x = options()
  i = match(tolower(name), tolower(names(x)))
  i = i[!is.na(i)]
  if (length(i) == 0) default else x[[i[1]]]
}

pkg_file = function(...) {
  system.file(..., package = 'markdown', mustWork = TRUE)
}

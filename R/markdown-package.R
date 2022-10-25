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

# TODO: remove these functions when xfun 0.35 is released to CRAN
protect_math = function(x, token = '') {
  i = xfun::prose_index(x)
  if (length(i)) x[i] = escape_math(x[i], token)
  x
}
escape_math = function(x, token = '') {
  m = gregexpr('(?<=^|[\\s])[$](?! )[^$]+?(?<! )[$](?![$0123456789])', x, perl = TRUE)
  regmatches(x, m) = lapply(regmatches(x, m), function(z) {
    if (length(z) == 0) return(z)
    z = sub('^[$]', paste0('`', token, '\\\\('), z)
    z = sub('[$]$', paste0('\\\\)', token, '`'), z)
    z
  })
  m = gregexpr('(?<=^|[\\s])[$][$](?! )[^$]+?(?<! )[$][$]', x, perl = TRUE)
  regmatches(x, m) = lapply(regmatches(x, m), function(z) {
    if (length(z) == 0) return(z)
    paste0('`', token, z, token, '`')
  })
  i = vapply(gregexpr('[$]', x), length, integer(1)) == 2
  if (any(i)) {
    x[i] = gsub('^([$][$])([^ ]+)', paste0('`', token, '\\1\\2'), x[i], perl = TRUE)
    x[i] = gsub('([^ ])([$][$])$', paste0('\\1\\2', token, '`'), x[i], perl = TRUE)
  }
  i1 = grep('^\\\\begin\\{[^}]+\\}$', x)
  i2 = grep('^\\\\end\\{[^}]+\\}$', x)
  if (length(i1) == length(i2)) {
    x[i1] = paste0('`', token, x[i1])
    x[i2] = paste0(x[i2], token, '`')
  }
  x
}

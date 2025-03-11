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
  # check if bibutils is available for bibliography, and convert yes/no to Boolean
  if (litedown:::is_file(file)) {
    parts = xfun::yaml_body(xfun::read_utf8(file), parse = FALSE)
    yaml = parts$yaml
    if (length(i <- grep('^bibliography:\\s+', yaml)) && !xfun::loadable('rbibutils')) {
      if (xfun::is_R_CMD_check()) {
        yaml = yaml[-i]
      } else stop(
        'Detected bibliography in YAML (', file, ') but the rbibutils package is ',
        'unavailable. Please make sure it is installed (and declared in Suggests ',
        'if bibliography is used in package vignettes).'
      )
    }
    if (length(i <- grep(':\\s+(yes|no)\\s*$', yaml))) {
      if (xfun::is_R_CMD_check()) {
        yaml[i] = sub('yes\\s*$', 'true', yaml[i])
        yaml[i] = sub('no\\s*$', 'false', yaml[i])
      } else stop(
        'Detected yes/no in YAML(', file, '). Please replace them with true/false.'
      )
    }
    if (xfun::is_R_CMD_check()) {
      xfun::write_utf8(c('---', yaml, '---', parts$body), file)
    }
  }
  res = litedown::mark(file, output, text, options, meta)
  if (format == 'html') {
    # remove sec/chp prefix in section IDs
    res = gsub('(<h[1-6] id=")(sec|chp):([^"]+")', '\\1\\3', res)
  }
  as.character(res)
}

#' @rdname mark
#' @param ... Arguments to be passed to `mark()`.
#' @export
mark_html = function(..., template = TRUE) {
  mark(..., format = 'html', template = template)
}

#' @export
#' @rdname mark
mark_latex = function(..., template = TRUE) {
  mark(..., format = 'latex', template = template)
}

#' Markdown rendering options
#'
#' A wrapper function of [litedown::markdown_options()].
#' @export
markdown_options = function() litedown::markdown_options()

output_format = function(to = 'html') {
  to
  function(
    meta = NULL, template = NULL, options = NULL, keep_md = FALSE,
    keep_tex = FALSE, latex_engine = 'xelatex'
  ) {
    opts = rmarkdown::pandoc_options(
      to = to, keep_tex = keep_tex, latex_engine = latex_engine, args = '--template'
    )
    opts$convert_fun = function(input, output, ...) {
      mark(input, output, NULL, to, options, template, meta)
    }
    rmarkdown::output_format(
      NULL, opts, keep_md = keep_md,
      clean_supporting = 'local' %in% litedown:::normalize_options(options)[['embed_resources']]
    )
  }
}

#' R Markdown output formats
#'
#' Convenience functions for R Markdown v2 users.
#'
#' We refer to this \pkg{markdown} package plus \pkg{knitr} as \dQuote{R
#' Markdown v1}, and the \pkg{rmarkdown} package as \dQuote{R Markdown v2}. The
#' former uses \pkg{commonmark} to convert Markdown, and the latter uses Pandoc.
#' However, the latter also accept custom converting tools in addition to
#' Pandoc. The output formats here provide the custom converting function
#' [mark()] to \pkg{rmarkdown}, so that users can take advantage of
#' [rmarkdown::render()] and the Knit button in RStudio. It is absolutely not
#' necessary to rely on \pkg{rmarkdown}. The only point is convenience. If you
#' do not use `rmarkdown::render()` or the Knit button, you can definitely just
#' call `markdown::mark()` directly.
#' @param meta,template,options Arguments to be passed to [mark()].
#' @param keep_md,keep_tex Whether to keep the intermediate \file{.md} and
#'   \file{.tex} files generated from \file{.Rmd}.
#' @param latex_engine The LaTeX engine to compile \file{.tex} to \file{.pdf}.
#'   This argument and `keep_tex` are for `latex_format()` only, and ignored in
#'   `html_format()`.
#' @export
html_format = output_format('html')

#' @rdname html_format
#' @export
latex_format = output_format('latex')

# compatibility layers to rmarkdown::[html|pdf]_document
html_document = function(...) do.call(html_format, map_args(...))
html_vignette = function(...) html_document(...)
pdf_document = function(...) do.call(latex_format, map_args(...))

map_args = function(...) {
  do.call(litedown:::map_args, convert_yn(list(...)))
}

convert_yn = function(x) {
  lapply(x, function(z) {
    if (is.list(z)) convert_yn(z) else if (identical(z, 'yes')) TRUE else
      if (identical(z, 'no')) FALSE else z
  })
}

# Markdown rendering for R

<!-- badges: start -->

[![R-CMD-check](https://github.com/rstudio/markdown/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/rstudio/markdown/actions/workflows/R-CMD-check.yaml)

<!-- badges: end -->

## Overview

Markdown is a plain-text formatting syntax that can be converted to HTML or
other formats. This package provides wrappers based on the
[commonmark](https://github.com/r-lib/commonmark) package.

The main function `mark()` renders Markdown to various output formats supported
by **commonmark**; `mark_html()` is a wrapper function to render Markdown to
HTML, and `mark_latex()` is a wrapper function for LaTeX output. Options
controlling output and supported Markdown extensions can be optionally
specified.

To learn more about Markdown syntax see: <https://github.github.com/gfm/>

This package is referred to as *R Markdown v1* when combined with **knitr**. The
primary output format is HTML. In 2014, we introduced [*R Markdown
v2*](https://posit.co/blog/r-markdown-v2/), which is based on Pandoc and
**knitr**, and supports many more types of output formats. Please see
<https://rmarkdown.rstudio.com> for details.

## License

The **markdown** package is licensed under the GPLv2.

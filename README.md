# Markdown rendering for R

<!-- badges: start -->

[![R-CMD-check](https://github.com/rstudio/markdown/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/rstudio/markdown/actions/workflows/R-CMD-check.yaml)

<!-- badges: end -->

## Overview

*Markdown* is a plain-text formatting syntax that can be converted to XHTML or
other formats. This package provides wrappers based on the
[commonmark](https://github.com/r-lib/commonmark) package.

The R function `markdownToHTML()` renders a Markdown file to HTML. Options
controlling HTML output and supported Markdown extensions can be optionally
specified.

To learn more about Markdown syntax see:
<https://en.wikipedia.org/wiki/Markdown>

This package is referred to as *R Markdown v1* when combined with **knitr**. The
primary output format is HTML. We do not plan to add new features to this
package in the future, and please consider this package to be in the maintenance
mode only. In 2014, we introduced [*R Markdown
v2*](https://www.rstudio.com/blog/r-markdown-v2/), which is based on Pandoc and
**knitr**, and supports much more types of output formats. Please see
<https://rmarkdown.rstudio.com> for details.

## License

The **markdown** package is licensed under the GPLv2.

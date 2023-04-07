# Markdown rendering for R

<!-- badges: start -->

[![R-CMD-check](https://github.com/rstudio/markdown/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/rstudio/markdown/actions/workflows/R-CMD-check.yaml)
[![CRAN
release](https://www.r-pkg.org/badges/version/markdown)](https://cran.r-project.org/package=markdown)

<!-- badges: end -->

## Overview

Markdown is a plain-text formatting syntax that can be converted to HTML or
other formats. This package provides wrappers based on the
[commonmark](https://github.com/r-lib/commonmark) package.

The main function `mark()` renders Markdown to various output formats supported
by **commonmark**; `mark_html()` is a wrapper function to render Markdown to
HTML, and `mark_latex()` is a wrapper function for LaTeX output.

This package supports a series of extra Markdown features that are not in
**commonmark**, such as LaTeX math, raw HTML/LaTeX blocks, fenced Divs,
heading/image attributes, and footnotes, etc. The Markdown rendering behavior
can be controlled by several options, e.g., whether to enable the table of
contents. It also supports custom HTML/LaTeX templates. To get a full
introduction, please read [the `intro`
vignette](https://cran.r-project.org/package=markdown/vignettes/intro.html).

You may also read [the `article`
vignette](https://cran.r-project.org/package=markdown/vignettes/article.html)
and [the `slides`
vignette](https://cran.r-project.org/package=markdown/vignettes/slides.html) to
learn more about possible applications based on this package.

### R Markdown

This package is referred to as the first generation (v1) of R Markdown when
combined with [**knitr**](https://github.com/yihui/knitr). The primary output
formats are HTML and LaTeX. In 2014, we introduced the second generation (v2) of
R Markdown, i.e., the [**rmarkdown**](https://github.com/rstudio/rmarkdown)
package, which is based on Pandoc and **knitr**, and supports many more types of
output formats.

It is a deliberate design choice to keep this **markdown** package lightweight,
to make it easy to use and simple to maintain. We may be conservative when
considering new feature requests. It is definitely not the goal to become a
substitute of tools based on Pandoc, such as **rmarkdown** and
[Quarto](https://quarto.org). To some degree, this package is intended for
minimalists, and does not aim at rich features.

## License

The **markdown** package is licensed under MIT.

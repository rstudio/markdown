Markdown rendering for R
=============================================================================

[![Build Status](https://travis-ci.org/rstudio/markdown.svg)](https://travis-ci.org/rstudio/markdown)
[![Downloads from the RStudio CRAN mirror](https://cranlogs.r-pkg.org/badges/markdown)](https://cran.r-project.org/package=markdown)

Overview
-----------------------------------------------------------------------------

*Markdown* is a plain-text formatting syntax that can be converted
to XHTML or other formats. This package provides R bindings to the
[Sundown](https://github.com/vmg/sundown) markdown rendering library.

The R function `markdownToHTML` renders a markdown file to HTML. Options
controlling HTML output and supported markdown extensions can be optionally
specified.

The package also exports the underlying Sundown C extension API which
enables creating and calling custom renderers using the `renderMarkdown`
function.

To learn more about markdown syntax see: <http://en.wikipedia.org/wiki/Markdown>

This package is referred to as _R Markdown v1_ when combined with **knitr**. The primary output format is HTML. We do not plan to add new features to this package in the future, and please consider this package to be in the maintenance mode only. In 2014, we introduced [_R Markdown v2_](http://blog.rstudio.org/2014/06/18/r-markdown-v2/), which is based on Pandoc and **knitr**, and supports much more types of output formats. Please see https://rmarkdown.rstudio.com for details.

License
-----------------------------------------------------------------------------

The markdown package is licensed under the GPLv2. See these files for
additional details:

- inst/COPYING - Markdown package license (GPLv2)
- inst/NOTICE  - Copyright notices for additional included software

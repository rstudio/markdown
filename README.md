Markdown rendering for R
=============================================================================

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

License
-----------------------------------------------------------------------------

The markdown package is licensed under the GPLv2. See these files for
additional details:

- inst/COPYING - Markdown package license (GPLv2)
- inst/NOTICE  - Copyright notices for additional included software

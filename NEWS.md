# CHANGES IN markdown VERSION 1.3

- Replaced the underlying Markdown rendering engine from the C library **sundown** (which has been deprecated for a decade) to the R package **commonmark**.

- Renamed the argument `stylesheet` to `css` in `markdownToHTML()`, and removed the argument `fragment.only` (please use `markdownToHTML(options = '-standalone')` instead of `fragment.only = TRUE`).

- The `file` argument of `renderMarkdown()` and `markdownToHTML()` can also take a character vector of Markdown text now.

- Removed functions `rendererExists()`, `rendererOutputType()`, and `registeredRenderer()`. They were primarily for internal use.

- Deprecated the function `markdownExtensions()`. All extensions should be specified via the `options` argument of functions like `renderMarkdown()`, e.g., `renderMarkdown(options = '+table+tasklist')`. See all options on the help page `?markdown::markdownOptions`.

- Renamed `markdownHTMLOptions()` to `markdownOptions()`.

# CHANGES IN markdown VERSION 1.2

- Fixed the warnings "a function declaration without a prototype is deprecated in all versions of C" (#94).

# CHANGES IN markdown VERSION 1.1

## MAJOR CHANGES

- renderMarkdown() and markdownToHTML() will signal an error if the input file is not encoded in "UTF-8".

# CHANGES IN markdown VERSION 1.0

## MAJOR CHANGES

- The default value of the encoding argument of renderMarkdown() and markdownToHTML() has been changed from getOption("encoding") to "UTF-8". The encoding of the input file will always be assumed to be UTF-8.

- markdownToHTML() will return a character vector encoded in UTF-8 (instead of the system's native encoding) when not writing to an output file.

# CHANGES IN markdown VERSION 0.9

## BUG FIXES

- Fixed clang-UBSAN and valgrind issues (thanks, @yixuan, #92).

# CHANGES IN markdown VERSION 0.8

## MINOR CHANGES

- the MathJax CDN URL was replaced by https://www.bootcdn.cn/mathjax/

## BUG FIXES

- fixed https://github.com/rstudio/htmltools/issues/30: markdownToHTML() did not work with empty files (thanks, @VermillionAzure)

# CHANGES IN markdown VERSION 0.7.7

## BUG FIXES

- renderMarkdown() works now even if text = character(0) or ""

- added an `encoding` argument to renderMarkdown() since multi-byte characters in renderMarkdown() did not work on Windows (thanks, Kohske Takahashi, #63)

- fixed #64: invalid 'n' argument in rpubsUpload() (thanks, Wouter van Atteveldt)

## MAJOR CHANGES

- if renderMarkdown() returns a character vector, it will be marked with the UTF-8 encoding if it contains multi-byte characters

# CHANGES IN markdown VERSION 0.7.4

## NEW FEATURES

- when an image is the only element of its parent node in the HTML output document, it is automatically centered on the page

## MINOR CHANGES

- images that have already been base64 encoded will not be encoded again (#61)

- the URL of the MathJax CDN was updated to cdn.mathjax.org

# CHANGES IN markdown VERSION 0.7.2

## BUG FIXES

- fixed #60: MathJax may be included even if it is unnecessary when syntax highlighting is enabled (thanks, @aoles)

- fixed a bug which may hang R when building R Markdown vignettes in a wrong locale (thanks, Dan Tenenbaum, yihui/knitr#782)

# CHANGES IN markdown VERSION 0.7

## BUG FIXES

- if both the 'file' and 'text' arguments are provided but file = NULL, e.g. markdownToHTML(file = NULL, text = ?), markdownToHTML() can throw an error indicating the file is invalid (thanks, Tyler Rinker, hadley/staticdocs#66)

- markdownToHTML(text = ?, output = ?) was broken (#54)

# CHANGES IN markdown VERSION 0.6.5

## NEW FEATURES

- added an argument 'encoding' to markdownToHTML() to specify the character encoding of the input markdown file, and the HTML output file is always encoded in UTF-8 now (thanks, Kohske Takahashi, #50)

# CHANGES IN markdown VERSION 0.6.4

## NEW FEATURES

- added 'mathjax_embed' to HTML options for embedding the MathJax JavaScript in the HTML document rather than linking to it online. Note the JavaScript code is read from the http instead of https MathJax URL. Contributed by Henrik Bengtsson.

- added another vignette to show the HTML output of the original vignette (see browseVignettes('markdown'))

- the default CSS style was tweaked (major changes include: page width is at most 800px, more line height, slightly larger fonts, and a different syntax highlighting theme)

# CHANGES IN markdown VERSION 0.6.3

## NEW FEATURES

- added a new argument 'template' to markdownToHTML() so that we can customize the HTML template (by default, it uses the template 'resources/markdown.html' in this package); thanks, Nacho Caballero

- the options markdown.HTML.stylesheet and markdown.HTML.header used in markdownToHTML() can be character vectors (they will be processed by paste(x, collapse = '\n')

## MAJOR CHANGES

- the 'text' argument in markdownToHTML() and renderMarkdown() is treated as lines of input now, i.e. if 'text' is provided, it is passed to the markdown renderer as paste(text, collapse = '\n'); in the previous versions, it was processed by paste(text, collapse = '')

# CHANGES IN markdown VERSION 0.6

## DOCUMENTATION

- added a package vignette; see browseVignettes(package = 'markdown')

# CHANGES IN markdown VERSION 0.5.5

## NEW FEATURES

- added a new argument 'header' to markdownToHTML() to insert code into the HTML header (e.g. custom CSS styles)

## BUG FIXES

- fixed #25 and #27: minor documentation problems

- fixed #26: the HTML output file will be written relative to the current working directory now when it contains images that need to be base64 encoded

- fixed #28: the image URL should be decoded before the image is based64 encoded

## MISC

- Yihui Xie has taken over the maintainership for this package from Jeffrey Horner

# CHANGES IN markdown VERSION 0.5.4

## NEW FEATURES

- Both Pandoc title blocks and Jekyll front matter sections are skipped when rendering markdown documents.

# CHANGES IN markdown VERSION 0.5.3

## NEW FEATURES

- C/C++ is now a supported language for code block highlighting.

## MAJOR CHANGES

- 'hard_wrap' has been dropped while 'mathjax' and 'highlight_code' have been added to the default list of html options.

## BUG FIXES

- fixed parsing of math equations when located at the end of a line.

# CHANGES IN markdown VERSION 0.5.2

## NEW FEATURES

- with the new 'latex_math' markdown extensions, users can include math equations using several syntaxes. For block level equations, use $$latex ... $$, $$ ... $$, or \[ ... \]. For inline equations, use $latex...$, $...$, or \( ... \).

## MAJOR CHANGES

- the markdown extension 'ingore_math' was replaced with 'latex_math'.

- users can now use the markdown.HTML.stylesheet option to override the package default stylesheet.

- setting the fragment_only rendering option or the fragment.only parameter to markdownToHTML will base64 encode images if applicable. version 0.5.1 did not.

# CHANGES IN markdown VERSION 0.5.1

## BUG FIXES

- fixed a GUIDgenerator bug; for escaping math equations before markdown parsing begins.

- image encoding was fixed for the case when there are more than one included in a markdown document.

# CHANGES IN markdown VERSION 0.5

## NEW FEATURES

- added fragment.only parameter to markdownToHTML

- added new html rendering options base64_images, fragment_only, mathjax, and highlight_code

- added new markdown extension ignore_math

## MAJOR CHANGES

- removed safelink from default html rendering options

- the default html rendering options are now hard_wrap, use_xhtml, smartypants, and base64_images.

## BUG FIXES

- fixed syntax errors in C exports

# CHANGES IN markdown VERSION 0.4

## NEW FEATURES

- added support for post-processing HTML using smartypants filter

- added optional support for rendering a table of contents

## MAJOR CHANGES

- changed exported C functions to use an rmd_ prefix (eliminating potential symbol conflicts with other packages)

- changed default html rendering options to use_xhtml, hard_wrap, safelink, and smartypants

## BUG FIXES

- eliminated name collision with render_markdown function in knitr


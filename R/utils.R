`%||%` = function(x, y) if (length(x)) x else y

#' Convert some ASCII strings to HTML entities
#'
#' Transform ASCII strings \verb{(c)} (copyright), \verb{(r)} (registered
#' trademark), \verb{(tm)} (trademark), and fractions \verb{n/m} into
#' \emph{smart} typographic HTML entities.
#' @param text A character vector of the Markdown text.
#' @return A character vector of the transformed text.
#' @export
#' @examples
#' cat(smartypants("1/2 (c)\n"))
smartypants = function(text) {
  text = xfun::split_lines(text)
  i = xfun::prose_index(text)
  r = '(?<!`)\\((c|r|tm)\\)|(\\d+/\\d+)(?!`)'
  text[i] = match_replace(text[i], r, perl = TRUE, function(z) {
    y = pants[z]
    i = is.na(y)
    y[i] = z[i]
    y
  })
  text
}

# Represent some fractions with HTML entities
fracs = local({
  n1 = c(
    '1/2', '1/3', '2/3', '1/4', '3/4', '1/5', '2/5', '3/5', '4/5', '1/6', '5/6',
    '1/8', '3/8', '5/8', '7/8'
  )
  n2 = c('1/7', '1/9', '1/10')
  x2 = seq_along(n2) + 8527  # &#8528;, 8529, 8530
  setNames(c(sprintf('&frac%s;', gsub('/', '', n1)), sprintf('&#%d;', x2)), c(n1, n2))
})

pants = c(fracs, c('(c)' = '&copy;', '(r)' = '&reg;', '(tm)' = '&trade;'))

# merge a later list in arguments into a former one by name
merge_list = function(...) {
  dots = list(...)
  res  = dots[[1]]
  for (i in seq_along(dots) - 1L) {
    if (i == 0) next
    x = dots[[i + 1]]
    if (!is.list(x)) next
    res[names(x)] = x
  }
  res
}

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
  regmatches(x, m) = lapply(regmatches(x, m), function(z) {
    if (length(z)) replace(z) else z
  })
  x
}

# *guess* if an input is a file: if ext = TRUE, avoid guessing by file extension
is_file = function(x, ext = FALSE) {
  length(x) == 1 && !inherits(x, 'AsIs') && nchar(x) < 1000 && !grepl('\n', x) && (
    xfun::file_exists(x) || (!ext && xfun::file_ext(x) != '')
  )
}

# substitute a variable in template `x` with its value; the variable may have
# more than one possible name, in which case we try them one by one
sub_var = function(x, name, value) {
  for (i in name) {
    if (any(grepl(i, x, fixed = TRUE))) {
      return(sub(i, one_string(value), x, fixed = TRUE))
    }
  }
  x
}

# unescape HTML code
restore_html = function(x) {
  x = gsub('&quot;', '"', x, fixed = TRUE)
  x = gsub('&amp;', '&', x, fixed = TRUE)
  x = gsub('&lt;', '<', x, fixed = TRUE)
  x = gsub('&gt;', '>', x, fixed = TRUE)
  x
}

# find the first heading in html
first_heading = function(html) {
  m = regexpr(r <- '<(h[1-6])[^>]*?>(.+?)</\\1>', html, perl = TRUE)
  gsub(r, '\\2', regmatches(html, m))
}

.requireMathJS = function(x) {
  regs = c('\\\\\\(.+?\\\\\\)', '[$]{2}.+?[$]{2}', '\\\\\\[.+?\\\\\\]')
  for (i in regs) if (any(grepl(i, x, perl = TRUE))) return(TRUE)
  FALSE
}

# set js/css variables according to the js_math option
set_math = function(meta, options, html) {
  o = js_options(options[['js_math']], 'katex', .requireMathJS(html))
  if (is.null(o)) return(meta)
  if (is_katex <- o$package == 'katex')
    o$js = c(o$js, 'dist/contrib/auto-render.min.js')
  js = paste0('@', paste(c(
    sprintf('npm/%s%s/%s', o$package, o$version, o$js),
    if (is_katex) 'npm/@xiee/utils/js/render-katex.js'
  ), collapse = ','))
  css = sprintf('@npm/%s%s/%s', o$package, o$version, o$css)
  add_meta(meta, list(js = js, css = css))
}

js_options = function(x, default, test) {
  d = js_default(x, default)
  x = if (is.list(x)) merge_list(d, x) else d
  if (is.null(x) || !test) return()
  if (x$version != '') x$version = sub('^@?', '@', x$version)
  x
}

js_default = function(x, default) {
  if (is.list(x)) x = x$package
  if (is.null(x) || isTRUE(x)) x = default
  if (is.character(x)) merge_list(js_libs[[x]], list(package = x))
}

js_libs = list(
  highlight = list(
    version = '11.7.0', style = 'github', js = 'build/highlight.min.js'
  ),
  katex = list(version = '', css = 'dist/katex.min.css', js = 'dist/katex.min.js'),
  mathjax = list(version = '3', js = 'es5/tex-mml-chtml.js'),
  prism = list(
    version = '1.29.0', style = 'prism', js = 'components/prism-core.min.js'
  )
)

add_meta = function(x, v) {
  for (i in names(v)) x[[i]] = c(v[[i]], x[[i]])
  x
}

# set js/css variables according to the js_highlight option
set_highlight = function(meta, options, html) {
  r = '(?<=<code class="language-)([^"]+)(?=")'
  o = js_options(options[['js_highlight']], 'prism', any(grepl(r, html, perl = TRUE)))
  if (is.null(o)) return(meta)

  p = o$package
  # return jsdelivr subpaths
  get_path = function(path) {
    t = switch(
      p, highlight = '@gh/highlightjs/cdn-release%s/%s', prism = '@npm/prismjs%s/%s'
    )
    sprintf(t, o$version, path)
  }
  # add the `prism-` prefix if necessary
  normalize_prism = function(x) {
    if (length(x) == 1 && x == 'prism') x else sub('^(prism-)?', 'prism-', x)
  }

  # if resources need to be embedded, we need to work harder to figure out which
  # js files to embed (this is quite tricky and may not be robust)
  embed = 'https' %in% options[['embed_resources']]

  # style -> css
  if (is.null(o$css) && !is.null(s <- o$style)) o$css = switch(
    p,
    highlight = sprintf('build/styles/%s.min.css', s),
    prism = sprintf('themes/%s.min.css', normalize_prism(s))
  )
  css = get_path(o$css)

  # languages -> js
  get_lang = function(x) switch(
    p,
    highlight = sprintf('build/languages/%s.min.js', x),
    prism = sprintf('components/%s.min.js', normalize_prism(x))
  )
  o$js = c(o$js, if (is.null(l <- o$languages)) {
    # detect <code> languages in html and load necessary language components
    lang = unique(unlist(regmatches(html, gregexpr(r, html, perl = TRUE))))
    if (embed) {
    } else {
      if (p == 'prism') 'plugins/autoloader/prism-autoloader.min.js'
    }
  } else get_lang(l))
  js = get_path(o$js)
  if (p == 'highlight') js = c(js, '@npm/@xiee/utils/js/load-highlight.js')

  add_meta(meta, list(js = js, css = css))
}

# get an option using a case-insensitive name
get_option = function(name, default = NULL) {
  x = options()
  i = match(tolower(name), tolower(names(x)))
  i = i[!is.na(i)]
  if (length(i) == 0) default else x[[i[1]]]
}

# if a string is a file path, read the file; then concatenate elements by \n
one_string = function(x) {
  if (!is.character(x)) return('')
  if (is_file(x, TRUE)) x = xfun::read_utf8(x)
  paste(x, collapse = '\n')
}

# find headings and build a table of contents as an unordered list
build_toc = function(html, n = 3) {
  if (n <= 0) return()
  if (n > 6) n = 6
  r = sprintf('<(h[1-%d])[^>]*>(.+?)</\\1>', n)
  items = unlist(regmatches(html, gregexpr(r, html, perl = TRUE)))
  if (length(items) == 0) return()
  x = gsub(r, '<toc>\\2</toc>', items)  # use a tag <toc> to protect heading text
  h = as.integer(gsub('^h', '', gsub(r, '\\1', items)))  # heading level
  s = strrep('  ', seq_len(n) - 1)  # indent
  x = paste0(s[h], '- ', x)  # create an unordered list
  x = commonmark::markdown_html(x)
  x = gsub('</?toc>', '', x)
  # add class 'numbered' to the first <ul> if any heading is numbered
  if (length(grep('<span class="section-number">', x)))
    x = sub('<ul>', '<ul class="numbered">', x)
  paste0('<div id="TOC">\n', x, '</div>')
}

sec_levels = c('subsubsection', 'subsection', 'section', 'chapter', 'part')
# raise section levels: redefine section to chapter or part, and so on
redefine_level = function(x, top) {
  n = switch(top, chapter = 1, part = 2, 0)
  if (n == 0) return(x)
  for (i in 3:1) {
    x = gsub(sprintf('(^|\n)\\\\%s', sec_levels[i]), sprintf('\\\\%s', sec_levels[i + n]), x)
  }
  x
}

# move image attributes like `![](){#id .class width="20%"}`, heading attributes
# `# foo {#id .class}`, and fenced Div's `::: {#id .class}` into HTML tags and
# LaTeX commands
move_attrs = function(x, format = 'html') {
  if (format == 'html') {
    # images
    x = convert_attrs(x, '(<img src="[^>]+ )/>\\{([^}]+)\\}', '\\2', function(r, z, z2) {
      z1 = sub(r, '\\1', z)
      paste0(z1, z2, ' />')
    })
    # headings
    x = convert_attrs(x, '(<h[1-6])(>.+?) \\{([^}]+)\\}(</h[1-6]>)', '\\3', function(r, z, z3) {
      z1 = sub(r, '\\1 ', z)
      z24 = sub(r, '\\2\\4', z)
      paste0(z1, z3, z24)
    })
    # fenced Div's
    x = convert_attrs(x, '<p>:::+ \\{(.+?)\\}</p>', '\\1', function(r, z, z1) {
      # add attributes to the div but remove the data-latex attribute
      z1 = str_trim(gsub('(^| )data-latex="[^"]*"( |$)', ' ', z1))
      sprintf('<div %s>', z1)
    })
    x = gsub('<p>:::+</p>', '</div>', x)
  } else if (format == 'latex') {
    # only support image width
    x = convert_attrs(x, '(\\\\includegraphics)(\\{[^}]+\\})\\\\\\{([^}]+)\\\\\\}', '\\3', function(r, z, z3) {
      r2 = '(^|.* )width="([^"]+)"( .*|$)'
      j = grepl(r2, z3)
      w = gsub(r2, '\\2', z3[j])
      w = gsub('\\\\', '\\', w, fixed = TRUE)
      k = grep('%$', w)
      w[k] = paste0(as.numeric(sub('%$', '', w[k])) / 100, '\\linewidth')
      z3[j] = paste0('[width=', w, ']')
      z3[!j] = ''
      z1 = sub(r, '\\1', z)
      z2 = sub(r, '\\2', z)
      paste0(z1, z3, z2)
    }, format)
    # discard attributes for headings
    r = sprintf('(\\\\(%s)\\{.+?) \\\\\\{([^}]+)\\\\\\}(\\})', paste(sec_levels, collapse = '|'))
    x = convert_attrs(x, r, '\\3', function(r, z, z3) {
      z = gsub(r, '\\1\\4', z)
      k = grepl('unnumbered', z3)
      z[k] = sub('{', '*{', z[k], fixed = TRUE)
      z
    }, format)
    # fenced Div's
    r = '\n\\\\begin\\{verbatim\\}\n(:::+)( \\{([^\n]+?)\\})? \\1\n\\\\end\\{verbatim\\}\n'
    x = convert_attrs(x, r, '\\3', function(r, z, z3) {
      r3 = '(^|.*? )class="([^" ]+)[" ].*? data-latex="([^"]*)".*$'
      z3 = ifelse(
        grepl(r3, z3), gsub(r3, '{\\2}\\3', z3), ifelse(z3 == '', '', '{@}')
      )
      z3 = latex_envir(gsub('\\\\', '\\', z3, fixed = TRUE))
      z3[z3 %in% c('\\begin{@}', '\\end{@}')] = ''
      i = grep('^\\\\begin', z3)
      z3[i] = paste0('\n', z3[i])
      i = grep('^\\\\end', z3)
      z3[i] = paste0(z3[i], '\n')
      z3
    }, format)
  } else {
    # TODO: remove attributes for other formats
  }
  x
}

convert_attrs = function(x, r, s, f, format = 'html') {
  r2 = '(?<=^| )[.#]([[:alnum:]-]+)(?= |$)'
  match_replace(x, r, function(y) {
    if (format == 'html') {
      z = gsub('[\U201c\U201d]', '"', y)
    } else {
      z = gsub('=``', '="', y, fixed = TRUE)
      z = gsub("''( |\\\\})", '"\\1', z)
      z = gsub('\\\\([#%])', '\\1', z)
    }
    z2 = sub(r, s, z)
    z2 = match_replace(z2, r2, perl = TRUE, function(a) {
      i = grep('^#', a)
      a[i] = gsub(r2, 'id="\\1"', a[i], perl = TRUE)
      i = grep('^[.]', a)
      if ((n <- length(i))) {
        # merge multiple classes into one class attribute
        a[i] = sub('^[.]', '', a[i])
        a[i] = c(rep('', n - 1), sprintf('class="%s"', paste(a[i], collapse = ' ')))
        a = c(a[i], a[-i])
      }
      a
    })
    f(r, z, str_trim(z2))
  })
}

str_trim = function(x) gsub('^\\s+|\\s+$', '', x)

# {A}, '', {B}, {C}, '', '' -> \begin{A}\end{A}\begin{B}\begin{C}\end{C}\end{B}
latex_envir = function(x, env = NULL) {
  n = length(x)
  if (n == 0) return()
  x1 = x[1]
  env2 = tail(env, 1)  # the most recent env is in the end
  env = if (x1 == '') head(env, -1) else c(env, sub('^(\\{[^}]+}).*$', '\\1', x1))
  c(if (x1 == '') paste0('\\end', env2) else paste0('\\begin', x1), latex_envir(x[-1], env))
}

# find and render footnotes for LaTeX output
render_footnotes = function(x) {
  f1 = f2 = NULL
  # [^1] is converted to {[}\^{}1{]}
  r = '(\n\n)(\\{\\[}\\\\\\^\\{}[0-9]+\\{\\]}): (.*?)\n(\n|$)'
  x = match_replace(x, r, function(z) {
    f1 <<- c(f1, sub(r, '\\2', z))
    f2 <<- c(f2, sub(r, '\\3', z))
    gsub(r, '\\1', z)
  })
  for (i in seq_along(f1)) {
    x = sub(f1[i], sprintf('\\footnote{%s}', f2[i]), x, fixed = TRUE)
  }
  x
}

# number sections in HTML output
number_sections = function(x) {
  m = gregexpr('</h[1-6]>', x)
  h = sub('</h([1-6])>', '\\1', unlist(regmatches(x, m)))
  if (length(h) == 0) return(x)  # no headings
  h = min(as.integer(h))  # highest level of headings
  r = '<h([1-6])([^>]*)>(?!<span class="section-number">)'
  n = rep(0, 6)  # counters for all levels of headings
  match_replace(x, r, perl = TRUE, function(z) {
    z1 = as.integer(sub(r, '\\1', z, perl = TRUE))
    z2 = sub(r, '\\2', z, perl = TRUE)
    for (i in seq_along(z)) {
      k = z1[i]
      if (k < 6) n[(k + 1):6] <<- 0
      if (grepl('unnumbered', z2[i])) next  # an unnumbered section
      n[k] <<- n[k] + 1
      # remove leading 0's
      s = paste(if (h > 1) n[-(1:(h - 1))] else n, collapse = '.')
      s = gsub('([.]0)+$', '', s)  # remove trailing 0's
      if (!grepl('[.]', s)) s = paste0(s, '.')  # '1. section' instead of '1 section'
      z[i] = paste0(z[i], sprintf('<span class="section-number">%s</span> ', s))
    }
    z
  })
}

#' @importFrom utils URLdecode
embed_resources = function(x, embed = 'local') {
  if (length(x) == 0) return(x)
  embed = c('https', 'local') %in% embed
  if (!any(embed)) return(x)

  r = '(<img[^>]* src="|<!--#[^>]*? style="background-image: url\\("?)([^"]+?)("|"?\\);)'
  x = match_replace(x, r, function(z) {
    z1 = sub(r, '\\1', z)
    z2 = sub(r, '\\2', z)
    z3 = sub(r, '\\3', z)
    # skip images already base64 encoded
    for (i in grep('^data:.+;base64,.+', z2, invert = TRUE)) {
      if (xfun::file_exists(f <- URLdecode(z2[i]))) {
        z2[i] = xfun::base64_uri(f)
      } else if (embed[1] && is_https(f)) {
        z2[i] = xfun::download_cache$get(f, 'base64')
      }
    }
    paste0(z1, z2, z3)
  })

  # CSS and JS
  r = paste0(
    '<link[^>]* rel="stylesheet" href="([^"]+)"[^>]*>|',
    '<script[^>]* src="([^"]+)"[^>]*>\\s*</script>'
  )
  x2 = NULL  # to be appended to x
  x = match_replace(x, r, function(z) {
    z1 = sub(r, '\\1', z)  # css
    z2 = sub(r, '\\2', z)  # js
    js = z2 != ''
    z3 = paste0(z1, z2)
    # skip resources already base64 encoded
    i1 = !grepl('^data:.+;base64,.+', z3)
    z3[i1] = gen_tags(z3[i1], ifelse(js[i1], 'js', 'css'), embed[1], embed[2])
    # for <script>s with defer/async, move them to the end of </body>
    i2 = grepl(' (defer|async)(>| )', z) & js
    x2 <<- c(x2, z3[i2])
    z3[i2] = ''
    z3
  })
  # move defer/async js to the end of <body>
  if (length(x2)) {
    x = if (length(grep('</body>', x)) != 1) {
      one_string(I(c(x, x2)))
    } else {
      match_replace(x, '</body>', fixed = TRUE, function(z) {
        one_string(I(c(x2, z)))
      })
    }
  }
  x
}

normalize_options = function(x, format = 'html') {
  g = get_option(sprintf('markdown.%s.options', format))
  x = option2list(x)
  n = names(x)
  n[n == 'hard_wrap'] = 'hardbreaks'
  n[n == 'tables'] = 'table'
  n[n == 'base64_images'] = 'embed_resources'
  names(x) = n
  # default options
  d = option2list(markdown_options())
  g = option2list(g)
  d[names(g)] = g  # merge global options() into default options
  d[n] = x  # then merge user-provided options
  if (!is.numeric(d[['toc_depth']])) d$toc_depth = 3L
  if (!is.character(d[['top_level']])) d$top_level = 'section'
  # mathjax = true -> js_math = 'mathjax'
  if (isTRUE(x[['mathjax']])) x$js_math = 'mathjax'
  x = x[setdiff(names(x), 'mathjax')]
  # highlight_code -> js_highlight
  if (!is.null(h <- x[['highlight_code']])) {
    h$package = 'highlight'
    x$js_highlight = h
    x$highlight_code = NULL
  }
  x = normalize_embed(x)
  # TODO: fully enable footnotes https://github.com/github/cmark-gfm/issues/314
  if (format == 'html' && !is.logical(d[['footnotes']])) d$footnotes = TRUE
  d
}

normalize_embed = function(x) {
  v = x[['embed_resources']]
  if (is.logical(v)) {
    v = if (v) 'local'
  } else {
    if (length(v) == 1 && v == 'all') v = c('local', 'https')
  }
  x[['embed_resources']] = v
  x
}

#' @import stats
namedBool = function(x, val = TRUE) as.list(setNames(rep(val, length(x)), x))

# normalize metadata variable names: change _ to -
normalize_meta = function(x) {
  # make sure some variables are available in metadata
  x = merge_list(list(classoption = '', documentclass = 'article'), x)
  names(x) = gsub('_', '-', names(x))
  x
}

# turn '+a-b c' to list(a = TRUE, b = FALSE, c = TRUE)
option2list = function(x) {
  if (!is.character(x)) return(as.list(x))
  x = unlist(strsplit(x, '\\b(?=[+-])', perl = TRUE))
  x = unlist(strsplit(x, '\\s+'))
  x = setdiff(x, '')
  i = grepl('^-', x)
  c(namedBool(sub('^[-]', '', x[i]), FALSE), namedBool(sub('^[+]', '', x[!i])))
}

pkg_file = function(...) {
  res = system.file(..., package = 'markdown', mustWork = TRUE)
  # TODO: remove this hack after the next release of polmineR
  if (!xfun::check_old_package('polmineR', '0.8.7')) return(res)
  x = basename(file.path(...))
  if (x == 'highlight.html') x = 'r_highlight.html'
  if (x == 'default.css') x = 'markdown.css'
  if (!x %in% c('markdown.css', 'markdown.html', 'r_highlight.html')) return(res)
  download_old(x)
}

# cache downloaded file
download_old = local({
  db = list()
  function(file) {
    if (!is.null(db[[file]])) return(db[[file]])
    u = jsdelivr(file, 'gh/rstudio/markdown@1.3/inst/resources/')
    f = tempfile()
    xfun::download_file(u, f, quiet = TRUE)
    db[[file]] <<- f
    f
  }
})

jsdelivr = function(file, dir = 'gh/rstudio/markdown/inst/resources/') {
  sprintf('https://cdn.jsdelivr.net/%s%s', dir, file)
}

# resolve CSS/JS shorthand filenames to actual paths (e.g., 'default' to 'default.css')
resolve_files = function(x, ext = 'css') {
  if (length(x) == 0) return(x)
  # @foo -> jsdelivr.net/gh/rstudio/markdown
  i0 = grepl('^@', x)
  x[i0] = sub('^@', '', x[i0])
  i = i0 & !grepl('/', x)
  x[i] = jsdelivr(xfun::with_ext(x[i], ext))
  # @foo/bar -> jsdelivr.net/foo/bar
  i = i0 & !grepl(',', x)
  x[i] = jsdelivr(x[i], '')
  # @foo/bar,baz -> jsdelivr.net/combine/foo/bar,foo/baz
  i = i0 & grepl(',', x)
  if (any(i)) x[i] = sapply(strsplit(x[i], ','), function(z) {
    d = dirname(z[1])
    for (j in 2:length(z)) {
      if (grepl('/', z[j])) {
        d = dirname(z[j])
      } else {
        z[j] = paste(d, z[j], sep = '/')
      }
    }
    paste0('combine/', paste(z, collapse = ','))
  })
  x[i] = jsdelivr(x[i], '')
  # built-in resources in this package
  i = dirname(x) == '.' & xfun::file_ext(x) == '' & !xfun::file_exists(x)
  x[i & (x == 'slides')] = 'snap'  # alias slides.css -> snap.css
  files = list.files(pkg_file('resources'), sprintf('[.]%s$', ext), full.names = TRUE)
  b = xfun::sans_ext(basename(files))
  if (any(!x[i] %in% b)) stop(
    "Invalid '", ext, "' option: ", paste0("'", setdiff(x[i], b), "'", collapse = ', '),
    " (possible values are: ", paste0("'", b, "'", collapse = ','), ")"
  )
  x[i] = files[match(x[i], b)]
  x = c(x[i], x[!i])
  if (ext %in% c('css', 'js')) gen_tags(x, ext) else xfun::read_all(x)
}

# generate tags for css/js depending on whether they need to be embedded or offline
gen_tag = function(x, ext = '', embed_https = FALSE, embed_local = FALSE) {
  if (ext == 'css') {
    t1 = '<link rel="stylesheet" href="%s">'
    t2 = c('<style type="text/css">', '</style>')
  } else if (ext == 'js') {
    t1 = '<script src="%s" defer></script>'
    t2 = c('<script>', '</script>')
  } else stop("The file extension '", ext, "' is not supported.")
  is_web = is_https(x)
  is_rel = !is_web && xfun::is_rel_path(x)
  if (is_web && embed_https && xfun::url_filename(x) == 'MathJax.js') {
    warning('MathJax.js cannot be embedded. Please use MathJax v3 instead.')
    embed_https = FALSE
  }
  if ((is_rel && !embed_local) || (is_web && !embed_https)) {
    # linking for 1) local rel paths that don't need to be embedded, or 2) web
    # resources that don't need to be accessed offline
    sprintf(t1, x)
  } else {
    # embedding for other cases
    one_string(I(c(t2[1], resolve_external(x, is_web, ext), t2[2])))
  }
}

is_https = function(x) grepl('^https://', x)

# a vectorized version
gen_tags = function(...) mapply(gen_tag, ...)

# read CSS/JS and embed external fonts/background images, etc.
resolve_external = function(x, web = TRUE, ext = '') {
  # download and cache web resources
  txt = if (web) xfun::download_cache$get(x, 'text', handler = function(code) {
    # remove jsdelivr comments
    if (grepl('^https://cdn[.]jsdelivr[.]net/', x)) {
      code = gsub(
        '^/[*][*]\n( [*][^\n]*\n)+ [*]/\n|\n/[*/]# sourceMappingURL=.+[.]map( [*]/)?$',
        '', one_string(I(code))
      )
      code = base64_url(x, code, ext)
    }
    code
  }) else {
    base64_url(x, xfun::read_utf8(x), ext)
  }
}

# find url("path") in JS/CSS and base64 encode the resources
base64_url = function(url, code, ext) {
  d = dirname(url)
  # embed fonts in mathjax's js
  if (grepl('^https://cdn[.]jsdelivr[.]net/npm/mathjax.+[.]js$', url)) {
    r = '.*?fontURL:[^"]+\\("([^"]+)".*'  # output/chtml/fonts/woff-v2
    p = xfun::grep_sub(r, '\\1', code)
    if (length(p) == 1) code = match_replace(
      code, '(?<=src:\'url\\(")(%%URL%%/[^"]+)(?="\\))', function(u) {
        u = sub('%%URL%%', paste(d, p, sep = '/'), u, fixed = TRUE)
        unlist(lapply(u, function(x) xfun::download_cache$get(x, 'base64')))
      }, perl = TRUE
    ) else warning(
      'Unable to determine the font path in MathJax. Please report an issue to ',
      'https://github.com/rstudio/markdown/issues and mention the URL ', url, '.'
    )
  }
  # find `attr: url(resource)` and embed url resources in CSS
  if (ext == 'css') {
    r = '(: ?url\\("?)([^)]+)("?\\))'
    code = match_replace(code, r, function(z) {
      z1 = gsub(r, '\\1', z)
      z2 = gsub(r, '\\2', z)
      z3 = gsub(r, '\\3', z)
      i = !is_https(z2)
      z2[i] = paste(d, z2[i], sep = '/')
      z2 = unlist(lapply(z2, function(x) {
        if (is_https(x)) xfun::download_cache$get(x, 'base64') else xfun::base64_uri(x)
      }))
      paste0(z1, z2, z3)
    }, perl = TRUE)
  }
  code
}

# compact HTML code
clean_html = function(x) {
  x = gsub('\n+(\n<[a-z1-6]+[^>]*>|\n</(body|div|head|html)>)', '\\1', x)
  # can also merge <style>/<script> tags (<style type="text/css">).+?</style>\\s*\\1
  x
}

# TODO: remove this after new release of https://github.com/rstudio/leaflet
.b64EncodeFile = function(...) xfun::base64_uri(...)

# TODO: remove this after https://github.com/PolMine/polmineR/issues/235 is fixed
.onLoad = function(lib, pkg) {
  if (is.null(getOption('markdown.HTML.stylesheet')) && 'polmineR' %in% loadedNamespaces()) {
    if (xfun::check_old_package('polmineR', '0.8.7')) {
      options(markdown.HTML.stylesheet = pkg_file('resources', 'default.css'))
    } else if (packageVersion('polmineR') == '0.8.7') {
      warning("Sorry, but the 'markdown' does not work with 'polmineR' 0.8.7: https://github.com/PolMine/polmineR/issues/235")
    }
  }
}

# TODO: remove these hacks eventually
tweak_html = function(x, text) {
  if (xfun::check_old_package('plumbertableau', '0.1.0') ||
      xfun::check_old_package('polmineR', '0.8.7')) {
    # remove extra blockquote
    x = gsub('</blockquote>\n<blockquote>', '', x)
    # double \n
    x = gsub('>\n<(p|h3|blockquote)>', '>\n\n<\\1>', x)
    # tweak language class names
    x = gsub('(<code class=")language-([^"]+)(">)', '\\1\\2\\3', x)
    # preserve trailing spaces
    if (length(sp <- xfun::grep_sub('.*?( +)\n*?$', '\\1', tail(one_string(I(text)), 1))))
      x = gsub('></p>(\n+)?$', paste0('>', sp, '</p>\\1'), x)
  }
  x
}

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
  regmatches(x, m) = lapply(regmatches(x, m), replace)
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

# find the first header in html
first_header = function(html) {
  m = regexpr(r <- '<(h[1-6])[^>]*?>(.+?)</\\1>', html, perl = TRUE)
  gsub(r, '\\2', regmatches(html, m))
}

.mathJax = local({
  js = NULL
  function(embed = FALSE, force = FALSE) {
    url = 'https://mathjax.rstudio.com/latest/MathJax.js?config=TeX-MML-AM_CHTML'
    # insert or link to MathJax script?
    c('<!-- MathJax scripts -->', if (embed) {
      # already in cache?
      if (force || is.null(js)) js <<- xfun::read_utf8(url)
      c('<script>', js)
    } else {
      sprintf('<script src="%s" async>', url)
    }, '</script>')
  }
})

.requiresMathJax = function(x) {
  regs = c('\\\\\\(.+?\\\\\\)', '[$]{2}.+?[$]{2}', '\\\\\\[.+?\\\\\\]')
  for (i in regs) if (any(grepl(i, x, perl = TRUE))) return(TRUE)
  FALSE
}

highlight_js = function(opts, html) {
  if (isTRUE(opts)) opts = list()
  if (!is.list(opts) || !any(grepl('<code class="(language-)?[^"]+"', html)))
    return()
  # TODO: we could automatically detect <code> languages in html and load the
  # necessary highlight.js language component (e.g., languages/latex.min.js)
  opts = merge_list(
    list(version = '11.6.0', style = 'github', languages = NULL), opts
  )
  tpl = one_string(pkg_file('resources', 'highlight.html'))
  js = paste0(
    sprintf('gh/highlightjs/cdn-release@%s/build/', opts$version),
    c('highlight', sprintf('languages/%s', opts$languages)),
    '.min.js', collapse = ','
  )
  tpl = sub_var(tpl, '$style$', opts$style)
  tpl = sub_var(tpl, '$js$', js)
  tpl
}

# get an option using a case-insensitive name
get_option = function(name, default = NULL) {
  x = options()
  i = match(tolower(name), tolower(names(x)))
  i = i[!is.na(i)]
  if (length(i) == 0) default else x[[i[1]]]
}

drop_null = function(x) {
  for (i in names(x)) if (is.null(x[[i]])) x[[i]] = NULL
  x
}

# if a string is a file path, read the file; then concatenate elements by \n
one_string = function(x) {
  if (!is.character(x)) return('')
  if (is_file(x, TRUE)) x = xfun::read_utf8(x)
  paste(x, collapse = '\n')
}

# find headers and build a table of contents as an unordered list
build_toc = function(html, n = 3) {
  if (n <= 0) return()
  if (n > 6) n = 6
  r = sprintf('<(h[1-%d])>([^<]+)</\\1>', n)
  items = unlist(regmatches(html, gregexpr(r, html, perl = TRUE)))
  if (length(items) == 0) return()
  x = gsub(r, '<toc>\\2</toc>', items)  # use a tag <toc> to protect header text
  h = as.integer(gsub('^h', '', gsub(r, '\\1', items)))  # header level
  s = sapply(seq_len(n), function(i) paste(rep('  ', i), collapse = ''))  # indent
  x = paste0(s[h], '- ', x)  # create an unordered list
  x = commonmark::markdown_html(x)
  x = gsub('</?toc>', '', x)
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

#' @importFrom utils URLdecode
.b64EncodeImages = function(x) {
  if (length(x) == 0) return(x)
  reg = '<img\\s+src\\s*=\\s*"([^"]+)"'
  m = gregexpr(reg, x, ignore.case = TRUE)
  regmatches(x, m) = lapply(regmatches(x, m), function(z) {
    src = sub(reg, '\\1', z)
    # skip images already base64 encoded
    for (i in grep('^data:.+;base64,.+', src, invert = TRUE)) {
      # TODO: perhaps can remove the hard dependency on mime
      if (file.exists(f <- URLdecode(src[i]))) z[i] = sub(
        src[i], xfun::base64_uri(f, mime::guess_type(f)), z[i], fixed = TRUE
      )
    }
    z
  })
  x
}

normalize_options = function(x, format = 'html') {
  g = get_option(sprintf('markdown.%s.options', format))
  x = option2list(x)
  n = names(x)
  n[n == 'hard_wrap'] = 'hardbreaks'
  n[n == 'tables'] = 'table'
  names(x) = n
  # default options
  d = option2list(markdown_options())
  g = option2list(g)
  d[names(g)] = g  # merge global options() into default options
  d[n] = x  # then merge user-provided options
  if (!is.numeric(d[['toc_depth']])) d$toc_depth = 3L
  if (!is.character(d[['top_level']])) d$top_level = 'section'
  d
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
  if (!x %in% c('markdown.css', 'markdown.html', 'r_highlight.html')) return(res)
  download_old(x)
}

# cache downloaded file
download_old = local({
  db = list()
  function(file) {
    if (!is.null(db[[file]])) return(db[[file]])
    u = sprintf('https://cdn.jsdelivr.net/gh/rstudio/markdown@v1.3/inst/resources/%s', file)
    f = tempfile()
    xfun::download_file(u, f, quiet = TRUE)
    db[[file]] <<- f
    f
  }
})

# partition the YAML metadata from the document body and parse it
split_yaml = function(x) {
  i = grep('^---\\s*$', x)
  n = length(x)
  res = if (n < 2 || length(i) < 2 || (i[1] > 1 && !all(is_blank(x[seq(i[1] - 1)])))) {
    list(yaml = list(), body = x)
  } else list(
    yaml = x[i[1]:i[2]], body = c(rep('', i[2]), tail(x, n - i[2]))
  )
  if ((n <- length(res$yaml)) >= 3) {
    res$yaml = yaml_load(res$yaml[-c(1, n)])
  }
  res
}

yaml_load = function(x, use_yaml = xfun::loadable('yaml')) {
  if (use_yaml) {
    res = xfun::try_silent(yaml::yaml.load(x, eval.expr = TRUE))
    if (!inherits(res, 'try-error')) return(res)
    warning(paste(c(x, '\nThe above YAML metadata may be invalid:\n', res), collapse = '\n'))
  }
  # the below simple parser is quite limited
  res = list()
  r = '^( *)([^ ]+?):($|\\s+.*)'
  x = xfun::split_lines(x)
  x = x[grep(r, x)]
  x = x[grep('^\\s*#', x, invert = TRUE)]  # comments
  if (length(x) == 0) return(res)
  lvl = gsub(r, '\\1', x)  # indentation level
  key = gsub(r, '\\2', x)
  val = gsub('^\\s*|\\s*$', '', gsub(r, '\\3', x))
  keys = NULL
  for (i in seq_along(x)) {
    keys = c(head(keys, nchar(lvl[i])/2), key[i])
    res[[keys]] = if (is_blank(val[i])) list() else yaml_value(val[i])
  }
  res
}

is_blank = function(x) grepl('^\\s*$', x)

# only support scalar logical, numeric, and character values
yaml_value = function(x) {
  v = tolower(x)
  if (v == 'null') return()
  if (grepl('^true|false$', v)) return(as.logical(x))
  if (grepl('^[0-9.e+-]', v)) {
    v = suppressWarnings(as.numeric(v))
    if (!is.na(v)) return(v)
  }
  gsub('^["\']|["\']$', '', x)  # remove optional quotes for strings
}

# TODO: remove this function when revdeps have been fixed
.b64EncodeFile = function(...) xfun::base64_uri(...)

# TODO: remove this after https://github.com/PolMine/polmineR/pull/232 is fixed
.onLoad = function(lib, pkg) {
  if (is.null(getOption('markdown.HTML.stylesheet')) && 'polmineR' %in% loadedNamespaces()) {
    options(markdown.HTML.stylesheet = pkg_file('resources', 'markdown.css'))
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
    if (length(sp <- xfun::grep_sub('.*?( +)\n*?$', '\\1', tail(paste(text, collapse = '\n'), 1))))
      x = gsub('></p>(\n+)?$', paste0('>', sp, '</p>\\1'), x)
  }
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

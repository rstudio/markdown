#
# renderMarkdown.R
#
# Copyright (C) 2009-1012 by RStudio, Inc.
#
# This program is licensed to you under the terms of version 3 of the
# GNU General Public License. This program is distributed WITHOUT ANY
# EXPRESS OR IMPLIED WARRANTY, INCLUDING THOSE OF NON-INFRINGEMENT,
# MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. Please refer to the
# GPL (http://www.gnu.org/licenses/gpl-3.0.txt) for more details.
#
#


registeredRenderers <- function() .Call(rmd_registered_renderers)

rendererExists <- function(name)
{
   name[1] %in% registeredRenderers()
}

rendererOutputType <- function(name)
{
   rnds <- registeredRenderers()
   if (!name[1] %in% rnds)
   {
      warning("Renderer is not registered!")
      return('')
   }
   names(which(rnds==name[1]))
}

renderMarkdown <-
function(file, output, text, renderer='HTML', renderer.options=NULL,
         extensions=getOption('markdown.extensions'))
{

   if (!rendererExists(renderer))
   {
      stop("Renderer '",renderer,"' is not registered!")
   }

   # Input from either a file or character vector
   if (!missing(file) && is.character(file) && file.exists(file))
   {
      text <- NULL
   }
   else if (!missing(text) && !is.null(text) && is.character(text))
   {
      file <- NULL
      if (length(text) > 1)
         text <- paste(text,collapse='')
   }
   else 
   {
      stop("Need input from either a file or a text string!")
   }

   # Output is either returned or written to a file
   if (missing(output))
      output <- NULL
   else if (!is.null(output) && !is.character(output))
      stop("output variable must be a file name!");


   # Options
   if (is.null(renderer.options))
      renderer.options <- getOption(paste('markdown',renderer,'options',
                                          sep='.'))

   # HTML options must be a character vector. 
   if (renderer=="HTML")
   {
      if (!is.null(renderer.options) && !is.character(renderer.options))
         stop("HTML options must be a character vector")
   }

   ret <- .Call(rmd_render_markdown,file,output,text,renderer,
                   renderer.options, extensions)

   if (is.raw(ret) && rendererOutputType(renderer)=='character')
      ret <- rawToChar(ret)

   invisible(ret)
}

.MIMEMAP <- new.env()

## most common web types
.MIMEMAP$htm <- "text/html"
.MIMEMAP$html <- "text/html"
.MIMEMAP$css <- "text/css"
.MIMEMAP$gif <- "image/gif"
.MIMEMAP$jpg <- "image/jpeg"
.MIMEMAP$jpeg <- "image/jpeg"
.MIMEMAP$jpe <- "image/jpeg"
.MIMEMAP$png <- "image/png"
.MIMEMAP$js <- "application/x-javascript"
.MIMEMAP$pdf <- "application/pdf"
.MIMEMAP$svg <- "image/svg+xml"
.MIMEMAP$swf <- "application/x-shockwave-flash"

## markdown types
.MIMEMAP$md <- "text/x-markdown"
.MIMEMAP$mdtxt <- "text/x-markdown"
.MIMEMAP$markdown <- "text/x-markdown"

## other types we are likely to serve
.MIMEMAP$xml <- "text/xml"
.MIMEMAP$csv <- "text/csv"
.MIMEMAP$ico <- "image/x-icon"
.MIMEMAP$zip <- "application/zip"
.MIMEMAP$bz <- "application/x-bzip"
.MIMEMAP$bz2 <- "application/x-bzip2"
.MIMEMAP$gz <- "application/x-gzip"
.MIMEMAP$tar <- "application/x-tar"

## yet more types...

.MIMEMAP$shtml <- "text/html"
.MIMEMAP$tsv <- "text/tab-separated-values"
.MIMEMAP$tab <- "text/tab-separated-values"
.MIMEMAP$dcf <- "text/debian-control-file"
.MIMEMAP$txt <- "text/plain"
.MIMEMAP$mml <- "text/mathml"

.MIMEMAP$tif <- "image/tiff"
.MIMEMAP$tiff <- "image/tiff"
.MIMEMAP$bmp <- "image/bmp" 
.MIMEMAP$ps <- "application/postscript"
.MIMEMAP$eps <- "application/postscript"
.MIMEMAP$dvi <-   "application/x-dvi"

.MIMEMAP$atom <- "application/atom+xml"
.MIMEMAP$rss <- "application/rss+xml"

.MIMEMAP$doc <- "application/msword"
.MIMEMAP$docx <- "application/vnd.openxmlformats-officedocument.wordprocessingml.document"
.MIMEMAP$odt <- "application/vnd.oasis.opendocument.text"
.MIMEMAP$rtf <- "application/rtf"
.MIMEMAP$xls <- "application/vnd.ms-excel"
.MIMEMAP$xlsx <- "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
.MIMEMAP$ods <- "application/x-vnd.oasis.opendocument.spreadsheet"
.MIMEMAP$ppt <- "application/vnd.ms-powerpoint"
.MIMEMAP$pps <- "application/vnd.ms-powerpoint"
.MIMEMAP$pptx <- "application/vnd.openxmlformats-officedocument.presentationml.presentation"

.MIMEMAP$sit <- "application/x-stuffit"
.MIMEMAP$sxw <- "application/vnd.sun.xml.writer"

.MIMEMAP$iso <- "application/octet-stream"
.MIMEMAP$dmg <- "application/octet-stream"
.MIMEMAP$exe <- "application/octet-stream"
.MIMEMAP$dll <- "application/octet-stream"
.MIMEMAP$deb <- "application/octet-stream"
.MIMEMAP$otf <- "application/octet-stream"
.MIMEMAP$xpi <- "application/x-xpinstall"

.MIMEMAP$mp2 <- "audio/mpeg"
.MIMEMAP$mp3 <- "audio/mpeg"

.MIMEMAP$mpg <- "video/mpeg"
.MIMEMAP$mpeg <- "video/mpeg"
.MIMEMAP$flv <- "video/x-flv"

.mimeType <- function(f){
   f <- f[1]
   fileExt <- function (x) 
   {
      pos <- regexpr("\\.([[:alnum:]]+)$", x)
      ifelse(pos > -1L, tolower(substring(x, pos + 1L)), "")
   }
   ext <- fileExt(f)
   ifelse(nchar(ext) > 1L && exists(ext,.MIMEMAP),.MIMEMAP[[ext]], "")
}

.b64EncodeFile <- function(inFile)
{
   fileSize <- file.info(inFile)$size

   if (fileSize > 0){
      paste( "data:", .mimeType(inFile),";base64,",
         .Call(rmd_b64encode_data,readBin(inFile,'raw',n=fileSize)),
         sep='')
   } else {
      warning(inFile,'is empty!')
      inFile
   }
}


.b64EncodeImages <- function(html)
{
   reg <- "<\\s*[Ii][Mm][Gg]\\s+[Ss][Rr][Cc]\\s*=\\s*[\"']([^\"']+)[\"']"
   m <- gregexpr(reg,html,perl=TRUE)
   if (m[[1]][1] != -1)
   {
      .b64EncodeImgSrc <- function(imgSrc)
      {
         inFile <- sub(reg,"\\1",imgSrc)
         if (length(inFile) && file.exists(inFile))
            imgSrc <- sub(inFile,.b64EncodeFile(inFile),imgSrc,fixed=TRUE)

         imgSrc
      }
      regmatches(html,m) <- list(unlist(lapply(regmatches(html,m)[[1]],.b64EncodeImgSrc)))
   }

   html
}


.requiresMathJax <- function(html)
{
   regs <- c(
              "\\\\\\(([\\s\\S]+?)\\\\\\)",
              "\\\\\\[([\\s\\S]+?)\\\\\\]"
            )
   for(i in regs){
      if (regexpr(i,html,perl=TRUE) > -1)
         return(TRUE)
   }
   FALSE
}

.requiresHighlighting <- function(html)
{
   reg <- "<pre><code class=\"r\""
   if (regexpr(reg,html,perl=TRUE) > -1)
      TRUE
   else
      FALSE
}

markdownToHTML <- function(file, output, text, 
                           options=getOption('markdown.HTML.options'),
                           extensions=getOption('markdown.extensions'),
                           title='', 
                           stylesheet=getOption('markdown.HTML.stylesheet'),
                           fragment.only=FALSE)
{
   if (fragment.only==TRUE)
      options <- c(options,'fragment_only')

   if (!missing(output))
   {
      outputFile <- output
      output <- NULL
   } 
   else
      outputFile <- NULL

   ret <- renderMarkdown(file,output,text,renderer="HTML",
                  renderer.options=options,extensions=extensions)

   if ('base64_images' %in% options){
      if (!missing(file) && is.character(file) && file.exists(file)){
         oldwd <- setwd(dirname(file))
         on.exit(setwd(oldwd))
      }
      ret <- .b64EncodeImages(ret);
   }

   if (!'fragment_only' %in% options)
   {
      html <- paste(readLines(
              system.file('resources/markdown.html',package='markdown')),collapse='\n')
      html <- sub('#!html_output#',ret,html,fixed=TRUE)

      if (is.character(stylesheet)){

         # what to do if user misspelled file name?
         if (file.exists(stylesheet))
            stylesheet <- paste(readLines(stylesheet),collapse='\n')

         # presume the character vector contains CSS.
         html <- sub('#!markdown_css#',stylesheet,html,fixed=TRUE)

      } else {
        warning("stylesheet must either be valid CSS or a file containint CSS!")
      }

      if (!is.character(title) || title == '')
      {
         # Guess title
         m <- regexpr("<[Hh][1-6].*?>(.*)</[Hh][1-6].*?>",html,perl=TRUE)
         if (m > -1){
            title <- regmatches(html,m)
            title <- sub("<[Hh][1-6].*?>","",title)
            title <- sub("</[Hh][1-6].*?>","",title)
         } else {
            title <- ''
         }
      }

      # Need to scrub title more, e.g. strip html, etc.
      html <- sub("#!title#",title,html,perl=TRUE)

      if ('highlight_code' %in% options && .requiresHighlighting(html)){
         highlight <- paste(readLines(system.file('resources/r_highlight.html',package='markdown')),collapse='\n')
      } else {
         highlight <- ''
      }
      html <- sub("#!r_highlight#",highlight,html,fixed=TRUE)

      if ('mathjax' %in% options && .requiresMathJax(html)){
         mathjax <- paste(readLines(system.file('resources/mathjax.html',package='markdown')),collapse='\n')
      } else {
         mathjax <- ''
      }
      html <- sub("#!mathjax#",mathjax,html,fixed=TRUE)

      ret <- html
   }

   if (is.character(outputFile))
   {
      cat(ret,file=outputFile)
      ret <- NULL
   }

   invisible(ret)
}

smartypants <- function(file,output,text)
{
   # Input from either a file or character vector
   if (!missing(file) && is.character(file) && file.exists(file))
   {
      text <- NULL
   }
   else if (!missing(text) && !is.null(text) && is.character(text))
   {
      file <- NULL
      if (length(text) > 1)
         text <- paste(text,collapse='')
   }
   else 
   {
      stop("Need input from either a file or a text string")
   }

   # Output is either returned or written to a file
   if (missing(output))
      output <- NULL
   else if (!is.character(output))
      stop("output variable must be a file name!");

   ret <- .Call(rmd_render_smartypants,file,output,text)
   if (is.raw(ret))
      ret <- rawToChar(ret)

   invisible(ret)
}

# Markdown extensions.
#
# To turn on all extensions:
#
# options(markdown.extensions=markdownExtensions())
# 
# To turn off all extensions:
# 
# options(markdown.extensions=c())
#
markdownExtensions <- function()
{
   c('no_intra_emphasis','tables','fenced_code','autolink','strikethrough',
     'lax_spacing','space_headers','superscript','latex_math')
}

# HTML renderer options.
#
# To turn on all options:
#
# options(markdown.HTML.options=markdownHTMLOptions())
#
# To turn on default options:
#
# options(markdown.HTML.options=markdownHTMLOptions(defaults=TRUE))
# 
# To turn off all options:
# 
# options(markdown.HTML.options=c())
#
markdownHTMLOptions <- function(defaults=FALSE)
{
   allOptions <- c('skip_html', 'skip_style', 'skip_images', 'skip_links', 
                   'safelink', 'toc', 'escape', 'fragment_only', 'hard_wrap',
                   'use_xhtml', 'smartypants','base64_images', 'mathjax',
                   'highlight_code')
   if (!defaults)
      allOptions
   else
      allOptions[seq(10,14)]
}

.onLoad <- function(libname,pkgname)
{

   if (is.null(getOption('markdown.extensions')))
      options(markdown.extensions=markdownExtensions())

   if (is.null(getOption('markdown.HTML.options')))
      options(markdown.HTML.options=markdownHTMLOptions(defaults=TRUE))

   if (is.null(getOption('markdown.HTML.stylesheet'))){
      sheet <- system.file('resources/markdown.css',package='markdown')
      options(markdown.HTML.stylesheet=sheet)
   }
}

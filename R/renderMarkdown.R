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

.filterMath <- function(file,text)
{
   gg <- .GUIDgenerator()

   if (!is.null(file))
      text <- paste(readLines(file),collapse='\n')

   if (nchar(text)==0)
      stop("Input is empty!")

   mFilter <- list(text=text, mathEnv=new.env(hash=TRUE))

   regexprs <- list(
                  list(
                     pat="\\${2}latex(\\s[\\s\\S]+?)\\${2}",
                     repl="\\\\[\\1\\\\]"
                  ),
                  list(
                     pat="\\$latex(\\s[\\s\\S]+?)\\$",
                     repl="\\\\(\\1\\\\)"
                  )
               )

   for (r in regexprs)
   {
      matches <- gregexpr(r$pat,mFilter$text,perl=TRUE)
      if (matches[[1]][1] != -1)
      {
         guids <- unlist(lapply(seq_along(matches[[1]]),function(i)gg$GUID()))
         guidStr <- regmatches(mFilter$text,matches)[[1]]
         lapply(
            seq_along(guids),
            function(i)
            {
               assign(
                  guids[i],
                  sub(r$pat,r$repl,guidStr[i],perl=TRUE),
                  mFilter$mathEnv
               )
            }
         )
         tmpText <- mFilter$text
         regmatches(tmpText,matches) <- list(guids)
         mFilter$text <- tmpText
      }
   }

   mFilter
}

.unfilterMath <- function(mFilter)
{

   text <- mFilter$text

   for (s in ls(envir=mFilter$mathEnv))
   {
      text <- sub(s,get(s,mFilter$mathEnv),text,fixed=TRUE)
   }

   if (!is.null(mFilter$outputFile))
   {
      cat(text,file=mFilter$outputFile)
      NULL
   }
   else
      text
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

   # Extensions must be a character vector
   if (!is.null(extensions) && !is.character(extensions))
      stop("extensions must be a character vector")

   if ('ignore_math' %in% extensions)
   {
      if (rendererOutputType(renderer) != 'character')
      {
         warning("Ignoring extension 'ignore_math'. Only works for renderers that output text.")
      } 
      else
      {
         mFilter <- .filterMath(file,text)
         text <- mFilter$text
         file <- NULL
         if (!is.null(output))
         {
            mFilter$outputFile <- output
            output <- NULL
         }

      }
   }

   ret <- .Call(rmd_render_markdown,file,output,text,renderer,
                   renderer.options, extensions)

   if ('ignore_math' %in% extensions && 
       rendererOutputType(renderer)=='character')
   {
      mFilter$text <- rawToChar(ret);
      ret <- .unfilterMath(mFilter)
   }

   if (is.raw(ret) && rendererOutputType(renderer)=='character')
      ret <- rawToChar(ret)

   invisible(ret)
}

.mimeType <- function(file){
   if      (grepl('.png$',file,perl=TRUE,ignore.case=TRUE))
      'image/png'
   else if (grepl('.gif$',file,perl=TRUE,ignore.case=TRUE))
      'image/gif'
   else if (grepl('(.jpg$|.jpeg$)',file,perl=TRUE,ignore.case=TRUE))
      'image/jpeg'
   else if (grepl('.tiff?$',file,perl=TRUE,ignore.case=TRUE))
      'image/tiff'
   else
      ''
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
      regmatches(html,m) <- lapply(regmatches(html,m)[[1]],.b64EncodeImgSrc)
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
         stylesheet=system.file('resources/markdown.css',package='markdown'),
                           fragment.only=FALSE)
{
   if (fragment.only==TRUE)
      options <- c(options,'fragment_only')

   if (!'fragment_only' %in% options)
   {
      if (!missing(output))
      {
         outputFile <- output
         output <- NULL
      } 
      else
         outputFile <- NULL
   }

   ret <- renderMarkdown(file,output,text,renderer="HTML",
                  renderer.options=options,extensions=extensions)

   if (!'fragment_only' %in% options)
   {
      html <- paste(readLines(
              system.file('resources/markdown.html',package='markdown')),collapse='\n')
      html <- sub('#!html_output#',ret,html,fixed=TRUE)

      if (is.character(stylesheet)){

         # TODO - what to do if user misspelled file name?
         if (file.exists(stylesheet))
            stylesheet <- paste(readLines(stylesheet),collapse='\n')

         html <- sub('#!markdown_css#',stylesheet,html,fixed=TRUE)

      } else {
        warning("stylsheet must either be valid CSS or a file containint CSS!")
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

      if ('highlight_code' %in% options || .requiresHighlighting(html)){
         highlight <- paste(readLines(system.file('resources/r_highlight.html',package='markdown')),collapse='\n')
      } else {
         highlight <- ''
      }
      html <- sub("#!r_highlight#",highlight,html,fixed=TRUE)

      if ('mathjax' %in% options || .requiresMathJax(html)){
         mathjax <- paste(readLines(system.file('resources/mathjax.html',package='markdown')),collapse='\n')
      } else {
         mathjax <- ''
      }
      html <- sub("#!mathjax#",mathjax,html,fixed=TRUE)

      if ('base64_images' %in% options){
         if (!missing(file) && is.character(file) && file.exists(file)){
            oldwd <- setwd(dirname(file))
            on.exit(setwd(oldwd))
         }
         html <- .b64EncodeImages(html);
      }

      if (is.character(outputFile))
         cat(html,file=outputFile)
      else
         ret <- html
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
     'lax_spacing','space_headers','superscript','ignore_math')
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
      allOptions[seq(9,12)]
}

.onLoad <- function(libname,pkgname)
{

   if (is.null(getOption('markdown.extensions')))
      options(markdown.extensions=markdownExtensions())

   if (is.null(getOption('markdown.HTML.options')))
      options(markdown.HTML.options=markdownHTMLOptions(defaults=TRUE))
}

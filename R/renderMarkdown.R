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

rendererExists <- function(name)
{
   .Call(rmd_renderer_exists,name)
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
      stop("Need input from either a file or a text string")
   }

   # Output is either returned or written to a file
   if (missing(output))
      output <- NULL
   else if (!is.character(output))
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

   invisible(.Call(rmd_render_markdown,file,output,text,renderer,
                   renderer.options, extensions))
}

markdownToHTML <- function(file, output, text, 
                           options=getOption('markdown.HTML.options'),
                           extensions=getOption('markdown.extensions'))
{
   ret <- renderMarkdown(file,output,text,renderer="HTML",
                  renderer.options=options,extensions=extensions)
   if (is.raw(ret))
      ret <- rawToChar(ret)

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

# Markdown extensions are ON by default
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
     'lax_html_blocks','space_headers','superscript')
}

# HTML renderer options are OFF by default
#
# To turn on all options:
#
# options(markdown.HTML.options=markdownHTMLOptions())
# 
# To turn off all options:
# 
# options(markdown.HTML.options=c())
#
markdownHTMLOptions <- function()
{
   c('skip_html', 'skip_style', 'skip_images', 'skip_links',
     'safelink', 'toc', 'hard_wrap', 'use_xhtml', 'escape','smartypants')
}

.onLoad <- function(libname,pkgname)
{
   options(markdown.extensions=markdownExtensions())
   options(markdown.HTML.options=markdownHTMLOptions()[c(5,7,8,10)])
}

.onUnload <- function(libPath)
{
   options(markdown.extensions=NULL)
   options(markdown.HTML.options=NULL)
}

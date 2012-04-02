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

# Function for registering renderers written in R
#
# registerMarkdownRenderer <-
# function(renderer=NULL,...)
# {
# 
# }

renderMarkdown <-
function(file,output,text=NULL,renderer='HTML',
         render.options=getOption('markdown.HTML.options'),
         options=getOption('markdown.options'))
{

   # Input from either a file or character vector
   if (is.character(file) && file.exists(file))
   {
      text <- NULL
   }
   else if (is.character(text) && !is.null(text))
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

   invisible(.Call(render_markdown,file,output,text,renderer,render.options,
                   options))
}


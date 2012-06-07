#
# guid.R
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

.GUIDgenerator <- function()
{
   GUID <- -.Machine$integer.max
   GUIDprefix <- 0L

   nextGUID <- function(){
      guid <- GUID
      suppressWarnings(GUID <<- GUID + 1L)
      if (is.na(GUID))
      {
         GUID <<- -.Machine$integer.max
         GUIDprefix <<- GUIDprefix + 1L
      }
      c(guid,GUIDprefix)
   }

   GUIDstr <- function(len=1L)
   {
      paste(
         unlist(lapply(1:len,function(i)paste(sprintf("%08x",nextGUID(),collapse='')))),
         collapse=''
      )
   }

   list(
      GUID = function() paste('markdown_math',GUIDstr(2),sep='_')
   )
}

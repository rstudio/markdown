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
         GUIDprefix <<- .GUIDprefix + 1L
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

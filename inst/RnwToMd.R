
RweaveMarkdownDriver <- function()
{
   commandEnv <- new.env()
   with(commandEnv,
      `documentclass` <- function(matchedString) ''
   )
   with(commandEnv,
      `tt` <- function(matchedString){
         paste('`',
               sub("\\{\\s*\\\\tt\\s+([^}]+)\\s*\\}","\\1",matchedString),
               '`',sep='')
      }
   )

   with(commandEnv,
      `title` <- function(matchedString)
      {
         sub("\\\\title\\{\\s*([^}]+)\\s*\\}","\n# \\1\n",matchedString)
      }
   )

   with(commandEnv,
      `section` <- function(matchedString)
      {
         sub("\\\\section\\{\\s*([^}]+)\\s*\\}","\n## \\1\n",matchedString)
      }
   )

   with(commandEnv,
      `subsection` <- function(matchedString)
      {
         sub("\\\\subsection\\{\\s*([^}]+)\\s*\\}","\n### \\1\n",matchedString)
      }
   )

   with(commandEnv,
      `texttt` <- function(matchedString)
      {
         sub("\\\\texttt\\{\\s*([^}]+)\\s*\\}","`\\1`",matchedString)
      }
   )

   with(commandEnv,
      `begin` <- function(matchedString)
      {
         cmd <- sub("\\\\begin\\{\\s*([^}]+)\\s*\\}\\s*","\\1",matchedString)
         if (grepl('verbatim',cmd,ignore.case=TRUE)==TRUE)
            return('```\n')
         else
            return('')
      }
   )

   with(commandEnv,
      `end` <- function(matchedString)
      {
         cmd <- sub("\\\\end\\{\\s*([^}]+)\\s*\\}\\s*","\\1",matchedString)
         if (grepl('verbatim',cmd,ignore.case=TRUE)==TRUE)
            return('\n```\n')
         else
            return('')
      }
   )

   transformChunk <- function(fun,matchedString){
      if (exists(fun,commandEnv) && is.function(get(fun,commandEnv))){
         do.call(fun,list(matchedString),envir=commandEnv)
      } else {
         #cat('ignoreing <',matchedString,'>\n')
         # Would be nice to do the following
         #      paste(
         #         "\n``` {latex}",
         #         chunk,
         #         "```",
         #         sep='\n'
         #         )
         ''
      }
   }

   RweaveMarkdownSetup <- function(file,syntax,output,debug=FALSE,debugEnv=NULL)
   {
      list(output=output,options=list(),debug=debug,debugEnv=debugEnv)
   }

   RweaveMarkdownFinish <- function(drobj,error) 
   { 
      close(drobj$output) 
   }

   RweaveMarkdownRuncode <- function(drobj,chunk,chunkopts)
   {

      if (drobj$debug==TRUE){
         assign('chunk',chunk,drobj$debugEnv)
         with(drobj$debugEnv,code[length(code)+1] <- paste(chunk,collapse='\n'))
         return(drobj)
      }
      cat('``` {r}\n',file=drobj$output)
      cat(chunk[2:length(chunk)],sep='\n',file=drobj$output)
      cat('```\n',file=drobj$output)

      drobj
   }

   RweaveMarkdownWritedoc <- function(drobj,chunk)
   {
      # Always remove comments
      chunk <- paste(chunk[!grepl('^%',chunk)],collapse='\n')

      if (drobj$debug==TRUE){
         assign('chunk',chunk,drobj$debugEnv)
         with(drobj$debugEnv,doc[length(doc)+1] <- paste(chunk,collapse='\n'))
         return(drobj)
      }

      regs <- list(
            list(
               reg = "\\{\\s*\\\\(\\w+)\\s+[^}]+\\}"
            ),
            list(
               reg = "\\\\(\\w+)\\{\\s*\\}"
            ),
            list(
               reg = "\\\\(\\w+)((\\[[^]]*\\])+|\\s+|(\\{([^{}]*)\\}+))*"
            )
      )
      for (r in regs){
         loopIter <- 1024
         repeat {
            loopIter <- loopIter - 1
            if (loopIter <=0) {
               warning("Maximum loop iterations!")
               cat(chunk)
               break
            }
            m <- regexpr(r$reg,chunk,perl=TRUE)
            if (m == -1) break

            
            mChunk <- regmatches(chunk,m)
            fun <- sub(r$reg,'\\1',mChunk)

            regmatches(chunk,m) <- transformChunk(fun,mChunk)
         }
      }

      cat(chunk,file=drobj$output)
      drobj
   }

   list(setup = RweaveMarkdownSetup, runcode = RweaveMarkdownRuncode, 
        writedoc = RweaveMarkdownWritedoc, finish = RweaveMarkdownFinish, 
        checkopts = NULL)
}

RnwToMd <- function(file,output=NULL,debug=FALSE){
   if (is.null(output)){
      returnOutput <- TRUE
      con <- textConnection('output',open="w",local=TRUE)
   } else {
      returnOutput <- FALSE
      con <- file(output,open="w+")
   }
   if (debug==TRUE){
      debugEnv <- new.env()
      assign('doc',character(),debugEnv)
      assign('code',character(),debugEnv)
   } else
      debugEnv=NULL
   Sweave(file,driver=RweaveMarkdownDriver(),output=con,debug=debug,
          debugEnv=debugEnv)
   if (debug==TRUE)
      return(as.list(debugEnv))
   if (returnOutput)
      paste(paste(output,collapse='\n'),'\n')
   else
      invisible(NULL)
}

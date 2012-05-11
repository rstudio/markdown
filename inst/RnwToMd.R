
RweaveMarkdownDriver <- function()
{
   commandEnv <- new.env()

   with(commandEnv,
      `unwrap` <- function(matchedString)
      {
         ret <- regmatches(matchedString,regexpr("\\{[^{}]+\\}",matchedString))
         ret <- gsub("[{}]","",ret)
         ret <- gsub("\\n"," ",ret)
         ret
      }
   )

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
         paste("\n# ",unwrap(matchedString),"\n",sep='')
      }
   )

   with(commandEnv,
      `author` <- function(matchedString)
      {
         paste("\n### By ",unwrap(matchedString),"\n",sep='')
      }
   )

   with(commandEnv,
      `section` <- function(matchedString)
      {
         paste("##",unwrap(matchedString),"\n")
      }
   )

   with(commandEnv,
      `subsection` <- function(matchedString)
      {
         paste("###",unwrap(matchedString),"\n")
      }
   )

   with(commandEnv,
      `texttt` <- function(matchedString)
      {
         paste("`",unwrap(matchedString),"`",sep='')
      }
   )

   with(commandEnv,
      `begin` <- function(matchedString)
      {
         cmd <- unwrap(matchedString)
         if (grepl('verbatim',cmd,ignore.case=TRUE)==TRUE)
            return('```\n')
         else
            return('')
      }
   )

   with(commandEnv,
      `end` <- function(matchedString)
      {
         cmd <- unwrap(matchedString)
         if (grepl('verbatim',cmd,ignore.case=TRUE)==TRUE)
            return('\n```\n')
         else
            return('')
      }
   )

   with(commandEnv,
      `pkg` <- function(matchedString)
      {
         paste("**",unwrap(matchedString),"**",sep='')
      }
   )

   with(commandEnv,
      `proglang` <- function(matchedString)
      {
         paste("**",unwrap(matchedString),"**",sep='')
      }
   )

   with(commandEnv,
      `emph` <- function(matchedString)
      {
         paste("**",unwrap(matchedString),"**",sep='')
      }
   )

   with(commandEnv,
      `citep` <- function(matchedString)
      {
         paste("[",unwrap(matchedString),"]",sep='')
      }
   )

   with(commandEnv,
      `cite` <- function(matchedString)
      {
         paste("[",unwrap(matchedString),"]",sep='')
      }
   )

   with(commandEnv,
      `doublebslash` <- function(matchedString)
      {
         sub("(\\w+)\\\\\\\\(\\w+)?","\\1 \\2",matchedString)
      }
   )

   with(commandEnv,
      `eliminate` <- function(matchedString)
      {
         ''
      }
   )


   transformChunk <- function(fun,matchedString,reg){
      if (!is.null(reg$replace)){
         sub(reg$reg,reg$replace,matchedString,perl=TRUE)
      } else if (exists(fun,commandEnv,inherits=FALSE) && is.function(get(fun,commandEnv))){
         do.call(fun,list(matchedString),envir=commandEnv)
      } else if (!is.null(reg$final)){
         do.call(reg$final,list(matchedString),envir=commandEnv)
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

      opts <- ''
      if (!is.null(chunkopts$label)){
         opts <- chunkopts$label
         chunkopts$label <- NULL
      }
      opts <- paste(opts, 
               paste(names(chunkopts),chunkopts,sep='=',collapse=', '),
               sep=', ')

      cat('\n``` {r ',opts,'} \n',sep='',file=drobj$output)
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
               reg = "\\s*\\\\documentclass[^\n]*\n",
               fun = "eliminate"
            ),
            list(
               reg = "\\s*\\\\usepackage[^\n]*\n",
               fun = "eliminate"
            ),
            list(
               reg = "\\s*\\\\newcommand[^\n]*\n",
               fun = "eliminate"
            ),
            list(
               reg = "\\s*\\\\definecolor[^\n]*\n",
               fun = "eliminate"
            ),
            list(
               reg = "\\s*\\\\setlength[^\n]*\n",
               fun = "eliminate"
            ),
            list(
               reg = "\\s*\\\\bibliography[^\n]*\n",
               fun = "eliminate"
            ),
            list(
               reg = "\\s*\\\\bibliographystyle[^\n]*\n",
               fun = "eliminate"
            ),
            list(
               reg = "\\s*\\\\pagestyle[^\n]*\n",
               fun = "eliminate"
            ),
            list(
               reg = "\\s*\\\\thispagestyle[^\n]*\n",
               fun = "eliminate"
            ),
            list(
               reg = "\\s*\\\\maketitle[^\n]*\n",
               fun = "eliminate"
            ),
            list(
               reg = "\\s*\\\\renewcommand[^\n]*\n",
               fun = "eliminate"
            ),
            list(
               reg = "\\s*\\\\DeclareGraphicsExtensions[^\n]*\n",
               fun = "eliminate"
            ),
            list(
               reg = "\\s*\\\\lhead[^\n]*\n",
               fun = "eliminate"
            ),
            list(
               reg = "\\s*\\\\chead[^\n]*\n",
               fun = "eliminate"
            ),
            list(
               reg = "\\s*\\\\lfoot[^\n]*\n",
               fun = "eliminate"
            ),
            list(
               reg = "\\s*\\\\cfoot[^\n]*\n",
               fun = "eliminate"
            ),
            list(
               reg = "\\s*\\\\rfoot[^\n]*\n",
               fun = "eliminate"
            ),

            list(
               reg = "\\s*\\\\newpage[^\n]*\n",
               replace = "\n***\n"
            ),

            list(
               reg = "\\s*\\\\clearpage[^\n]*\n",
               replace = "\n***\n"
            ),

            list(
               reg ="\\s+(\\\\And)",
               replace = " &"
            ),

            list(
               reg = "(\\s+)\\\\\\\\(\\s+)",
               replace = "\\1 \\2"
            ),

            list(
               reg = "\\w+\\\\\\\\(\\w+)?",
               fun = "doublebslash"
            ),
            list(
               reg = "\\{\\s*\\\\(\\w+)\\s+[^}]+\\}"
            ),
            list(
               reg = "\\\\(\\w+)\\{\\s*\\}"
            ),
            list(
               reg = "\\\\href\\{([^{}]+)\\}\\s*\\{([^{}]+)\\}",
               replace = "[\\2] (\\1)"
            ),
            list(
               reg = "\\\\color\\{([^{}]+)\\}\\s*\\{([^{}]+)\\}",
               replace = "\\2"
            ),
            list(
               reg = "\\\\(\\w+)\\*?(\\[[^]]*\\])?\\{[^{}]*\\}",
               final = "unwrap"
            )
      )
      for (r in regs){
         loopIter <- 1024
         repeat {
            loopIter <- loopIter - 1
            if (loopIter <=0) {
               warning("Maximum loop iterations!")
               #cat(chunk)
               break
            }
            m <- regexpr(r$reg,chunk,perl=TRUE)
            if (m == -1) break

            
            mChunk <- regmatches(chunk,m)
            if (!is.null(r$fun))
               fun <- r$fun
            else
               fun <- sub(r$reg,'\\1',mChunk)

            regmatches(chunk,m) <- transformChunk(fun,mChunk,r)
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

RnwToHTML <- function(file,output)
{
   require(knitr)
   temp <- tempfile()
   temp2 <- tempfile()
   RnwToMd(file,temp)
   knit(temp,temp2)
   Md2HTMLPage(temp2,output)
   unlink(c(temp,temp2))
}

# Function for registering renderers written in R
#
# registerMarkdownRenderer <-
# function(renderer=NULL,...)
# {
# 
# }

renderMarkdown <-
function(file,output,renderer='HTML',
         render.options=getOptions('markdown.HTML.options'),
         options=getOptions('markdown.options'))
{
   invisible(.Call(render_markdown,file,output,renderer,render.options,options))
}


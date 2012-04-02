/*
 * Rinit.c
 * 
 * Copyright (C) 2009-1012 by RStudio, Inc.
 * 
 * This program is licensed to you under the terms of version 3 of the
 * GNU General Public License. This program is distributed WITHOUT ANY
 * EXPRESS OR IMPLIED WARRANTY, INCLUDING THOSE OF NON-INFRINGEMENT,
 * MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. Please refer to the
 * GPL (http://www.gnu.org/licenses/gpl-3.0.txt) for more details.
 *
 */
            
#include <R_ext/Rdynload.h>
#include "markdown.h"
#include "Rmarkdown.h"

#define CALLDEF(name, n) {#name,(DL_FUNC) &name, n}
static R_CallMethodDef CallEntries[] = {
   CALLDEF(render_markdown,6),
   CALLDEF(renderer_exists,1),
   {NULL,NULL,0}
};

void R_init_markdown(DllInfo *dll)
{
   R_registerRoutines(dll,NULL,CallEntries, NULL, NULL);
   R_useDynamicSymbols(dll, FALSE);

   init_renderer_list();

   /* Callable functions from other packages' C code */
#define RREGDEF(name) R_RegisterCCallable("markdown", #name, (DL_FUNC) name)
   RREGDEF(bufput);
   RREGDEF(bufgrow);
   RREGDEF(bufnew);
   RREGDEF(bufcstr);
   RREGDEF(bufprefix);
   RREGDEF(bufput);
   RREGDEF(bufputs);
   RREGDEF(bufputc);
   RREGDEF(bufrelease);
   RREGDEF(bufreset);
   RREGDEF(bufslurp);
   RREGDEF(bufprintf);
   RREGDEF(sd_autolink_issafe);
   RREGDEF(sd_autolink__www);
   RREGDEF(sd_autolink__email);
   RREGDEF(sd_autolink__url);
   RREGDEF(sd_markdown_new);
   RREGDEF(sd_markdown_render);
   RREGDEF(sd_markdown_free);
   RREGDEF(sd_version);
}

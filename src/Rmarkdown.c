/*
 * Rmarkdown.c
 * 
 * Copyright (C) 2009-2013 by RStudio, Inc.
 * 
 * This program is licensed to you under the terms of version 2 of the
 * GNU General Public License. This program is distributed WITHOUT ANY
 * EXPRESS OR IMPLIED WARRANTY, INCLUDING THOSE OF NON-INFRINGEMENT,
 * MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. Please refer to the
 * GPL (http://www.gnu.org/licenses/gpl-2.0.txt) for more details.
 *
 */

#include "Rmarkdown.h"

#define NREND 8

static struct rmd_renderer RENDERERS[NREND];

static struct rmd_renderer *renderer(const char *name);

static Rboolean render_to_html(struct buf *ib, struct buf *ob,
                                  SEXP Soptions, SEXP Sextensions)
{
   struct sd_callbacks callbacks;
   struct html_renderopt renderopt;
   unsigned int exts=0, options=0;
   struct sd_markdown *markdown;
   struct buf *htmlbuf;
   Rboolean toc = FALSE, smarty = FALSE;

   /* Marshal extensions */
   if (isString(Sextensions))
   {
      int i;
      for (i = 0; i < LENGTH(Sextensions); i++)
      {
         if (strcasecmp(CHAR(STRING_ELT(Sextensions,i)),
                        "NO_INTRA_EMPHASIS") == 0)
            exts |= MKDEXT_NO_INTRA_EMPHASIS;
         else if (strcasecmp(CHAR(STRING_ELT(Sextensions,i)),
                        "TABLES") == 0)
            exts |= MKDEXT_TABLES;
         else if (strcasecmp(CHAR(STRING_ELT(Sextensions,i)),
                        "FENCED_CODE") == 0)
            exts |= MKDEXT_FENCED_CODE;
         else if (strcasecmp(CHAR(STRING_ELT(Sextensions,i)),
                        "AUTOLINK") == 0)
            exts |= MKDEXT_AUTOLINK;
         else if (strcasecmp(CHAR(STRING_ELT(Sextensions,i)),
                        "STRIKETHROUGH") == 0)
            exts |= MKDEXT_STRIKETHROUGH;
         else if (strcasecmp(CHAR(STRING_ELT(Sextensions,i)),
                        "LAX_SPACING") == 0)
            exts |= MKDEXT_LAX_SPACING;
         else if (strcasecmp(CHAR(STRING_ELT(Sextensions,i)),
                        "SPACE_HEADERS") == 0)
            exts |= MKDEXT_SPACE_HEADERS;
         else if (strcasecmp(CHAR(STRING_ELT(Sextensions,i)),
                        "SUPERSCRIPT") == 0)
            exts |= MKDEXT_SUPERSCRIPT;
         else if (strcasecmp(CHAR(STRING_ELT(Sextensions,i)),
                        "LATEX_MATH") == 0)
            exts |= MKDEXT_LATEX_MATH;
      }
   }

   /* Marshal HTML options */
   if (isString(Soptions))
   {
      int i;
      for (i = 0; i < LENGTH(Soptions); i++)
      {
         if (strcasecmp(CHAR(STRING_ELT(Soptions,i)),
                        "SKIP_HTML") == 0)
            options |= HTML_SKIP_HTML;
         else if (strcasecmp(CHAR(STRING_ELT(Soptions,i)),
                        "SKIP_STYLE") == 0)
            options |= HTML_SKIP_STYLE;
         else if (strcasecmp(CHAR(STRING_ELT(Soptions,i)),
                        "SKIP_IMAGES") == 0)
            options |= HTML_SKIP_IMAGES;
         else if (strcasecmp(CHAR(STRING_ELT(Soptions,i)),
                        "SKIP_LINKS") == 0)
            options |= HTML_SKIP_LINKS;
         else if (strcasecmp(CHAR(STRING_ELT(Soptions,i)),
                        "SAFELINK") == 0)
            options |= HTML_SAFELINK;
         else if (strcasecmp(CHAR(STRING_ELT(Soptions,i)),
                        "TOC") == 0)
         {
            options |= HTML_TOC;
            toc = TRUE;
         }
         else if (strcasecmp(CHAR(STRING_ELT(Soptions,i)),
                        "HARD_WRAP") == 0)
            options |= HTML_HARD_WRAP;
         else if (strcasecmp(CHAR(STRING_ELT(Soptions,i)),
                        "USE_XHTML") == 0)
            options |= HTML_USE_XHTML;
         else if (strcasecmp(CHAR(STRING_ELT(Soptions,i)),
                        "ESCAPE") == 0)
            options |= HTML_ESCAPE;
         else if (strcasecmp(CHAR(STRING_ELT(Soptions,i)),
                        "SMARTYPANTS") == 0)
            smarty = TRUE;
      }
   }

   htmlbuf = bufnew(OUTPUT_UNIT);
   if (!htmlbuf)
   {
      RMD_WARNING_NOMEM;
      return FALSE;
   }

   if (toc==TRUE)
   {
      struct buf *tocbuf = bufnew(OUTPUT_UNIT);

      if (!tocbuf)
      {
         RMD_WARNING_NOMEM;
         return FALSE;
      }

      sdhtml_toc_renderer(&callbacks, &renderopt);
      markdown = sd_markdown_new(exts,16,&callbacks,(void *)&renderopt);
      if (!markdown)
      {
         RMD_WARNING_NOMEM;
         return FALSE;
      }
      
      sd_markdown_render(tocbuf, ib->data, ib->size, markdown);
      sd_markdown_free(markdown);

      bufputs(htmlbuf,"<div id=\"toc\">\n");
      bufputs(htmlbuf,"<div id=\"toc_header\">Table of Contents</div>\n");
      bufput(htmlbuf,tocbuf->data,tocbuf->size);
      bufputs(htmlbuf,"</div>\n");
      bufputs(htmlbuf,"\n");
      bufrelease(tocbuf);
   }

   sdhtml_renderer(&callbacks, &renderopt, options);

   markdown = sd_markdown_new(exts,16,&callbacks,(void *)&renderopt);
   if (!markdown)
   {
      RMD_WARNING_NOMEM;
      return FALSE;
   }

   sd_markdown_render(htmlbuf, ib->data, ib->size, markdown);

   sd_markdown_free(markdown);

   if (smarty==TRUE)
   {
      struct buf *smartybuf = bufnew(OUTPUT_UNIT);
      if (!smartybuf)
      {
         RMD_WARNING_NOMEM;
         return FALSE;
      }
      sdhtml_smartypants(smartybuf,htmlbuf->data,htmlbuf->size);
      bufrelease(htmlbuf);
      htmlbuf = smartybuf;
   }

   bufput(ob,htmlbuf->data,htmlbuf->size);

   bufrelease(htmlbuf);

   return TRUE;
}

void rmd_init_renderer_list()
{
   int i;
   struct rmd_renderer *html;
   for (i=0;i<NREND;i++)
   {
      memset(&RENDERERS[i],0,sizeof(struct rmd_renderer));
   }

   /* Add HTML renderer */
   html = &RENDERERS[0];
   html->name = "HTML";
   html->render = render_to_html;
   html->output_type = "character";
}

Rboolean rmd_renderer_exists(const char *name)
{
   return (renderer(name) != NULL)? TRUE: FALSE;
}

Rboolean rmd_register_renderer(struct rmd_renderer *renderer)
{
   int i, empty_slot = -1, name_exists = -1;

   if (!renderer)
      return FALSE;

   for (i=0;i<NREND;i++)
   {
      if (RENDERERS[i].name == NULL)
      {
         if (empty_slot < 0)
            empty_slot = i;
      }
      else if (strcmp(RENDERERS[i].name,renderer->name)==0)
         name_exists = i;
   }

   /* replace old renderer without warning */
   if (name_exists>=0)
      empty_slot = name_exists;

   if (empty_slot>=0)
   {
      if (RENDERERS[empty_slot].name != NULL)
      {
         free(RENDERERS[empty_slot].name);
         free(RENDERERS[empty_slot].output_type);
      }
      RENDERERS[empty_slot].name = strdup(renderer->name);
      RENDERERS[empty_slot].render = renderer->render;
      RENDERERS[empty_slot].output_type = strdup(renderer->output_type);
   }
   else
   {
      error("Renderer list full!");
      return FALSE;
   }
   return TRUE;
}

SEXP rmd_registered_renderers(void)
{
   SEXP ans;
   SEXP names;
   char *name, *output_type; 
   int i;

   PROTECT(ans = allocVector(STRSXP,NREND));
   PROTECT(names = allocVector(STRSXP,NREND));

   for (i=0;i<NREND;i++)
   {
      if (RENDERERS[i].name != NULL)
      {
         name = RENDERERS[i].name;
         output_type = RENDERERS[i].output_type;

      } else {
         name = "";
         output_type = "";
      }

      SET_STRING_ELT(ans,i,mkChar(name));
      SET_STRING_ELT(names,i,mkChar(output_type));
   }

   setAttrib(ans,R_NamesSymbol,names);
   UNPROTECT(2);

   return ans;
}

static struct rmd_renderer *renderer(const char *name)
{
   int i;
   for (i=0;i<NREND;i++)
   {
      if (RENDERERS[i].name != NULL && strcmp(RENDERERS[i].name,name)==0)
      {
         return &RENDERERS[i];
      }
   }
   return NULL;
}

Rboolean rmd_input_to_buf(SEXP Sfile, SEXP Stext, struct buf *ib)
{
   /* Setup input buffer */
   if (isNull(Sfile))
   {
      int len;
      const char *text = CHAR(STRING_ELT(Stext,0));
      len = strlen(text);
      if (len > 0)
      {
         bufgrow(ib,len);
         bufput(ib,(const void *)text,len);
      }
      else
      {
         warning("Input text is zero length!");
         return FALSE;
      }
   } 
   else
   {
      FILE *in;
      size_t ret;
      const char *file = CHAR(STRING_ELT(Sfile,0));

      in = fopen(file,"r");
      if (!in)
      {
         warning("Cannot open %s!", file);
         return FALSE;
      }
      bufgrow(ib, READ_UNIT);
      while ((ret = fread(ib->data + ib->size, 1, ib->asize - ib->size,
                          in)) > 0) {
         ib->size += ret;
         bufgrow(ib, ib->size + READ_UNIT);
      }
      fclose(in);
   }

   return TRUE;
}

Rboolean rmd_buf_to_output(struct buf *ob, SEXP Soutput, SEXP *raw_vec)
{
   /* Output */
   if (isNull(Soutput))
   {
      PROTECT(*raw_vec = allocVector(RAWSXP, ob->size));
      memcpy(RAW(*raw_vec),ob->data,ob->size);
      UNPROTECT(1);
   }
   else
   {
      const char *filename = CHAR(STRING_ELT(Soutput,0));
      FILE *out = fopen(filename,"w");
      if (!out)
      {
         warning("Cannot save output to %s!", filename);
         return FALSE;
      }

      fwrite(ob->data, 1, ob->size, out);
      fclose(out);

      if (ferror (out))
      {
         warning("Error occurred writing to %s!", filename);
         return FALSE;
      }
   }

   return TRUE;
}

/* Pandoc title blocks are prepended with percents '%'. They start on the
 * first line of the document and contain 3 elements: 'title','author', 
 * and date. Both 'title' and 'author' can extend to multiple lines so
 * long as that line starts with a space, but 'date' cannot.
 */
void skip_pandoc_title_block(struct buf *ib){
	int i = 0;
   size_t pos = 0;

   /* pandoc 1.9.1.1 expects title blocks to start on the first line */
   if (ib->data[0] != '%') return;

   /* We search for at most 3 elements: title, author, and date */
   for (i = 0; i < 3; i++){
      if (ib->data[pos] != '%') break;

      /* Search for end of line */
      while (pos < ib->size && ib->data[pos] != '\n') pos++;
      if (pos < ib->size) pos++;
      else break;

      do {
         /* Only title and author can contain continuation lines,
          * e.g. i < 2 
          */
         if (ib->data[pos] == ' ' && i < 2){
            while (pos < ib->size && ib->data[pos] != '\n') pos++;
            if (pos < ib->size) pos++;
            else break;
         } else {
            break;
         }
      } while(1);
   }

   /* If we've seen a title block, we'll take it off
    * the beginning of our buffer by slurping up pos bytes.
    */
   if (pos > 0) bufslurp(ib,pos);
}

/* Jekyll front matter begins on the first line and the first three characters
 * of the line are '---'. Front matter ends when a line is started with '---'.
 * We skip everything in between including the ending '---'.
 */
void skip_jekyll_front_matter(struct buf *ib){
   int front_matter_found = 0;
   size_t pos = 0;

   /* Jekyll 0.12.0 expects front matter to start on the first line */
   if (ib->size < 3 || !(ib->data[0] == '-' && ib->data[1] == '-' && 
      ib->data[2] == '-') ) return;

   pos = 3;
   do {
      while (pos < ib->size && ib->data[pos] != '\n') pos++;
      if (pos == ib->size) break;
      if (pos+3 < ib->size){
         if (ib->data[pos+1] == '-' && ib->data[pos+2] == '-' && ib->data[pos+3] == '-'){
            front_matter_found = 1;
            pos += 4;
            break;
         } else {
            pos++;
         }
      } else {
         break;
      }
   } while(1);

   if (front_matter_found && pos > 0)
      bufslurp(ib,pos);
}

SEXP rmd_render_markdown(SEXP Sfile, SEXP Soutput, SEXP Stext, SEXP Srenderer,
                            SEXP Soptions, SEXP Sextensions)
{
   const char *name;
   struct buf *ib, *ob;
   SEXP ret_val = R_NilValue;
   Rboolean success;

   name = CHAR(STRING_ELT(Srenderer,0));

   if (!rmd_renderer_exists(name))
   {
      error("Renderer '%s' not registered!",name);
      return R_NilValue;
   }

   ib = bufnew(READ_UNIT);
   if (!ib)
      error("Out of memory!");

   success = rmd_input_to_buf(Sfile,Stext,ib);
   if (!success)
   {
      bufrelease(ib);
      error("Input error!");
   }

   skip_pandoc_title_block(ib);
   skip_jekyll_front_matter(ib);

   ob = bufnew(OUTPUT_UNIT);
   if (!ob)
      error("Out of memory!");

   success = renderer(name)->render(ib,ob,Soptions,Sextensions);
   if (!success)
   {
      bufrelease(ib);
      bufrelease(ob);
      error("Render error!");
   }

   success = rmd_buf_to_output(ob,Soutput,&ret_val);

   bufrelease(ib);
   bufrelease(ob);

   if (!success)
      error("Output error!");

   return ret_val;
}

SEXP rmd_render_smartypants(SEXP Sfile, SEXP Soutput, SEXP Stext)
{
   struct buf *ib, *ob;
   SEXP ret_val = R_NilValue;
   Rboolean success;

   ib = bufnew(READ_UNIT);
   if (!ib)
      error("Out of memory!");

   success = rmd_input_to_buf(Sfile, Stext, ib);

   if (!success)
   {
      bufrelease(ib);
      error("Input error!");
   }

   ob = bufnew(OUTPUT_UNIT);
   if (!ob)
      error("Out of memory!");

   sdhtml_smartypants(ob,ib->data,ib->size);

   success = rmd_buf_to_output(ob,Soutput,&ret_val);

   bufrelease(ib);
   bufrelease(ob);

   if (!success)
      error("Output error!");

   return ret_val;
}

/*
 * Rmarkdown.c
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

#include "Rmarkdown.h"

#define READ_UNIT 1024
#define OUTPUT_UNIT 64

#define NREND 8
struct rmd_renderer RENDERERS[NREND];

struct rmd_renderer *renderer(const char *name);

static void html_init_args(struct sd_markdown_new_args *args,
                           SEXP Soptions, SEXP Sextensions)
{
   struct sd_callbacks *callbacks;
   struct html_renderopt *renderopt;
   unsigned int exts=0, options=0;

   callbacks = malloc(sizeof(struct sd_callbacks));
   renderopt = malloc(sizeof(struct html_renderopt));
   if (!callbacks || !renderopt)
   {
      if (callbacks) free(callbacks);
      if (renderopt) free(renderopt);
      error("malloc failure in html_init_args!");
      return;
   }

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
                        "LAX_HTML_BLOCKS") == 0)
            exts |= MKDEXT_LAX_HTML_BLOCKS;
         else if (strcasecmp(CHAR(STRING_ELT(Sextensions,i)),
                        "SPACE_HEADERS") == 0)
            exts |= MKDEXT_SPACE_HEADERS;
         else if (strcasecmp(CHAR(STRING_ELT(Sextensions,i)),
                        "SUPERSCRIPT") == 0)
            exts |= MKDEXT_SUPERSCRIPT;
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
            options |= HTML_TOC;
         else if (strcasecmp(CHAR(STRING_ELT(Soptions,i)),
                        "HARD_WRAP") == 0)
            options |= HTML_HARD_WRAP;
         else if (strcasecmp(CHAR(STRING_ELT(Soptions,i)),
                        "USE_XHTML") == 0)
            options |= HTML_USE_XHTML;
         else if (strcasecmp(CHAR(STRING_ELT(Soptions,i)),
                        "ESCAPE") == 0)
            options |= HTML_ESCAPE;
      }
   }

   sdhtml_renderer(callbacks, renderopt, options);

   args->extensions = exts;
   args->max_nesting = 16;
   args->callbacks = callbacks;
   args->opaque = (void *)renderopt;
}

static void html_destroy_args(struct sd_markdown_new_args *args)
{
   free(args->callbacks);
   free(args->opaque);
}

void init_renderer_list()
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
   html->init_args = html_init_args;
   html->destroy_args = html_destroy_args;
}

Rboolean register_renderer(struct rmd_renderer *renderer)
{
   int i, empty_slot = -1, name_exists = -1;
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
   if (name_exists>0)
      empty_slot = name_exists;

   if (empty_slot>0)
   {
      if (name_exists<0)
         RENDERERS[empty_slot].name = strdup(renderer->name);
      RENDERERS[empty_slot].init_args = renderer->init_args;
      RENDERERS[empty_slot].destroy_args = renderer->destroy_args;
   }
   else
   {
      error("Renderer list full!");
      return FALSE;
   }
   return TRUE;
}

SEXP renderer_exists(SEXP Srenderer)
{
   SEXP ans;

   PROTECT(ans = allocVector(LGLSXP,1));
   LOGICAL(ans)[0] = FALSE;

   if (isString(Srenderer))
   {
      const char *name = CHAR(STRING_ELT(Srenderer,0));
      if (renderer(name) != NULL)
         LOGICAL(ans)[0] = TRUE;
   }

   UNPROTECT(1);

   return ans;
}

struct rmd_renderer *renderer(const char *name)
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

SEXP render_markdown(SEXP Sfile, SEXP Soutput, SEXP Stext, SEXP Srenderer,
                            SEXP Soptions, SEXP Sextensions)
{
   const char *text, *name;
   struct buf *ib, *ob;
   struct sd_markdown_new_args args;
   struct sd_markdown *markdown;

   name = CHAR(STRING_ELT(Srenderer,0));

   if (!LOGICAL(renderer_exists(Srenderer))[0])
   {
      error("Renderer '%s' not registered!",name);
      return R_NilValue;
   }

   /* Setup input buffer */
   if (isNull(Sfile))
   {
      int len;
      text = CHAR(STRING_ELT(Stext,0));
      len = strlen(text);
      ib = bufnew(len);
      bufgrow(ib,len);
      bufput(ib,(const void *)text,len);
   } 
   else
   {
      FILE *in;
      size_t ret;
      text = CHAR(STRING_ELT(Sfile,0));
      in = fopen(text,"r");
      if (!in)
      {
         error("Cannot open %s!", text);
         return R_NilValue;
      }
      ib = bufnew(READ_UNIT);
      bufgrow(ib, READ_UNIT);
      while ((ret = fread(ib->data + ib->size, 1, ib->asize - ib->size,
                          in)) > 0) {
         ib->size += ret;
         bufgrow(ib, ib->size + READ_UNIT);
      }
      fclose(in);
   }

   ob = bufnew(OUTPUT_UNIT);
   renderer(name)->init_args(&args,Soptions,Sextensions);
   markdown = sd_markdown_new(args.extensions,args.max_nesting,args.callbacks,
                              args.opaque);

   sd_markdown_render(ob, ib->data, ib->size, markdown);

   sd_markdown_free(markdown);
   renderer(name)->destroy_args(&args);
   bufrelease(ib);

   /* Output */
   if (isNull(Soutput))
   {
      SEXP ans;
      PROTECT(ans = allocVector(RAWSXP, ob->size));
      memcpy(RAW(ans),ob->data,ob->size);
      UNPROTECT(1);
      bufrelease(ob);
      return ans;
   }
   else
   {
      const char *filename = CHAR(STRING_ELT(Soutput,0));
      FILE *out = fopen(filename,"w");
      size_t ret;
      if (!out)
      {
         error("Cannot save output to %s!", filename);
         bufrelease(ob);
         return R_NilValue;
      }
      ret = fwrite(ob->data, 1, ob->size, out);
      if (ret < 0)
         warning("Error occured writing to %s!", filename);
      fclose(out);
      bufrelease(ob);
   }

   return R_NilValue;
}

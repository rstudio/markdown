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
Rboolean renderer_exists(SEXP Srenderer);

static void html_init_args(struct sd_markdown_new_args *args,
                           SEXP Shtml_options, SEXP Soptions)
{
   struct sd_callbacks *callbacks;
   struct html_renderopt *options;

   callbacks = malloc(sizeof(struct sd_callbacks));
   options = malloc(sizeof(struct html_renderopt));
   if (!callbacks || !options)
   {
      if (callbacks) free(callbacks);
      if (options) free(options);
      error("malloc failure in html_init_args!");
      return;
   }

   sdhtml_renderer(callbacks, options, 0);

   args->extensions = 0;
   args->max_nesting = 16;
   args->callbacks = callbacks;
   args->opaque = (void *)options;
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

Rboolean renderer_exists(SEXP Srenderer)
{
   if (isString(Srenderer))
   {
      const char *name = CHAR(STRING_ELT(Srenderer,0));
      if (renderer(name) != NULL)
         return TRUE;
   }

   return FALSE;
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
                            SEXP Srender_options, SEXP Soptions)
{
   const char *text, *name;
   struct buf *ib, *ob;
   struct sd_markdown_new_args args;
   struct sd_markdown *markdown;

   name = CHAR(STRING_ELT(Srenderer,0));

   if (renderer_exists(Srenderer) == FALSE)
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
   renderer(name)->init_args(&args,Srender_options,Soptions);
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

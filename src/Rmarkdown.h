/*
 * Rmarkdown.h
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

#include <R.h>
#include <Rinternals.h>
#include "markdown.h"
#include "html.h"

struct sd_markdown_new_args {
   unsigned int extensions;
   size_t max_nesting;
   struct sd_callbacks *callbacks;
   void *opaque;
};

struct rmd_renderer {
   char *name;
   void (*init_args)(struct sd_markdown_new_args *,SEXP, SEXP);
   void (*destroy_args)(struct sd_markdown_new_args *);
};

extern void init_renderer_list();

extern Rboolean register_renderer(struct rmd_renderer *);

extern SEXP render_markdown(SEXP Sfile, SEXP Soutput, SEXP Stext,
                            SEXP Srenderer, SEXP Srender_options,
                            SEXP Soptions);
extern SEXP renderer_exists(SEXP name);

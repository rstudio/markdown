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

#define READ_UNIT 1024
#define OUTPUT_UNIT 64
#define RMD_WARNING_NOMEM warning("Out of memory!")

/* 
 * output_type should be either "raw" or "character"
 */
struct rmd_renderer {
   char *name;
   Rboolean (*render)(struct buf *,struct buf *, SEXP, SEXP);
   char *output_type;
};

extern void rmd_init_renderer_list();

extern Rboolean rmd_buf_to_output(struct buf *, SEXP, SEXP *);

extern Rboolean rmd_input_to_buf(SEXP, SEXP, struct buf *);

extern Rboolean rmd_register_renderer(struct rmd_renderer *);

extern Rboolean rmd_renderer_exists(const char *name);

extern SEXP rmd_registered_renderers(void);

extern SEXP rmd_render_markdown(SEXP Sfile, SEXP Soutput, SEXP Stext,
                            SEXP Srenderer, SEXP Srender_options,
                            SEXP Soptions);

extern SEXP rmd_render_smartypants(SEXP Sfile, SEXP Soutput, SEXP Stext);

extern SEXP rmd_b64encode_data( SEXP Sdata);

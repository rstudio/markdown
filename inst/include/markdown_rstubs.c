/*
 * markdown_rstubs.c
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

/* Inspird by Matrix/inst/include/Matrix_stubs.c */

#include "markdown_rstubs.h"

Rboolean rmd_register_renderer(struct rmd_renderer *renderer)
{
   static Rboolean (*fun)(struct rmd_renderer *) = NULL;
   if (fun==NULL)
      fun = (Rboolean (*)(struct rmd_renderer *))
            R_GetCCallable("markdown","register_renderer");
   return fun(renderer);
}

int rmd_bufgrow(struct buf *buf, size_t sz)
{
   static int (*fun)(struct buf *, size_t) = NULL;
   if (fun==NULL)
      fun = (int (*)(struct buf *,size_t))R_GetCCallable("markdown","bufgrow");
   return fun(buf,sz);
}

struct buf *rmd_bufnew(size_t sz)
{
   static struct buf *(*fun)(size_t) = NULL;
   if (fun==NULL)
      fun = (struct buf *(*)(size_t))R_GetCCallable("markdown","bufnew");
   return fun(sz);
}

const char *rmd_bufcstr(struct buf *buf)
{
   static const char *(*fun)(struct buf *) = NULL;
   if (fun==NULL)
      fun = (const char *(*)(struct buf *))R_GetCCallable("markdown","bufcstr");
   return fun(buf);
}

int rmd_bufprefix(const struct buf *buf, const char *prefix)
{
   static int (*fun)(const struct buf *, const char *) = NULL;
   if (fun==NULL)
      fun = (int (*)(const struct buf *, const char *))R_GetCCallable("markdown","bufprefix");
   return fun(buf,prefix);
}

void rmd_bufput(struct buf *buf, const void *v, size_t sz)
{
   static void (*fun)(struct buf *, const void *, size_t) = NULL;
   if (fun==NULL)
      fun = (void (*)(struct buf *, const void *, size_t))R_GetCCallable("markdown","bufput");
   return fun(buf,v,sz);
}

void rmd_bufputs(struct buf *buf, const char *c)
{
   static void (*fun)(struct buf *, const char *) = NULL;
   if (fun==NULL)
      fun = (void (*)(struct buf *, const char *))R_GetCCallable("markdown","bufputs");
   return fun(buf,c);
}

void rmd_bufputc(struct buf *, int i);
{
   static void (*fun)(struct buf *, int) = NULL;
   if (fun==NULL)
      fun = (void (*)(struct buf *, int))R_GetCCallable("markdown","bufputc");
   return fun(buf,i);
}

void rmd_bufrelease(struct buf *buf)
{
   static void (*fun)(struct buf *) = NULL;
   if (fun==NULL)
      fun = (void (*)(struct buf *t))R_GetCCallable("markdown","bufrelease");
   return fun(buf);
}
   
void rmd_bufreset(struct buf *buf)
{
   static void (*fun)(struct buf *) = NULL;
   if (fun==NULL)
      fun = (void (*)(struct buf *t))R_GetCCallable("markdown","bufreset");
   return fun(buf);
}

void rmd_bufslurp(struct buf *buf, size_t sz)
{
   static int (*fun)(struct buf *, size_t) = NULL;
   if (fun==NULL)
      fun = (int (*)(struct buf *,size_t))R_GetCCallable("markdown","bufslurp");
   return fun(buf,sz);
}

void rmd_bufprintf(struct buf *buf, const char *fmt, ...)
{
   va_list ap;
   static int (*fun)(struct buf *, const char *, ...) = NULL;
   if (fun==NULL)
      fun = (int (*)(struct buf *,const char *, ...))
         R_GetCCallable("markdown","bufprintf");

	va_start(ap, fmt);
   fun(buf,fmt,ap);
   va_end(ap);
}

extern int
rmd_sd_autolink_issafe(const uint8_t *link, size_t link_len){
   static int (*fun)(const uint8_t *, size_t) = NULL;
   if (fun==NULL)
      fun = (int (*)(const uint8_t *, size_t))
         R_GetCCallable("markdown","sd_autolink_issafe");
   return fun(link,sz);
}

extern size_t
rmd_sd_autolink__www(size_t *rewind_p, struct buf *link, uint8_t *data,
                     size_t offset, size_t size)
{
   static size_t (*fun)(size_t *rewind_p, struct buf *link, uint8_t *data,
                        size_t offset, size_t size) = NULL;
   if (fun==NULL)
      fun = (size_t (*fun)(size_t *rewind_p, struct buf *link, uint8_t *data,
                           size_t offset, size_t size))
         R_GetCCallable("markdown","sd_autolink__www");
   return fun(rewind_p,link,data,offset,size);
}

extern size_t
rmd_sd_autolink__email(size_t *rewind_p, struct buf *link, uint8_t *data,
                       size_t offset, size_t size)
{
   static size_t (*fun)(size_t *rewind_p, struct buf *link, uint8_t *data,
                        size_t offset, size_t size) = NULL;
   if (fun==NULL)
      fun = (size_t (*fun)(size_t *rewind_p, struct buf *link, uint8_t *data,
                           size_t offset, size_t size))
         R_GetCCallable("markdown","sd_autolink__email");
   return fun(rewind_p,link,data,offset,size);
}

extern size_t
rmd_sd_autolink__url(size_t *rewind_p, struct buf *link, uint8_t *data,
                     size_t offset, size_t size)
{
   static size_t (*fun)(size_t *rewind_p, struct buf *link, uint8_t *data,
                        size_t offset, size_t size) = NULL;
   if (fun==NULL)
      fun = (size_t (*fun)(size_t *rewind_p, struct buf *link, uint8_t *data,
                           size_t offset, size_t size))
         R_GetCCallable("markdown","sd_autolink__url");
   return fun(rewind_p,link,data,offset,size);
}

extern struct sd_markdown *
rmd_sd_markdown_new(unsigned int extensions, size_t max_nesting,
                    const struct sd_callbacks *callbacks, void *opaque)
{
   static struct sd_markdown *(*fun)(unsigned int, size_t,
                                     const struct sd_callbacks *callbacks,
                                     void *) = NULL;
   if (fun==NULL)
      fun = (struct sd_markdown *(*)(unsigned int, size_t,
                                     const struct sd_callbacks *callbacks,
                                     void *))
         R_GetCCallable("markdown","sd_markdown_new");
   return fun(extensions,max_nesting,callbacks,opaque);
}

extern void
rmd_sd_markdown_render(struct buf *ob, const uint8_t *document, 
                       size_t doc_size, struct sd_markdown *md)
{
   static void (*fun)(struct buf *, const uint8_t *, size_t,
                      struct sd_markdown *) = NULL;
   if (fun==NULL)
      fun = (void (*)(struct buf *, const uint8_t *, size_t,
                      struct sd_markdown *))
         R_GetCCallable("markdown","sd_markdown_render");
   return fun(ob,document,doc_size,md);
}

extern void
rmd_sd_markdown_free(struct sd_markdown *md)
{
   static void (*fun)(struct sd_markdown *) = NULL;
   if (fun==NULL)
      fun = (void (*)(struct sd_markdown *))
         R_GetCCallable("markdown","rmd_sd_markdown_free");
   return fun(md);
}

extern void
rmd_sd_version(int *major, int *minor, int *revision)
{
   static void (*fun)(int *, int *, int *) = NULL;
   if (fun==NULL)
      fun = (void (*)(inst *, inst *, int *))
         R_GetCCallable("markdown","rmd_sd_version");
   return fun(major,minor,revision);
}

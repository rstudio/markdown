/*
 * markdown_rstubs.h
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

#ifndef MARKDOWN_RSTUBS
#define MARKDOWN_RSTUBS

#include "markdown.h"

#ifdef __cplusplus
extern "C" {
#endif

Rboolean rmd_register_renderer(struct rmd_renderer *renderer);
#define register_renderer rmd_register_renderer

/* bufgrow: increasing the allocated size to the given value */
int rmd_bufgrow(struct buf *, size_t);
#define bufgrow rmd_bufgrow

/* bufnew: allocation of a new buffer */
struct buf *rmd_bufnew(size_t) __attribute__ ((malloc));
#define bufnew rmd_bufnew

/* bufnullterm: NUL-termination of the string array (making a C-string) */
const char *rmd_bufcstr(struct buf *);
#define bufcstr rmd_bufcstr

/* bufprefix: compare the beginning of a buffer with a string */
int rmd_bufprefix(const struct buf *buf, const char *prefix);
#define bufprefix rmd_bufprefix

/* bufput: appends raw data to a buffer */
void rmd_bufput(struct buf *, const void *, size_t);
#define bufput rmd_bufput

/* bufputs: appends a NUL-terminated string to a buffer */
void rmd_bufputs(struct buf *, const char *);
#define bufputs rmd_bufputs

/* bufputc: appends a single char to a buffer */
void rmd_bufputc(struct buf *, int);
#define bufputc rmd_bufputc

/* bufrelease: decrease the reference count and free the buffer if needed */
void rmd_bufrelease(struct buf *);
#define bufrelease rmd_bufrelease

/* bufreset: frees internal data of the buffer */
void rmd_bufreset(struct buf *);
#define bufreset rmd_bufreset

/* bufslurp: removes a given number of bytes from the head of the array */
void rmd_bufslurp(struct buf *, size_t);
#define bufslurp rmd_bufslurp

/* bufprintf: formatted printing to a buffer */
void rmd_bufprintf(struct buf *, const char *, ...) __attribute__ ((format (printf, 2, 3)));
#define bufprintf rmd_bufprintf

extern int
rmd_sd_autolink_issafe(const uint8_t *link, size_t link_len);
#define sd_autolink_issafe rmd_sd_autolink_issafe

extern size_t
rmd_sd_autolink__www(size_t *rewind_p, struct buf *link, uint8_t *data,
                     size_t offset, size_t size);
#define sd_autolink__www rmd_sd_autolink__www

extern size_t
rmd_sd_autolink__email(size_t *rewind_p, struct buf *link, uint8_t *data,
                       size_t offset, size_t size);
#define sd_autolink__email rmd_sd_autolink__email

extern size_t
rmd_sd_autolink__url(size_t *rewind_p, struct buf *link, uint8_t *data,
                     size_t offset, size_t size);
#define sd_autolink__url rmd_sd_autolink__url

extern struct sd_markdown *
rmd_sd_markdown_new(
	unsigned int extensions,
	size_t max_nesting,
	const struct sd_callbacks *callbacks,
	void *opaque);
#define sd_markdown_new rmd_sd_markdown_new

extern void
rmd_sd_markdown_render(struct buf *ob, const uint8_t *document, size_t doc_size,
                       struct sd_markdown *md);
#define sd_markdown_render rmd_sd_markdown_render

extern void
rmd_sd_markdown_free(struct sd_markdown *md);
#define sd_markdown_free rmd_sd_markdown_free

extern void
rmd_sd_version(int *major, int *minor, int *revision);
#define sd_version rmd_sd_version

#ifdef __cplusplus
}
#endif

#endif // MARKDOWN_RSTUBS
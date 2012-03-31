#include "markdown.h"

/* RMD_BUFPUTSL: optimized bufputs of a string litteral */
#define RMD_BUFPUTSL(output, literal) \
	rmd_bufput(output, literal, sizeof literal - 1)

/* bufgrow: increasing the allocated size to the given value */
int rmd_bufgrow(struct buf *, size_t);

/* bufnew: allocation of a new buffer */
struct buf *rmd_bufnew(size_t) __attribute__ ((malloc));

/* bufnullterm: NUL-termination of the string array (making a C-string) */
const char *rmd_bufcstr(struct buf *);

/* bufprefix: compare the beginning of a buffer with a string */
int rmd_bufprefix(const struct buf *buf, const char *prefix);

/* bufput: appends raw data to a buffer */
void rmd_bufput(struct buf *, const void *, size_t);

/* bufputs: appends a NUL-terminated string to a buffer */
void rmd_bufputs(struct buf *, const char *);

/* bufputc: appends a single char to a buffer */
void rmd_bufputc(struct buf *, int);

/* bufrelease: decrease the reference count and free the buffer if needed */
void rmd_bufrelease(struct buf *);

/* bufreset: frees internal data of the buffer */
void rmd_bufreset(struct buf *);

/* bufslurp: removes a given number of bytes from the head of the array */
void rmd_bufslurp(struct buf *, size_t);

/* bufprintf: formatted printing to a buffer */
void rmd_bufprintf(struct buf *, const char *, ...) __attribute__ ((format (printf, 2, 3)));

extern int
rmd_sd_autolink_issafe(const uint8_t *link, size_t link_len);

extern size_t
rmd_sd_autolink__www(size_t *rewind_p, struct buf *link, uint8_t *data,
                     size_t offset, size_t size);

extern size_t
rmd_sd_autolink__email(size_t *rewind_p, struct buf *link, uint8_t *data,
                       size_t offset, size_t size);

extern size_t
rmd_sd_autolink__url(size_t *rewind_p, struct buf *link, uint8_t *data,
                     size_t offset, size_t size);

extern struct sd_markdown *
rmd_sd_markdown_new(
	unsigned int extensions,
	size_t max_nesting,
	const struct sd_callbacks *callbacks,
	void *opaque);

extern void
rmd_sd_markdown_render(struct buf *ob, const uint8_t *document, size_t doc_size,
                       struct sd_markdown *md);

extern void
rmd_sd_markdown_free(struct sd_markdown *md);

extern void
rmd_sd_version(int *major, int *minor, int *revision);

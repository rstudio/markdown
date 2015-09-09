#include "Rmarkdown.h"
#include <stdio.h>
#include <stdlib.h>

/*
 * Translation Table as described in RFC1113
 */
static const char cb64[]="ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";

/*
 * encodeblock
 *
 * encode 3 8-bit binary bytes as 4 '6-bit' characters
 */
void encodeblock( unsigned char in[3], unsigned char out[4], int len )
{
   out[0] = cb64[ in[0] >> 2 ];
   out[1] = cb64[ ((in[0] & 0x03) << 4) | ((in[1] & 0xf0) >> 4) ];
   out[2] = (unsigned char) (len > 1 ? cb64[ ((in[1] & 0x0f) << 2) | ((in[2] & 0xc0) >> 6) ] : '=');
   out[3] = (unsigned char) (len > 2 ? cb64[ in[2] & 0x3f ] : '=');
}

SEXP rmd_b64encode_data( SEXP Sdata)
{
   unsigned char in[3], out[4];
   int i, len;

   int data_len = LENGTH(Sdata);
   int data_elem = 0;
   Rbyte *data = RAW(Sdata);
   SEXP ans;
   struct buf *databuf;
   const char* str;

   /* Create a buffer that grows READ_UNIT bytes at
      the time as a larger buffer is needed */
   databuf = bufnew(READ_UNIT);
   if (!databuf)
   {
      RMD_WARNING_NOMEM;
      return R_NilValue;
   }

   while( data_elem < data_len ) {
      len = 0;
      for( i = 0; i < 3; i++ ) {
         if( data_elem < data_len ) {
            in[i] = (unsigned char) data[data_elem++];
            len++;
         } else {
            in[i] = 0;
         }
      }
      if( len ) {
         encodeblock( in, out, len );
         bufput(databuf,out,4);
      }
   }

   str = bufcstr(databuf);
   if (!str)
   {
      bufrelease(databuf);
      RMD_WARNING_NOMEM;
      return R_NilValue;
   }
     
   PROTECT(ans = allocVector(STRSXP,1));
   SET_STRING_ELT(ans,0,mkChar(str));
   bufrelease(databuf);
   UNPROTECT(1);

   return ans;
}

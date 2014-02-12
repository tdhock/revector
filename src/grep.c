/* -*- compile-command: "R CMD INSTALL .. && R CMD check .." -*- */
#include <R.h>
#include <Rinternals.h>

#ifdef ENABLE_NLS
#include <libintl.h>
#define _(String) dgettext ("pkg", String)
/* replace pkg as appropriate */
#else
#define _(String) (String)
#endif

/* Some systems using --with-system-pcre might have pcre headers in
   a subdirectory -- not seen recently.
*/
#ifdef HAVE_PCRE_PCRE_H
# include <pcre/pcre.h>
#else
# include <pcre.h>
#endif

SEXP
matcheach_interface(SEXP strings, SEXP patterns){
    const char *string, *spat, *errorptr;
    char *captured;
    SEXP ans, dmn;         /* return vect and its attribute */
    pcre *re_pcre = NULL /* -Wall */;
    pcre_extra *re_pe = NULL;
    const unsigned char *tables = NULL /* -Wall */;
    int erroffset, i, j;
    int capture_count, *ovector = NULL, ovector_size = 0, /* -Wall */
	*start_ptr, *end_ptr, 
	start_byte, end_byte, captured_bytes,
	name_count, name_entry_size, info_code, capture_count_i;
    char *name_table;
    SEXP capture_names;
    int cflags = 0;

    // analyze the first element of pattern for groups/names.

/*    if (useBytes) */

    spat = CHAR(STRING_ELT(patterns, 0));

/*    else if (use_WC) ;
    else if (use_UTF8) {*/

/*  spat = translateCharUTF8(STRING_ELT(pattern, 0));
    if (!utf8Valid(spat)) error(_("regular expression is invalid UTF-8"));
*/

/*    } else {
	spat = translateChar(STRING_ELT(pat, 0));
	if (mbcslocale && !mbcsValid(spat))
	    error(_("regular expression is invalid in this locale"));
	    }*/

    tables = pcre_maketables();
    re_pcre = pcre_compile(spat, cflags, &errorptr, &erroffset, tables);
    if (!re_pcre) {
	if (errorptr)
	    warning(_("PCRE pattern compilation error\n\t'%s'\n\tat '%s'\n"),
		    errorptr, spat+erroffset);
	error(_("invalid regular expression '%s'"), spat);
    }

    pcre_fullinfo(re_pcre, re_pe, PCRE_INFO_NAMECOUNT, &name_count);
    pcre_fullinfo(re_pcre, re_pe, PCRE_INFO_NAMEENTRYSIZE, &name_entry_size);
    pcre_fullinfo(re_pcre, re_pe, PCRE_INFO_NAMETABLE, &name_table);
    info_code = 
	pcre_fullinfo(re_pcre, re_pe, PCRE_INFO_CAPTURECOUNT, 
		      &capture_count);
    pcre_free(re_pcre);
    if(info_code < 0)
	error(_("'pcre_fullinfo' returned '%d' "), info_code);
    ovector_size = (capture_count + 1) * 3;
    ovector = (int *) malloc(ovector_size*sizeof(int));
    if(ovector == NULL)
	error(_("not enough memory for ovector"));
    SEXP thisname;
    PROTECT(capture_names = allocVector(STRSXP, capture_count));
    for(i = 0; i < name_count; i++) {
	char *entry = name_table + name_entry_size * i;
	PROTECT(thisname = mkChar(entry + 2));
	int capture_num = (entry[0]<<8) + entry[1] - 1;
	SET_STRING_ELT(capture_names, capture_num, thisname);
	UNPROTECT(1);
    }

    PROTECT(ans = allocMatrix(STRSXP, length(patterns), capture_count));
    PROTECT(dmn = allocVector(VECSXP, 2));
    SET_VECTOR_ELT(dmn, 1, capture_names);
    setAttrib(ans, R_DimNamesSymbol, dmn);

    // Go through the strings and patterns.
    for(i = 0; i < length(patterns); i++){
	string = CHAR(STRING_ELT(strings, i));
	spat = CHAR(STRING_ELT(patterns, i));
	re_pcre = pcre_compile(spat, cflags, &errorptr, &erroffset, tables);
	if (!re_pcre) {
	    if (errorptr)
		warning(_("PCRE pattern compilation error\n\t'%s'\n\tat '%s'\n"),
			errorptr, spat+erroffset);
	    error(_("invalid regular expression '%s'"), spat);
	}
	info_code = 
	    pcre_fullinfo(re_pcre, re_pe, PCRE_INFO_CAPTURECOUNT, 
			  &capture_count_i);
	if(info_code < 0)
	    error(_("'pcre_fullinfo' returned '%d' "), info_code);
	if(capture_count_i != capture_count)
	    error(_("first pattern has %d group(s), pattern %d has %d"),
		    capture_count, i+1, capture_count_i);
	int rc, slen = (int) strlen(string);
	rc = pcre_exec(re_pcre, re_pe, string, slen, 0, 0, ovector,
		       ovector_size);
	for(j = 0; j < capture_count; j++){
	    if (rc >= 0) { // if matched...
/* parse the "ovector" from pcre which contains the match and group
   start and end bytes. it is organized as follows: match_start
   match_end group1_start group1_end group2_start group2_end ...

   Toby Dylan Hocking 2014-02-10, adapted from my R source code
   contributions in src/main/grep.c */
		start_ptr = ovector + 2*(j+1);
		end_ptr = start_ptr + 1;
		start_byte = *start_ptr;
		end_byte = *end_ptr;
		captured_bytes = end_byte - start_byte;
		captured = (char*)malloc((captured_bytes+1)*sizeof(char));
		if(captured == NULL)
		    error(_("not enough memory for captured string"));
		strncpy(captured, string+start_byte, captured_bytes);
		captured[captured_bytes] = '\0';
		SET_STRING_ELT(ans, i + j*length(patterns), mkChar(captured));
		free(captured);
	    } else {
		// otherwise set this row to NA.
		SET_STRING_ELT(ans, i + j*length(patterns), NA_STRING);
	    }
	}
	pcre_free(re_pcre);
    }
    UNPROTECT(3);
    pcre_free((void *)tables);
    free(ovector);
    return ans;
}



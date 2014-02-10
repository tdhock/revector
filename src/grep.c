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

/* This function is used to convert a single ovector (match_start,
   match_end) pair (in bytes) to a pair of (match_start in 1-indexed
   unicode characters stored in mptr, match_length in number of
   unicode characters stored in lenptr)

   We have to do this once for the match and once for every group, so
   I generalized the method and call it twice from
   extract_match_and_groups to avoid repetitive code.
   
   Toby Dylan Hocking 2011-03-10
*/
static Rboolean 
ovector_extract_start_length(int *ovector,
			     int *mptr,int *lenptr,const char *string)
{
    int st = ovector[0];
    *mptr = st + 1; /* index from one */
    *lenptr = ovector[1] - st;
}

/* this function generalizes the parsing of the "ovector" from pcre
   which contains the match and group start and end bytes. it is
   organized as follows: match_start match_end group1_start group1_end
   group2_start group2_end ... we process these in regexpr and
   gregexpr, so I made this function to avoid duplicating code between
   the 2. 

   Toby Dylan Hocking 2011-03-10 */
static Rboolean 
extract_match_and_groups(int *ovector, int capture_count,
			 int *mptr, int *lenptr, int *cptr, int *clenptr,
			 const char *string, int capture_stride)
{
    Rboolean foundAll =
	ovector_extract_start_length(ovector, mptr, lenptr, string);
    /* also extract capture locations */
    for(int i = 0; i < capture_count; i++) {
	int ind = capture_stride*i;
	ovector_extract_start_length(ovector+2*(i+1),
				     cptr+ind, clenptr+ind, string);
    }
    return foundAll;
}

SEXP
matcheach_interface(SEXP strings, SEXP patterns){
    const char *string = NULL;
    Rboolean foundAll = FALSE, foundAny = FALSE;
    int matchIndex = -1, start = 0;
    SEXP ans, dmn, matchlen;         /* return vect and its attribute */
    SEXP capturebuf, capturelenbuf;
    SEXP matchbuf, matchlenbuf; /* buffers for storing multiple matches */
    int bufsize = 1024;         /* starting size for buffers */
    PROTECT_INDEX cb, clb, mb, mlb;
    const char *spat;

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


    pcre *re_pcre = NULL /* -Wall */;
    pcre_extra *re_pe = NULL;
    const unsigned char *tables = NULL /* -Wall */;
    int erroffset, i, j;
    const char *errorptr;
    int capture_count, *ovector = NULL, ovector_size = 0, /* -Wall */
	name_count, name_entry_size, info_code, capture_count_i;
    char *name_table;
    SEXP capture_names = R_NilValue;
    int cflags = 0;

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
    if(info_code < 0)
	error(_("'pcre_fullinfo' returned '%d' "), info_code);
    ovector_size = (capture_count + 1) * 3;
    ovector = (int *) malloc(ovector_size*sizeof(int));
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
	    error(_("first pattern has %d groups, element %d has %d"),
		    capture_count, i+1, capture_count_i);
	int rc, slen = (int) strlen(string);
	rc = pcre_exec(re_pcre, re_pe, string, slen, start, 0, ovector,
		       ovector_size);
	if (rc >= 0) { // if matched...
	    for(j = 0; j < capture_count; j++){
		SET_STRING_ELT(ans, i, mkChar(string));
		/* extract_match_and_groups(ovector, capture_count, */
		/* 			 INTEGER(matchbuf) + matchIndex, */
		/* 			 INTEGER(matchlenbuf) + matchIndex, */
		/* 			 INTEGER(capturebuf) + matchIndex, */
		/* 			 INTEGER(capturelenbuf) + matchIndex, */
		/* 			 string, bufsize); */
	    }
	} else {
	    for(j = 0; j < capture_count; j++){
		SET_STRING_ELT(ans, i + j*length(patterns), NA_STRING);
	    }
	}
    }

    UNPROTECT(3);
    if (re_pe) pcre_free(re_pe);
    pcre_free(re_pcre);
    pcre_free((void *)tables);
    free(ovector);
    return ans;
}



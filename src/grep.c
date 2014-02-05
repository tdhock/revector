/* -*- compile-command: "R CMD INSTALL .." -*- */
#include <R.h>
#include <Rinternals.h>

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
ovector_extract_start_length(Rboolean use_UTF8,int *ovector,
			     int *mptr,int *lenptr,const char *string)
{
    Rboolean foundAll = FALSE;
    int st = ovector[0];
    *mptr = st + 1; /* index from one */
    *lenptr = ovector[1] - st;
    if (use_UTF8) {
	/* Unfortunately these are in bytes */
	if (st > 0) {
	    *mptr = 1 + getNc(string, st);
	    if (*mptr <= 0) { /* an invalid string */
		*mptr = NA_INTEGER;
		foundAll = TRUE; /* if we get here, we are done */
	    }
	}
	*lenptr = getNc(string + st, *lenptr);
	if (*lenptr < 0) {/* an invalid string */
	    *lenptr = NA_INTEGER;
	    foundAll = TRUE;
	}
    }
    return foundAll;
}

/* this function generalizes the parsing of the "ovector" from pcre
   which contains the match and group start and end bytes. it is
   organized as follows: match_start match_end group1_start group1_end
   group2_start group2_end ... we process these in regexpr and
   gregexpr, so I made this function to avoid duplicating code between
   the 2. 

   Toby Dylan Hocking 2011-03-10 */
static Rboolean 
extract_match_and_groups(Rboolean use_UTF8, int *ovector, int capture_count,
			 int *mptr, int *lenptr, int *cptr, int *clenptr,
			 const char *string, int capture_stride)
{
    Rboolean foundAll =
	ovector_extract_start_length(use_UTF8, ovector, mptr, lenptr, string);
    /* also extract capture locations */
    for(int i = 0; i < capture_count; i++) {
	int ind = capture_stride*i;
	ovector_extract_start_length(use_UTF8, ovector+2*(i+1),
				     cptr+ind, clenptr+ind, string);
    }
    return foundAll;
}

SEXP
matcheach_interface(SEXP string, SEXP pattern){
{
    Rboolean foundAll = FALSE, foundAny = FALSE;
    int matchIndex = -1, start = 0;
    SEXP ans, matchlen;         /* return vect and its attribute */
    SEXP capturebuf, capturelenbuf;
    SEXP matchbuf, matchlenbuf; /* buffers for storing multiple matches */
    int bufsize = 1024;         /* starting size for buffers */
    PROTECT_INDEX cb, clb, mb, mlb;

    pcre *re_pcre = NULL /* -Wall */;
    pcre_extra *re_pe = NULL;
    const unsigned char *tables = NULL /* -Wall */;

	tables = pcre_maketables();
	re_pcre = pcre_compile(spat, cflags, &errorptr, &erroffset, tables);
	if (!re_pcre) {
	    if (errorptr)
		warning(_("PCRE pattern compilation error\n\t'%s'\n\tat '%s'\n"),
			errorptr, spat+erroffset);
	    error(_("invalid regular expression '%s'"), spat);
	}
	if (n > 10) {
	    re_pe = pcre_study(re_pcre, 0, &errorptr);
	    if (errorptr)
		warning(_("PCRE pattern study error\n\t'%s'\n"), errorptr);
	}
	/* also extract info for named groups */
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

    PROTECT_WITH_INDEX(capturebuf = 
		       allocVector(INTSXP, bufsize*capture_count), &cb);
    PROTECT_WITH_INDEX(capturelenbuf = 
		       allocVector(INTSXP, bufsize*capture_count), &clb);
    PROTECT_WITH_INDEX(matchbuf = allocVector(INTSXP, bufsize), &mb);
    PROTECT_WITH_INDEX(matchlenbuf = allocVector(INTSXP, bufsize), &mlb);
    while (!foundAll) {
	int rc, slen = (int) strlen(string);
	rc = pcre_exec(re_pcre, re_pe, string, slen, start, 0, ovector,
		       ovector_size);
	if (rc >= 0) {
	    if ((matchIndex + 1) == bufsize) {
		/* Reallocate match buffers */
		int newbufsize = bufsize * 2;
		SEXP tmp;
		tmp = allocVector(INTSXP, newbufsize);
		for (int j = 0; j < bufsize; j++) /* or use memcpy */
		    INTEGER(tmp)[j] = INTEGER(matchlenbuf)[j];
		REPROTECT(matchlenbuf = tmp, mlb);
		tmp = allocVector(INTSXP, newbufsize);
		for (int j = 0; j < bufsize; j++)  /* or use memcpy */
		    INTEGER(tmp)[j] = INTEGER(matchbuf)[j];
		REPROTECT(matchbuf = tmp, mb);
		if (capture_count) {
		    tmp = allocVector(INTSXP, newbufsize*capture_count);
		    for(int j = 0; j < bufsize; j++)
			for(int i = 0; i < capture_count; i++)
			    INTEGER(tmp)[j + newbufsize*i] = 
				INTEGER(capturebuf)[j + bufsize*i];
		    REPROTECT(capturebuf = tmp, cb);
		    tmp = allocVector(INTSXP, newbufsize*capture_count);
		    for(int j = 0; j < bufsize; j++)
			for(int i = 0; i < capture_count; i++)
			    INTEGER(tmp)[j + newbufsize*i] = 
				INTEGER(capturelenbuf)[j + bufsize*i];
		    REPROTECT(capturelenbuf =  tmp, clb);
		}
		bufsize = newbufsize;
	    }
	    matchIndex++;
	    foundAny = TRUE;
	    foundAll = 
		extract_match_and_groups(use_UTF8, ovector, capture_count,
					 INTEGER(matchbuf) + matchIndex,
					 INTEGER(matchlenbuf) + matchIndex,
					 INTEGER(capturebuf) + matchIndex,
					 INTEGER(capturelenbuf) + matchIndex,
					 string, bufsize);
	    /* we need to advance 'start' in bytes */
	    if (ovector[1] - ovector[0] == 0)
		start = ovector[0] + 1;
	    else
		start = ovector[1];
	    if (start >= slen) foundAll = 1;
	} else {
	    foundAll = TRUE;
	    if (!foundAny) matchIndex = 0;
	}
    }
    PROTECT(ans = allocVector(INTSXP, matchIndex + 1));
    /* Protect in case install("match.length") allocates */
    PROTECT(matchlen = allocVector(INTSXP, matchIndex + 1));
    setAttrib(ans, install("match.length"), matchlen);
    if(useBytes) {
	setAttrib(ans, install("useBytes"), ScalarLogical(TRUE));
    }
    UNPROTECT(1);
    if (foundAny) {
	for (int j = 0; j <= matchIndex; j++) {
	    INTEGER(ans)[j] = INTEGER(matchbuf)[j];
	    INTEGER(matchlen)[j] = INTEGER(matchlenbuf)[j];
	}
    } else
	INTEGER(ans)[0] = INTEGER(matchlen)[0] = -1;

    if (capture_count) {
	SEXP capture, capturelen, dmn;
	PROTECT(capture = allocMatrix(INTSXP, matchIndex+1, capture_count));
	PROTECT(capturelen = allocMatrix(INTSXP, matchIndex+1, capture_count));
	PROTECT(dmn = allocVector(VECSXP, 2));
	SET_VECTOR_ELT(dmn, 1, capture_names);
	setAttrib(capture, R_DimNamesSymbol, dmn);
	setAttrib(capturelen, R_DimNamesSymbol, dmn);
	if (foundAny) {
	    for (int j = 0; j <= matchIndex; j++)
		for(int i = 0; i < capture_count; i++) {
		    int return_index = j + (matchIndex+1) * i;
		    int buffer_index = j + bufsize * i;
		    INTEGER(capture)[return_index] =
			INTEGER(capturebuf)[buffer_index];
		    INTEGER(capturelen)[return_index] =
			INTEGER(capturelenbuf)[buffer_index];
		}
	} else
	    for(int i = 0; i < capture_count; i++)
		INTEGER(capture)[i] = INTEGER(capturelen)[i] = -1;
	setAttrib(ans, install("capture.start"), capture);
	setAttrib(ans, install("capture.length"), capturelen);
	setAttrib(ans, install("capture.names"), capture_names);
	UNPROTECT(3);
    }
    UNPROTECT(5); /* 4 with indices, ans */
    return ans;
}


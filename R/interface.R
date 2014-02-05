str_match_each <- structure(function
### Run each regex on each string.
(string,
### Character vector to parse.
 pattern
### Character vector of the same length with regular expressions. Also
### each regular expression should have the same number of capture
### groups/parentheses.
 ){
  string <- as.character(string)
  pattern <- as.character(pattern)
  stopifnot(is.character(string))
  stopifnot(is.character(pattern))
  stopifnot(length(string)==length(pattern))
  .Call("matcheach_interface", string, pattern, 
        PACKAGE="revector")
### A character matrix with the matched groups.
},ex=function(){
  genotype <- c("AA", "TT", "GA", "TAATAAA", "TAAATAA")
  two <- function(x)sprintf("^(?<A>%s)(?<B>%s)$", x, x)
  snp <- two("[ATCG]")
  taa <- two("TAA|TAAA")
  pat <- c(snp, snp, snp, taa, taa)
  str_match_each(genotype, pat)
})

\name{str_match_each}
\alias{str_match_each}
\title{str match each}
\description{Run each regex on each string.}
\usage{str_match_each(string, pattern)}
\arguments{
  \item{string}{Character vector to parse.}
  \item{pattern}{Character vector of the same length with regular expressions. Also
each regular expression should have the same number of capture
groups/parentheses.}
}

\value{A character matrix with the matched groups.}

\author{Toby Dylan Hocking}




\examples{
genotype <- c("AA", "TT", "GA", "TAATAAA", "TAAATAA")
two <- function(x)sprintf("^(?<A>\%s)(?<B>\%s)$", x, x)
snp <- two("[ATCG]")
taa <- two("TAA|TAAA")
pat <- c(snp, snp, snp, taa, taa)
str_match_each(genotype, pat)
}

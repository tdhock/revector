\name{genotypes}
\alias{genotypes}
\docType{data}
\title{
Detecting genotype variants
}
\description{
Reference and Alternate DNA sequences are given, and we want to extract
exactly two of these from each genotype, or NA.
}
\usage{data(genotypes)}
\format{
  A data frame with 625442 observations on the following 3 variables.
  \describe{
    \item{\code{Ref}}{The reference sequence/allele on hg19}
    \item{\code{Alt}}{Comma-separated list of possible Alternate alleles.}
    \item{\code{genotype}}{Two alleles which are concatenated without
      any separator.}
  }
}
\keyword{datasets}

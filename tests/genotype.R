library(revector)
## Positive control: we should parse these genotypes.
genotype <- c("AA", "TT", "GA", "TAATAAA", "TAAATAA")
two <- function(x)sprintf("^(?<A>%s)(?<B>%s)$", x, x)
snp <- two("[ATCG]")
taa <- two("TAA|TAAA")
pat <- c(snp, snp, snp, taa, taa)
match <- str_match_each(genotype, pat)
expect <- rbind(c("A", "A"),
                c("T", "T"),
                c("G", "A"),
                c("TAA", "TAAA"),
                c("TAAA", "TAA"))
stopifnot(is.character(match))
stopifnot(expect == match)
stopifnot(colnames(expect) == colnames(match))

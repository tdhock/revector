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
stopifnot(is.matrix(match))
stopifnot(nrow(match) == nrow(expect))
stopifnot(ncol(match) == ncol(expect))
stopifnot(!is.null(colnames(match)))
stopifnot(c("A", "B") == colnames(match))
stopifnot(expect == match)

data(genotypes)
ref.alt <- sapply(genotypes, function(x)nchar(as.character(x)))
##table(ref.alt[,1:2])
stopifnot(ref.alt < 1e3) #bigger will crash regexp engine.
group.pat <- sprintf("%s|%s", genotypes$Ref, gsub(",", "|", genotypes$Alt))
genotypes[grepl("N", group.pat),]
pat <- sprintf("^(?<A>%s)(?<B>%s)$", group.pat, group.pat)
ab <- str_match_each(genotypes$genotype, pat)

## Check that all NN/NAs are detected correctly.
is.nn <- is.na(ab[,1])
nn <- genotypes[is.nn,]
stopifnot(nn$genotype == "NN")

## Check that all others are detected correctly.
not.nn <- data.frame(genotypes, ab)[!is.nn,]
with(not.nn, {
  stopifnot(identical(paste0(A, B), genotype))
})

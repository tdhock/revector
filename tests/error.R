library(revector)
check <- function(strings, patterns, error.or.result){
  stopifnot(is.character(error.or.result))
  if(is.matrix(error.or.result) && is.null(colnames(error.or.result))){
    colnames(error.or.result) <- rep("", ncol(error.or.result))
  }
  list(strings=strings, patterns=patterns, error.or.result=error.or.result)
}
to.check <-
  list(check(c("foobar", "foobar"), c("(oo)(ba)", "(o)(b)"),
             rbind(c("oo", "ba"),
                   c("o", "b"))),
       check(c("foobar", "foobar"), c("(?<first>oo)(ba)", "(o)(b)"),
             structure(rbind(c("oo", "ba"),
                             c("o", "b")),
                       dimnames=list(NULL, list("first", "")))),
       check(c("foobar", "foobar"), c("(?<first>oo)(?<second>ba)", "(o)(b)"),
             cbind(first=c("oo", "o"), second=c("ba", "b"))),
       check(c("foobarbaz", "foobar"),
             c("(?<first>oo)(?<second>ba)r(?<third>baz)", "(o)(b)(a)"),
             cbind(first=c("oo", "o"), second=c("ba", "b"), third=c("baz", "a"))),
       check(c("zzzzzzzzzz", "zzzzobazzz"),
             c("(?<first>oo)(?<second>ba)r(?<third>baz)", "(o)(b)(a)"),
             cbind(first=c(NA, "o"), second=c(NA, "b"), third=c(NA, "a"))),
       check(c("foobarbaz", "zzzzzzzzzz"),
             c("(?<first>oo)(?<second>ba)r(?<third>baz)", "(o)(b)(a)"),
             cbind(first=c("oo", NA), second=c("ba", NA), third=c("baz", NA))),
       check(c("foobar", "foobar"), c("(?<first>oo)(ba)", "(o)(?<second>b)"),
             structure(rbind(c("oo", "ba"),
                             c("o", "b")),
                       dimnames=list(NULL, list("first", "")))),
       check(c("foobar", "foobar"), c("(oo)(ba)", "(ob"),
             "invalid regular expression '(ob'"),
       check(c("foobar", "foobar"), c("(oo)ba)", "(o)b"),
             "invalid regular expression '(oo)ba)'"),
       check(c("foobar", "foobar"), c("(oo)(ba)", "(o)b"),
             "first pattern has 2 group(s), pattern 2 has 1"),
       check(c("foobar", "foobar"), c("(oo)(ba)", "ob"),
             "first pattern has 2 group(s), pattern 2 has 0"),
       check(c("foobar", "foobar"), c("(oo)(ba)", "((o)(b))"),
             "first pattern has 2 group(s), pattern 2 has 3"))
for(L in to.check){
  generated <- tryCatch({
    str_match_each(L$strings, L$patterns)
  }, error=function(e){
    e$mes
  })
  stopifnot(is.character(generated))
  expected <- L$error.or.result
  if(is.matrix(generated)){ ## result.
    stopifnot(dim(expected) == dim(generated))
    stopifnot(colnames(expected) == colnames(generated))
  }else{ ##error.
    stopifnot(length(generated)==1)
  }
  if(!identical(expected, generated)){
    print(expected)
    print(generated)
    stop("expected first, but got second")
  }
}

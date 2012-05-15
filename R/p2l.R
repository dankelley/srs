p2l <- function(p, scheme=c("A", "B"))
{
    scheme <- match.arg(scheme)
    n <- length(p)
    g <- gradeMapping(scheme)
    rval <- vector("character", n)
    for (i in 1:n)
        rval[i] <- g$letter[sum(p[i]>=g$lowerLimit)]
    rval
}

l2p <- function(l, scheme=c("A", "B"))
{
    scheme <- match.arg(scheme)
    g <- gradeMapping(scheme)
    np <- length(l)
    rval <- vector("numeric", np)
    if (scheme == "A") {
        letters <- c(g$letter, "perfect")
        limits <- c(g$lowerLimit, 100)
    } else if (scheme == "B") {
        letters <- c(g$letter, "perfect")
        limits <- c(g$lowerLimit, 4.3)
    } else {
        stop("unknown scheme")     # cannot get here
    }
    for (i in 1:np)
        rval[i] <- mean(limits[which(letters==l[i])+c(0,1)])
    rval
}

subset.srs <- function(x, subset, ...)
{
    if (!inherits(x, "srs"))
        stop("method is only for srs objects")
    if (length(subset)<2)
        stop("Temporary restriction: the subset must have two or more elements")
    n <- length(subset)
    rval <- vector("list", n)
    for (i in 1:n)
        rval[[i]] <- x[[subset[i]]]
    class(rval) <- "srs"
    return(rval)
}

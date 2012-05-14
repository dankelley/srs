sortByName <- function(x, decreasing = FALSE, ...)
{
    if (!inherits(x, "srs")) stop("method is only for srs objects")
    name <- NULL
    n <- length(x)
    for (i in 1:n)
        name <- c(name, x[[i]]$name)
    o <- sort(name, decreasing=decreasing, index.return=TRUE)$ix
    res <- vector("list", n)
    for (i in 1:n)
        res[[i]] <- x[[o[i]]]
    class(res) <- class(x)
    res
}

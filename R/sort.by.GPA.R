sort.by.GPA <- function(x, decreasing = FALSE, ...)
{
    if (!inherits(x, "srs")) stop("method is only for srs objects")
    gpa <- NULL
    n <- length(x)
    for (i in 1:n)
        gpa <- c(gpa, mean(x[[i]]$grades$point))
    o <- sort(gpa, decreasing=decreasing, index.return=TRUE)$ix
    res <- vector("list", n)
    for (i in 1:n)
        res[[i]] <- x[[o[i]]]
    class(res) <- class(x)
    res
}

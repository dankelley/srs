trimtime <- function(x, start.times, stop.times)
{
    if (!inherits(x, "srs"))
        stop("method is only for srs objects")
    n <- length(x)
    if (missing(start.times))
        stop("Must provide start times")
    if (length(start.times) != n)
        stop("Need to have ", n, " start.times, but got ", length(start.times))
    if (missing(stop.times))
        stop.times <- rep(3333, n)
    if (length(stop.times) != length(start.times))
        stop("lengths of start.times and stop.times must agree")
    rval <- x
    for (i in 1:n) {
        keep <- (start.times[i] <= rval[[i]]$grades$year) & (rval[[i]]$grades$year <= stop.times[i])
        rval[[i]]$grades$course     <- rval[[i]]$grades$course[keep]
        rval[[i]]$grades$year       <- rval[[i]]$grades$year[keep]
        rval[[i]]$grades$letter     <- rval[[i]]$grades$letter[keep]
        rval[[i]]$grades$point      <- rval[[i]]$grades$point[keep]
        rval[[i]]$grades$coursename <- rval[[i]]$grades$coursename[keep]
    }
    rval
}

summary.srs <- function(object, ...)
{
    if (!inherits(object, "srs"))
        stop("method is only for srs objects")
    n <- length(object)
    for (i in 1:n) {
        info <- sprintf("%-15s | %3s | %3.2f mo | %.2f%% (%.2f/%.0f) | %d published, %d submitted",
                        object[[i]]$name,
                        object[[i]]$program,
                        difftime(Sys.Date(), as.POSIXct(object[[i]]$enrolled), "days")/30,
                        mean(object[[i]]$grades$point),
                        sum(object[[i]]$grades$point),
                        length(object[[i]]$grades$point),
                        length(object[[i]]$papers.published)+length(object[[i]]$papers.inpress),
                        length(object[[i]]$papers.submitted))
        cat(info, "\n",sep="")
        if ("notes" %in% names(object[[i]]) && nchar(object[[i]]$notes > 0))
            cat(object[[i]]$notes, "\n")
    }
}

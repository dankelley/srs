summary.srs <- function(object, ...)
{
    if (!inherits(object, "srs"))
        stop("method is only for srs objects")
    n <- length(object)
    for (i in 1:n) {
        info <- sprintf("%20s %3s/%3.0f GPA %.2f=%.2f/%.0f Presentations %d  Published %d  In-Press %d  Submitted %d  In-prep %d",
                        object[[i]]$name,
                        object[[i]]$program,
                        object[[i]]$months,
                        mean(object[[i]]$grades$point),
                        sum(object[[i]]$grades$point),
                        length(object[[i]]$grades$point),
                        length(object[[i]]$presentations),
                        length(object[[i]]$papers.published),
                        length(object[[i]]$papers.inpress),
                        length(object[[i]]$papers.submitted),
                        length(object[[i]]$papers.inprep))
        cat(info, "\n",sep="")
    }
}

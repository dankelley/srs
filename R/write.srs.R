write.srs <- function(srs, con=stdout())
{
    if (!inherits(srs, "srs")) stop("method is only for srs objects")
    if (is.character(con)) {
        con <- file(con, "w")
        on.exit(close(con))
    }
    n <- length(srs)
    for (i in 1:n) {
        cat("Student\n",file=con)
        cat("\t", srs[[i]]$name, "\n",sep="",file=con)
        cat("Program\n",file=con)
        cat("\t",srs[[i]]$program,"\n",sep="",file=con)
        len <- length(srs[[i]]$scholarships)
        if (len > 0) {
            cat("Scholarships\n",file=con)
            for (j in 1:len) cat("\t",srs[[i]]$scholarships[j],"\n",sep="",file=con)
        }
        len <- length(srs[[i]]$prizes)
        if (len > 0) {
            cat("Prizes\n",file=con)
            for (j in 1:len) cat("\t",srs[[i]]$prizes[j],"\n",sep="",file=con)
        }
        len <- length(srs[[i]]$presentations)
        if (len > 0) {
            cat("Presentations\n",file=con)
            for (j in 1:len) cat("\t",srs[[i]]$presentations[j],"\n",sep="",file=con)
        }
        len <- length(srs[[i]]$papers.published)
        if (len > 0) {
            cat("Papers published\n",file=con)
            for (j in 1:len) cat("\t",srs[[i]]$papers.published[j],"\n",sep="",file=con)
        }
        len <- length(srs[[i]]$papers.inpress)
        if (len > 0) {
            cat("Papers in press\n",file=con)
            for (j in 1:len) cat("\t",srs[[i]]$papers.inpress[j],"\n",sep="",file=con)
        }
        len <- length(srs[[i]]$papers.submitted)
        if (len > 0) {
            cat("Papers submitted\n",file=con)
            for (j in 1:len) cat("\t",srs[[i]]$papers.submitted[j],"\n",sep="",file=con)
        }
        len <- length(srs[[i]]$papers.inprep)
        if (len > 0) {
            cat("Papers in preparation\n",file=con)
            for (j in 1:len) cat("\t",srs[[i]]$papers.inprep[j],"\n",sep="",file=con)
        }
        gg <- srs[[i]]$grades
        len <- length(gg$course)
        if (len > 0) {
            cat("Grades\n",file=con)
            for (j in 1:len) cat("\t",gg$course[j],",",gg$year[j],",",gg$letter[j],",\"",gg$coursename[j],"\"\n",sep="",file=con)
        }
        cat("----\n",file=con)
    }
}

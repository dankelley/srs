read.srs <- function(file="fake.dat", debug=FALSE ,silent=FALSE, year.start=0)
{
    trim <- function(string) { sub(iws,"",string) }
    lines <- readLines(file)
    n <- length(lines)
    if (!silent) cat(paste("File ", file, "has", n, "lines.  Students list is ...\n"))
    for (i in 1:n) { # trim trailing space
        if (debug) cat(paste("[",lines[i],"]\n",sep=""))
        lines[i] <- sub("[[:space:]]*$", "", lines[i])
        if (debug) cat(paste("  (",lines[i],")\n",sep=""))
    }
    iws <- "^[\t ]+"
    res <- vector("list", 1000) # will trim later
    nstudent <- 0
    i <- 1
    name <- NULL
    while (i <= n) {
        if(debug)cat(file,":",i," is '",lines[i],"'\n",sep="")
        if (lines[i] == "Student") {
            if (debug) cat(paste("student at line ", i, "\n"))
            nstudent <- nstudent + 1
            program <- ""
            months <- NA
            scholarships <- NULL
            prizes <- NULL
            presentations <- NULL
            papers.published <- NULL
            papers.inpress <- NULL
            papers.submitted <- NULL
            papers.inprep <- NULL
            grades <- NULL
            if (n <= (i <- i + 1))
                break
            name <- trim(lines[i])
            if(debug)cat(paste(file,":", i, " gives 'Student' as '", name, "'\n", sep=""))
            while (i < n) {  # Get data on this student
                i <- i + 1
                if (lines[i] == "Student") {
                    i <- i - 1
                    break;
                }
                else if (lines[i] == "Program") {
                    if (n <= (i <- i + 1)) stop(paste(file,":",i," ERROR: no Program data"))
                    program <- trim(lines[i])
                    if(debug)cat(paste(file,":", i, " gives 'Program' as '", program, "'\n", sep=""))
                }
                else if (lines[i] == "Months") {
                    if (n <= (i <- i + 1)) stop(paste(file,":",i," ERROR: no Months data"))
                    months <- as.numeric(trim(lines[i]))
                    if(debug)cat(paste(file,":", i, " gives 'Months' as '", months, "'\n", sep=""))
                }
                else if (lines[i] == "Scholarships") {
                    if(debug)cat(paste(file,":",i, " is 'Scholarships'\n",sep=""))
                    while (n >= (i <- i + 1)) {
                        if (0<length(grep(iws, lines[i]))) {
                            scholarships <- c(scholarships, trim(lines[i]))
                            if(debug)cat(paste(file,":",i," scholarship is  '",trim(lines[i]),"'\n",sep=""))
                        } else {
                            i <- i - 1 # so we can catch the data in next pass
                            break;
                        }
                    }
                }
                else if (lines[i] == "Prizes") {
                    if(debug)cat(paste(file,":",i, " is 'Prizes'\n",sep=""))
                    while (n >= (i <- i + 1)) {
                        if (0<length(grep(iws, lines[i]))) {
                            prizes <- c(prizes, trim(lines[i]))
                            if(debug)cat(paste(file,":",i," prize is  '",trim(lines[i]),"'\n",sep=""))
                        } else {
                            i <- i - 1 # so we can catch the data in next pass
                            break;
                        }
                    }
                }
                else if (lines[i] == "Presentations") {
                    if(debug)cat(paste(file,":",i,"is 'Presentations'\n"))
                    while (n >= (i <- i + 1)) {
                        if (0<length(grep(iws, lines[i]))) {
                            presentations <- c(presentations, trim(lines[i]))
                            if(debug)cat(paste(file,":",i," presentation is  '",trim(lines[i]),"'\n",sep=""))
                        } else {
                            i <- i - 1 # so we can catch the data in next pass
                            break;
                        }
                    }
                }
                else if (lines[i] == "Papers published") {
                    if(debug)cat(paste(file,":",i," is 'Papers published'\n",sep=""))
                    while (n >= (i <- i + 1)) {
                        if (0<length(grep(iws, lines[i]))) {
                            papers.published <- c(papers.published, trim(lines[i]))
                            if(debug)cat(paste(file,":",i," paper published is  '",trim(lines[i]),"'\n",sep=""))
                        } else {
                            i <- i - 1 # so we can catch the data in next pass
                            break;
                        }
                    }
                }
                else if (lines[i] == "Papers in press") {
                    if(debug)cat(paste(file,":",i," is 'Papers in press'\n",sep=""))
                    while (n >= (i <- i + 1)) {
                        if (0<length(grep(iws, lines[i]))) {
                            papers.inpress <- c(papers.inpress, trim(lines[i]))
                            if(debug)cat(paste(file,":",i," paper in press is  '",trim(lines[i]),"'\n",sep=""))
                        } else {
                            i <- i - 1 # so we can catch the data in next pass
                            break;
                        }
                    }
                }
                else if (lines[i] == "Papers submitted") {
                    if(debug)cat(paste(file,":",i," is 'Papers submitted'\n",sep=""))
                    while (n >= (i <- i + 1)) {
                        if (0<length(grep(iws, lines[i]))) {
                            papers.submitted <- c(papers.submitted, trim(lines[i]))
                            if(debug)cat(paste(file,":",i," paper submitted is  '",trim(lines[i]),"'\n",sep=""))
                        } else {
                            i <- i - 1 # so we can catch the data in next pass
                            break;
                        }
                    }
                }
                else if (lines[i] == "Papers in preparation") {
                    if(debug)cat(paste(file,":",i," is 'Papers in preparation'\n",sep=""))
                    while (n >= (i <- i + 1)) {
                        if (0<length(grep(iws, lines[i]))) {
                            papers.inprep <- c(papers.inprep, trim(lines[i]))
                            if(debug)cat(paste(file,":",i," paper in preparation is '",trim(lines[i]),"'\n",sep=""))
                        } else {
                            i <- i - 1 # so we can catch the data in next pass
                            break;
                        }
                    }
                }
                else if (lines[i] == "Grades") {
                    if(debug)cat(paste(file,":",i," is 'Grades'\n",sep=""))
                    course <- NULL
                    year <- NULL
                    letter <- NULL # letter
                    point <- NULL # 'point', as in GPA
                    coursename <- NULL
                    ng <- 0
                    while (n >= (i <- i + 1)) {
                        if (0<length(grep(iws, lines[i]))) {
                            if(debug) cat(paste(file,":",i," parsing '",lines[i],"' as a grade\n",sep=""))
                            tmp <- strsplit(trim(lines[i]), ",")[[1]];
                            if (tmp[2] > year.start) {
                                course <- c(course, tmp[1])
                                year   <- c(year,   tmp[2])
                                t <- sub(" ", "", tmp[3])
                                letter <- c(letter, t)
                                point  <- c(point, l2p(t))
                                if (length(tmp) > 3) coursename <- c(coursename, tmp[4])
                                else coursename <- c(coursename, "")
                                if (debug) cat("    grade = ", tmp[3], " or ", l2p(t), "\n")
                                ng <- ng + 1
                            } else {
                                cat("Skipping grade in year", tmp[2], "\n")
                            }
                        } else {
                            if (debug) cat(paste(file,":",i," is end of student record\n",sep=""))
                            if (ng > 0) {
                                grades <- list(course=course, year=year, letter=letter, point=point, coursename=coursename)
                                if(debug)cat(paste("\tThis student has gpa",mean(point),"\n"))
                                i <- i - 1 # so we can catch the data in next pass
                                break
                            } else {
                                stop(file,":",i," ERROR: no grades found")
                            }
                        }
                    }
                }
                else if (substr(lines[i], 1, 4) == "----") {
                    if(debug)cat(paste(file,":",i,"is the end of a student record\n"))
                    res[[nstudent]] <- list(name=name,
                                            program=program,
                                            months=months,
                                            scholarships=scholarships,
                                            prizes=prizes,
                                            presentations=presentations,
                                            papers.published=papers.published,
                                            papers.inpress=papers.inpress,
                                            papers.submitted=papers.submitted,
                                            papers.inprep=papers.inprep,
                                            grades=grades)
                    if (!silent) cat(paste("\t",name, ": gpa=", mean(point),"\n",sep=""))
                    if (i == n) {
                        if(debug)cat(paste(file,":",i," this file seems to be well-formed\n",sep=""))
                        break
                    }
                }
                else {
                    stop(file,":",i," ERROR: unrecognized line '",lines[i],"'\n",sep="")
                }
            }
        }
        i <- i + 1
    }
    if (!silent) cat(paste("File has ",nstudent,"student records, listed above\n"))
    length(res) <- nstudent
    class(res) <- "srs"
    return(res)
}


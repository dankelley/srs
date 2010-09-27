\name{srs-package}
\alias{srs-package}
\docType{package}
\title{Analysis of Student Records}
\description{Read, summarize, and analyze student-record data.}

\details{The srs package is designed to count beans in student records;
thus, the focus is on grades and scholarships.  A typical use might be
as follows.


\preformatted{
library(grid) # need this for graphs
library(srs)
data(srs)
srs.sorted <- sort.by.GPA(srs)
plot(srs.sorted)
srs10 <- subset.srs(srs.sorted, 1:10)
plot(srs10)
}

The input data for \code{read.srs()} must follow a format exactly like
the following.   (Indentation can be either whitespace or tabs, but
it is manditory.)
\preformatted{
Student
        Rex, Elizabeth
Program
        MSc
Scholarships
        Regal 2000
        Regal 2001
Prizes
		Born into it
Presentations
        How we shall be addressed
Papers published
        Call me princess
Papers submitted
        Maybe it's late
Papers in press
        Call me queen
Papers in preparation
        But just call me
Grades
        OCEA0001,1999,B,Life is grand
        OCEA0002,2000,B,and so are some pianos
        OCEA0003,2001,B+
        OCEA0004,2002,B+
----
Student
        Sister, Smart
Program
        PhD
Scholarships
        Most Regal 2000
        Regal 2001
Presentations
		Something good.
		Something better.
Papers published
       Paper 1
	Paper 2
Papers submitted
        Maybe it's late
Papers in press
	One
	Two
	Three
Papers in preparation
       Open up the barnyard door
Grades
        OCEA0001,2000,A+
        OCEA0002,2000,A+
        OCEA0003,2000,A+
----
}
}

\author{Dan Kelley <Dan.Kelley@Dal.Ca>}
\keyword{package}
\examples{
library(srs)
data(srs)
summary(srs)
plot(srs)
}

\name{read.srs}
\alias{read.srs}
\title{Read student records}
\description{Read student records}
\usage{read.srs(file="fake.dat", debug=FALSE, silent=FALSE, year.start=0)}
\arguments{
  \item{file}{A connection or a character string giving the name of the
    file to load.}
  \item{debug}{Set true to print debugging information.} 
  \item{silent}{Set TRUE for this to print nothing.}
  \item{year.start}{Lets you skip some of the record.}
}
\details{
A sample input file appears below.  Students are separated by lines
containing 4 dashes.  Where you see leading white-space below, use
a single tab character.  The srs library is \strong{very} picky
about the headings, and they must appear \strong{exactly} as shown; for
example, if you write \code{Papers Published} (with the use of
an upper-case character to start the second word),
then \code{read.srs} will fail.

Items in the \dQuote{Grades} section are comma-separated, and course
names (the fourth item) are optional.  Actually, all that really matters
is the third column, which is a grade in letter form (assumed out of
A+); in a rush, just put \code{x} for the course number and the year,
e.g. a minimal grade entry is \code{x,x,A}.

Make sure to put each paper or presentation on a line by itself, or the
count will be wrong.  (Don't let your editor wrap the lines!)

\preformatted{
Student
	Rex, Elizabeth
Program
	MSc
Months
	20
Scholarships
	Regal 2000
	Regal 2001
Presentations
	How we shall be addressed
Papers published
	Call me princess
Papers submitted
	One nice paper
	And another one
Papers in press
	Call me queen
Papers in preparation
	But just call me
Grades
	OCEA0001,2000,B,Life is grand
	OCEA0002,2000,B,and so are some pianos
	OCEA0003,2000,B+
	OCEA0004,2001,B+
----
}
The line of (4 or more) minus signs ends the student record.  If there
there are other students, just add them after this line.

It makes sense to use a consistent scheme for the various fields,
e.g. perhaps scholarships should only include national scholarships,
or awards over 2000 dollars.

For an example of a two-student file, do the following
\preformatted{
  data(srs)
  write.srs(srs)
}
}
\value{ An \code{srs} object containing information about students.}

\seealso{\code{\link{summary.srs}}, \code{\link{plot.srs}}, \code{\link{write.srs}}}

\examples{
library(srs)
data(srs)
summary(srs)
}
\author{Dan Kelley \email{Dan.Kelley@Dal.Ca}}
\keyword{misc}

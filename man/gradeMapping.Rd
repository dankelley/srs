\name{gradeMapping}

\alias{gradeMapping}

\title{Numeric gradeMapping corresponding to lettering schemes}

\description{Numeric gradeMapping corresponding to lettering schemes with numbers.}

\usage{gradeMapping(scheme=c("A", "B"))}

\arguments{

  \item{scheme}{Indication of the grading scheme.  \code{"A"} gives the
      Dalhousie Faculty of Engineering percentage scheme as of 2012.
      \code{"B"} gives a GPA scheme out of 4.3.}
}

\details{This is used by \code{\link{p2l}} and \code{\link{l2p}}.}

\value{A list containing letter grades (\code{letter}), paired with the
    numerical lower limits (\code{lowerLimit}) for those grades.  For example,
    the Dalhousie Faculty of Science scheme (as of 2012) has A+ corresponding
    to percentages between 90 and 100, so the the \code{value} paired with
    \code{"A+"} is 95, for \code{scheme="A"}.  Similarly, that score maps to a
    GPA of 4.3, and that value is returned for \code{scheme="B"}.
}
    
\examples{
library(srs)
data.frame(gradeMapping())
data.frame(gradeMapping('B'))
pA <- gradeMapping("A")
pB <- gradeMapping("B")
iA <- which(pA$letter=="A+") 
iB <- which(pB$letter=="A+") 
cat("Lower limit for A+ is", pA$lowerLimit[iA], "percent or", pB$lowerLimit[iB], "points\n")
}

\author{Dan Kelley}

\references{Different academic units use different scales, even at any given
    university.  \code{scheme="B"}, the Dalhousie undergraduate standard, is
    described at section 17.1 at \url{http://ug.cal.dal.ca/ACRG.htm}.}

\keyword{misc}

\name{l2p}

\alias{l2p}

\title{Letter to point-value}

\description{Convert letter grade to point value or percent value.}

\usage{l2p(l, scheme=c("A", "B"))}

\arguments{
  \item{l}{Letter grade.}
  \item{scheme}{Select from a scheme as listed below.}
}

\details{Convert percent to letter grade using one particular scheme.  (More
    may be added later.)
    \itemize{
        \item Scheme A: Dalhousie undergraduate grade-point scheme.
        \item Scheme B: Dalhousie undergraduate percentage scheme (changes over time).
    }
}

\value{A number that is at the mid-point of the grading category for the given letter grade.}

\examples{
library(srs)
l2p(c("F","D","C-","C","C+","B-","B","B+","A-","A","A+"), scheme="B")
l2p(c("F","D","C-","C","C+","B-","B","B+","A-","A","A+"), scheme="A")
}

\author{Dan Kelley}

\keyword{misc}

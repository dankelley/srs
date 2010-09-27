\name{l2p}
\alias{l2p}
\title{Letter to point-value}
\description{
  Convert letter grade to point value or percent value.
}
\usage{
l2p(l, scale=4.3)
}
\arguments{
  \item{l}{Letter grade.}
  \item{scale}{A value indicating the type of scale, e.g. 4.3 for
               a conventional GPA-type calculation, or
               100 for percentage calculations (see below).}
}
\details{
  Convert letter grade to a "point" scale.  If scale is 4.3
  or 100, then the grades are taken from the appropriate
  column below.  For any other scale, the grades are linearly
  scaled from the second column.
  \preformatted{
  A+  4.3  97
  A   4.0  92
  A-  3.7  85
  B+  3.3  79
  B   3.0  74
  B-  2.7  69
  C+  2.3  65
  C   2.0  61
  C-  1.7  57
  D+  1.3  54
  D   1.0  52
  D-  0.7  50
  F   0.0  25
}
 NB. the second column is fairly conventional, at least for schools using the 
 "4.3" convention, but the third column is just one of many variants.  For
 example, surely a blank answer is worth less than 25% ... 
}
\value{A number, as described above.}

\examples{
library(srs)
p <- l2p("A+") # 4.3
}
\author{Dan Kelley \email{Dan.Kelley@Dal.Ca}}
\keyword{misc}

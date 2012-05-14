\name{l2p}
\alias{l2p}
\title{Letter to point-value}
\description{
  Convert letter grade to point value or percent value.
}
\usage{ l2p(l, scale=100) }
\arguments{
  \item{l}{Letter grade.}
  \item{scale}{A value indicating the type of scale, e.g. 4.3 for
               a conventional GPA-type calculation, or
               100 for percentage calculations (see below).}
}

\details{Convert letter grade to a "point" scale.  If scale is 4.3 or 100, then
    the grades are taken from the appropriate column below.  For any other
    scale, the grades are linearly scaled from the second column.

\tabular{rl}{
Percent \tab     Letter Grade \cr
90-100 	\tab     A+           \cr
85-89.9 \tab 	 A            \cr
80-84.9 \tab 	 A-           \cr
75-79.9 \tab 	 B+           \cr
70-74.9 \tab 	 B            \cr
65-69.9 \tab 	 B-           \cr
62-64.9 \tab 	 C+           \cr
58-61.9 \tab 	 C            \cr
55-57.9 \tab 	 C-           \cr
50-54.9 \tab 	 D            \cr
 0-49   \tab     F            \cr
}

}
\value{A number, as described above.}

\examples{
library(srs)
p <- l2p("A+") # 4.3
}
\author{Dan Kelley \email{Dan.Kelley@Dal.Ca}}
\keyword{misc}

\name{p2l}
\alias{p2l}
\title{percent to letter}
\description{Convert percent to letter grade.}
\usage{p2l(p, scheme="A")}
\arguments{
  \item{p}{Percent.}
  \item{scheme}{Select from a scheme as listed below.}
}
\details{Convert percent to letter grade using one particular scheme.  (More
may be added later.)

Scheme A

\tabular{rl}{
Percent \tab     Letter Grade \cr
95-100 	\tab     A+           \cr
90-94   \tab 	 A            \cr
82-89   \tab 	 A-           \cr
77-81   \tab 	 B+           \cr
72-76   \tab 	 B            \cr
67-71   \tab 	 B-           \cr
64-66   \tab 	 C+           \cr
60-63   \tab 	 C            \cr
56-59   \tab 	 C-           \cr
50-55   \tab 	 D            \cr
 0-49   \tab     F            \cr
}

Scheme B (Page 25, section 6.6.2, of GR20072008Calendar.pdf, the
Graduate-School calendar)

\tabular{rl}{
Percent \tab     Letter Grade \cr
90-100 	\tab     A+           \cr
85-89   \tab 	 A            \cr
80-84   \tab 	 A-           \cr
77-79   \tab 	 B+           \cr
73-76   \tab 	 B            \cr
70-72   \tab 	 B-           \cr
0-69    \tab 	 F            \cr
}


}
\value{A letter grade, or list of letter grades.}
\examples{
library(srs)
p <- p2l(91) # A
}
\references{I think I got this conversion table from AJB, who got it from the Maths-Stats department.}
\author{Dan Kelley \email{Dan.Kelley@Dal.Ca}}
\keyword{misc}

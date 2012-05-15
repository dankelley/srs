\name{p2l}

\alias{p2l}

\title{percent to letter}

\description{Convert percent to letter grade.}

\usage{p2l(p, scheme=c("A", "B"))}

\arguments{
    \item{p}{Percentage or grade-point value.}
    \item{scheme}{Select from a scheme as listed below.}
}

\details{Convert percent to letter grade using one particular scheme.  (More
    may be added later.)
    \itemize{
        \item Scheme A: Dalhousie undergraduate percentage scheme.
        \item Scheme B: Dalhousie undergraduate grade-point scheme.
    }
}

\value{A letter grade, or list of letter grades.}

\examples{
    library(srs)
    p <- p2l(91) # A
}

\seealso{\code{\link{gradeMapping}}.}

\keyword{misc}

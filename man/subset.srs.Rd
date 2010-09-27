\name{subset.srs}
\alias{subset.srs}
\title{Subset student records}
\description{Subset student records}
\usage{subset.srs(x, subset, ...)}
\arguments{
  \item{x}{An sr object.}
  \item{subset}{Subset to keep (indices).}  
  \item{...}{Passed along (well, ignored).}
}
\details{Subset student records.}
\value{An \code{srs} object containing information about students.}
\examples{
\dontrun{
library(srs)
data(srs)
sr.ranked <- sort.srs(srs, "by.gpa")
sr.top <- subset.srs(srs.ranked, 1:3)
summary(sr.top)
}
}
\author{Dan Kelley \email{Dan.Kelley@Dal.Ca}}
\keyword{misc}

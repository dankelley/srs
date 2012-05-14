\name{sortByGPA}
\alias{sortByGPA}
\title{Sort student records by GPA}
\description{Sort student records by GPA}
\usage{sortByGPA(x, decreasing=FALSE, ...)}
\arguments{
  \item{x}{An srs object.}
  \item{decreasing}{Sort direction.}
  \item{\dots}{Not used.}
}
\details{Sort student records by GPA.}
\value{An \code{srs} object containing information about students.}
\examples{
library(srs)
data(srs)
srs.s <- sortByGPA(srs)
summary(srs.s)
plot(srs.s)
}
\seealso{\code{\link{sortByName}}, \code{\link{read.srs}}, \code{\link{plot.srs}}}
\author{Dan Kelley \email{Dan.Kelley@Dal.Ca}}
\keyword{misc}

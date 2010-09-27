\name{sort.by.GPA}
\alias{sort.by.GPA}
\title{Sort student records by GPA}
\description{Sort student records by GPA}
\usage{sort.by.GPA(x, decreasing=FALSE, ...)}
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
srs.s <- sort.by.GPA(srs)
summary(srs.s)
plot(srs.s)
}
\seealso{\code{\link{sort.by.name}}, \code{\link{read.srs}}, \code{\link{plot.srs}}}
\author{Dan Kelley \email{Dan.Kelley@Dal.Ca}}
\keyword{misc}

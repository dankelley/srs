\name{sortByName}
\alias{sortByName}
\title{Sort student records by name}
\description{Sort student records by name}
\usage{sortByName(x, decreasing=FALSE, ...)}
\arguments{
  \item{x}{An srs object.}
  \item{decreasing}{Sort direction.}
  \item{\dots}{Not uses.}
}
\details{Sort student records by name.}
\value{An \code{srs} object containing information about students.}

\seealso{\code{\link{sortByGPA}}, \code{\link{read.srs}}, \code{\link{plot.srs}}}
\examples{
library(srs)
data(srs)
srs.s <- sortByName(srs)
summary(srs.s)
plot(srs.s)
}
\author{Dan Kelley \email{Dan.Kelley@Dal.Ca}}
\keyword{misc}

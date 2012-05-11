\name{summary.srs}
\alias{summary.srs}
\title{Summarize student records}
\description{Summarize student records }
\usage{\method{summary}{srs}(object, ...)}
\arguments{
  \item{object}{An \code{srs} object.}
  \item{...}{Extra options handed below.}
}
\details{Summarize student records.}
\value{None.}
\seealso{\code{\link{sort.by.GPA}}, \code{\link{sort.by.name}}, \code{\link{read.srs}}, \code{\link{plot.srs}}}
\examples{
library(srs)
data(srs)
summary(srs)
}
\author{Dan Kelley \email{Dan.Kelley@Dal.Ca}}
\keyword{misc}

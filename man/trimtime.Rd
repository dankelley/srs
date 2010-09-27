\name{trimtime}
\alias{trimtime}
\title{Trim student records to certain year intervals}
\description{Trim student records to certain year intervals}
\usage{trimtime(x, start.times, stop.times)}
\arguments{
  \item{x}{An srs object.}
  \item{start.times}{A list of start-years, one per student.  Grades earlier than this year will be discarded.}
  \item{stop.times}{A list of top-years.  (If empty, the stop year is taken to be far in the future.)}
}
\details{This is useful for e.g. trimming undergraduate grades from a record.}
\seealso{\code{\link{subset.srs}}}
\examples{
library(srs)
data(srs)
summary(srs)
x <- trimtime(srs, start.times=c(2001,2001))
summary(x)
}
\value{An \code{sr} object containing information about students.}
\author{Dan Kelley \email{Dan.Kelley@Dal.Ca}}
\keyword{misc}

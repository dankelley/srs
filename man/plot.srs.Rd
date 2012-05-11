\name{plot.srs}
\alias{plot.srs}
\title{Plot student records}
\description{Plot student records}
\usage{\method{plot}{srs}(x, gleft=1.7, skip.presentations=FALSE, skip.inprep=TRUE,
skip.communications=FALSE, jitter=FALSE, colours=c("blue"), quiet=TRUE, ...)}
\arguments{
\item{x}{An srs object.}
\item{gleft}{GPA score at left of plot, e.g. 2.0 is C.  Use this if there are some very low grades.}
\item{skip.presentations}{If true, skip them.}
\item{skip.inprep}{If true, skip them.}
\item{skip.communications}{If TRUE, skip them.}
\item{jitter}{Set to TRUE to jitter the grades; otherwise, spread them
  on the lines.}
\item{colours}{List of colours to use for names, ...}
\item{quiet}{Set to FALSE to get some debugging information.}
\item{...}{Extra options handed below.}
}
\details{Plot student records.}
\value{An \code{srs} object containing information about students.}

\seealso{\code{\link{read.srs}} to read records.}
\examples{
library(srs)
data(srs)
plot(srs)
}
\author{Dan Kelley \email{Dan.Kelley@Dal.Ca}}
\keyword{misc}

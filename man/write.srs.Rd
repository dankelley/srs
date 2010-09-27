\name{write.srs}
\alias{write.srs}
\title{Write an srs data object as a plain-text file}
\description{Write an srs data object as a plain-text file.}
\usage{write.srs(srs, con=stdout())}
\arguments{
  \item{srs}{An \code{srs} object, e.g. as read by \code{\link{read.srs}}.}
  \item{con }{A filename or a connection.  If missing, the output is written to the screen.}
}
\details{Writes a file in a plain-text format that can be read by
  \code{\link{read.srs}}.
}
\seealso{\code{\link{read.srs}}, \code{\link{summary.srs}}}
\examples{  
library(srs)
data(srs)
write.srs(srs, "~/grades.dat")     
}
\author{Dan Kelley \email{Dan.Kelley@Dal.Ca}}
\keyword{misc}

\name{midPci}
\alias{midPci}
\title{
mid-P confidence interval adaptation of the Clopper-Pearson interval
}
\description{
mid-P confidence interval adaptation of the Clopper-Pearson interval
}
\usage{
midPci(x, n, alpha)
}
\arguments{
  \item{x}{
 number of successes
}
  \item{n}{
 number of trials
}
  \item{alpha}{
    Level-I error, 1 - confidence coefficient
  }
}
\value{
  \item{lowlim }{lower confidence bound}
  \item{uplim }{upper confidence bound}
}
\references{
  Agresti, A., Gottard, A. (2005): Randomized confidence intervals and
  the mid-P approach, discussion of article by C. Geyer and G. Meeden,
  Statistical Science, vol. 20, pp. 367-371
}
\examples{
midPci(x = 15, n = 112, alpha = 0.05)
  }
\keyword{ htest }

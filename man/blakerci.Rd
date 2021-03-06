\name{blakerci}
\alias{blakerci}
\title{
  Blaker's exact CI for a binomial proportion
}
\description{
  Blaker's exact CI for a binomial proportion
}
\usage{
blakerci(x, n, conf.level, tolerance=1e-05)
}
\arguments{
  \item{x}{Number of successes}
  \item{n}{Total sample size}
  \item{conf.level}{Confidence level}
  \item{tolerance}{default tolerance}
}
\value{
  A list with class '"htest"' containing the following components:
  \item{conf.int }{The confidence intervall for the proportion}
}
\references{
  Blaker, H. (2000). Confidence curves and improved exact confidence
  intervals for discrete distributions, \emph{Canadian Journal of Statistics}
  28 (4), 783--798
}
\keyword{ htest }

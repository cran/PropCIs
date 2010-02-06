\name{blakerci}
\Rdversion{0.1-1}
\alias{blakerci}
\title{
  Blaker's exact CI for a binomial proportion
}
\description{
  Blaker's exact CI for a binomial proportion
}
\usage{
blakerci(x, n, level, tolerance=1e-05)
}
\arguments{
  \item{x}{Number of successes}
  \item{n}{Total sample size}
  \item{level}{Confidence level}
  \item{tolerance}{default tolerance}
}
\value{
  \item{lower }{lower confidence bound}
  \item{upper }{upper confidence bound}
}
\references{
  Blaker, H. 2000. Confidence curves and improved exact confidence
  intervals for discrete distributions, Canadian Journal of Statistics,
  Vol: 28, iss: 4, pg: 783-798
}
\keyword{ htest }

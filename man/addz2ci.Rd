\name{addz2ci}
\alias{addz2ci}
\title{
  Agresti-Coull CI for a binomial proportion
}
\description{
  Agresti-Coull CI for a binomial proportion based on adding \eqn{z^2/2}
  successes and \eqn{z^2/2} failures before computing the Wald CI. The CI is
  truncated, when it overshoots the boundary.
}
\usage{
addz2ci(x, n, conf.level)
}
\arguments{
  \item{x}{
    number of successes
}
  \item{n}{
    number of trials
}
  \item{conf.level}{
    confidence coefficient
}
}
\value{
  A list with class "htest" containing the following components:
  \item{conf.int }{a confidence interval for the difference in proportions.}
  \item{estimate }{estimated \eqn{\hat{p}}}
}
\references{
  Agresti, A., Coull, B. (1998): Approximate is better than exact for
  interval estimation of binomial proportions. The American Statistician
}
\examples{
addz2ci(x = 15, n = 112, conf.level = 0.95)
}
\keyword{ htest }

\name{add4ci}
\alias{add4ci}
\title{
Agresti-Coull add-4 CI for a binomial proportion, based on adding 2
successes and 2 failures before computing the Wald CI
}
\description{
Agresti-Coull add-4 CI for a binomial proportion, based on adding 2
successes and 2 failures before computing the Wald CI. The CI is
truncated, when it overshoots the boundary
}
\usage{
add4ci(x, n, conflev)
}
\arguments{
  \item{x}{
 number of successes
}
  \item{n}{
 number of trials
}
  \item{conflev}{
 confidence coefficient
}
}
\value{
  \item{ll }{lower confidence bound}
  \item{ul }{upper confidence bound}
}
\references{
  Agresti, A., Coull, B. (1998): Approximate is better than exact for interval estimation of binomial proportions. The American Statistician

  Agresti, A., Caffo, B.(2000): Simple and effective confidence intervals for proportions and difference of proportions result from adding two successes and two failures, The American Statistician
}
\examples{
add4ci(x = 15, n = 112, conflev = 0.95)
}
\keyword{ htest }

\name{scoreci.mp}
\alias{scoreci.mp}
\title{
  Tango's score confidence interval for a difference of proportions with
  matched pairs}
\description{
  Tango's score confidence interval for a difference of proportions with
  matched pairs
 }
\usage{
scoreci.mp(b, c, n, conflev)
}
\arguments{
  \item{b}{off-diagonal count}
  \item{c}{off-diagonal count}
  \item{n}{sample size}
  \item{conflev}{confidence coefficient}
}
\value{
  \item{ll }{lower confidence bound}
  \item{ul }{upper confidence bound}
}
\references{
  A. Agresti and Y. Min, 2004. Improved confidence intervals for
  comparing matched proportions, Statistics in Medicine

  Tango T. Equivalence test and confidence interval for the difference in proportions for the paired-sample design. Statistics in Medicine. 1998; 17: 891-908
}
\examples{scoreci.mp(b = 40, c = 20, n = 160, conflev = 0.95)}
\keyword{ htest }

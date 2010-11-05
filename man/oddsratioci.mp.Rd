\name{oddsratioci.mp}
\alias{oddsratioci.mp}
\title{
  Adapted binomial score confidence interval for the subject-specific
  odds ratio with matched pairs
}
\description{
  Adapted binomial score confidence interval for the subject-specific
  odds ratio with matched pairs. This uses the Wilson score CI for a
  binomial parameter with the off-diagonal counts.
}
\usage{
oddsratioci.mp(b, c, conflev)
}
\arguments{
  \item{b}{off-diagonal count}
  \item{c}{off-diagonal count}
  \item{conflev}{confidence coefficient}
}
\value{
  \item{ll }{lower confidence bound}
  \item{ul }{upper confidence bound}
}
\references{
  A. Agresti and Y. Min, (2005) Simple improved confidence intervals for
  comparing matched proportions. \emph{Statistics in Medicine} 24 (5), 729--740.
}
\examples{
oddsratioci.mp(b = 40, c = 20, conflev = 0.95)
}
\keyword{ htest }

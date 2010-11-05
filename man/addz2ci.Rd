\name{addz2ci}
\alias{addz2ci}
\title{
  Agresti-Coull CI for a binomial proportion based on adding z^2/2
  successes and z^2/2 failures before computing the Wald CI
}
\description{
  Agresti-Coull CI for a binomial proportion based on adding z^2/2
  successes and z^2/2 failures before computing the Wald CI. The CI is
  truncated, when it overshoots the boundary.
}
\usage{
addz2ci(x, n, conflev)
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
  Agresti, A., Coull, B. (1998): Approximate is better than exact for
  interval estimation of binomial proportions. \emph{The American
    Statistician} 52, 119--126.
}
\examples{
addz2ci(x = 15, n = 112, conflev = 0.95)
}
\keyword{ htest }

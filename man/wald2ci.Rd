\name{wald2ci}
\alias{wald2ci}
\title{
  Wald interval for difference in proportions and independent samples
}
\description{
  Wald interval for difference in proportions and independent samples \eqn{p1 - p2}
}
\usage{
wald2ci(x1, n1, x2, n2, conf.level)
}
\arguments{
  \item{x1}{
    success counts in sample 1
  }
  \item{n1}{
    sample size in sample 1
  }
  \item{x2}{
    success counts in sample 2
  }
  \item{n2}{
    sample size in sample 2
  }
  \item{conf.level}{
    confidence coefficient
  }
}
\details{
  Add 1 to \eqn{x_1} and \eqn{x_2} and add 2 to \eqn{n_1} and \eqn{n_2} to get Agresti-Caffo adjusted CI (American Statistician, 2000)
}
\value{
  \item{ll }{lower confidence limit}
  \item{ul }{upper confidence limit}
}
\references{
  Agresti, A. 2002. Categorical Data Analysis. Wiley, 2nd Edition.
}
\keyword{ htest }

\name{orscoreci}
\Rdversion{0.1-1}
\alias{orscoreci}
\title{
  score confidence interval for an odds ratio in a 2x2 table [p1(1-p1)/(p2(1-p2))]

}
\description{
  score confidence interval for an odds ratio in a 2x2 table [p1(1-p1)/(p2(1-p2))]}
\usage{
orscoreci(x1, n1, x2, n2, conflev)
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
  \item{conflev}{
    confidence coefficient
  }
}
\value{
  \item{ll }{lower confidence limit}
  \item{ul }{upper confidence limit}
}
\references{
  Cornfield 1956

  Miettinen OS, Nurminen M. Comparative analysis of two
  rates. Statistics in Medicine 1985;4:213-226.

  Agresti, A. 2002. Categorical Data Analysis. Wiley, 2nd Edition.
}
\keyword{ htest }

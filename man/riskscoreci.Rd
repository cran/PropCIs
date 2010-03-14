\name{riskscoreci}
\alias{riskscoreci}
\title{
  score confidence interval for a relative risk in a 2x2 table
}
\description{
  score confidence interval for a relative risk in a 2x2 table
}
\usage{
riskscoreci(x1, n1, x2, n2, conf.level)
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
\value{
  \item{ll }{lower confidence limit}
  \item{ul }{upper confidence limit}
}
\references{
  Koopman PAR. Confidence limits for the ratio of two binomial
  proportions. Biometrics 1984;40:513-517

  Miettinen OS, Nurminen M. Comparative analysis of two
  rates. Statistics in Medicine 1985;4:213-226.

  Nurminen, 1986. Analysis of trends in proportions with an ordinally
  scaled determinant. Biometrical J. v28. 965-974

  Agresti, A. 2002. Categorical Data Analysis. Wiley, 2nd Edition.
}
\keyword{ htest }

\name{diffscoreci}
\alias{diffscoreci}
\title{
  Score interval for difference of proportions
}
\description{
  Score interval for difference of proportions and independent samples (p1 - p2)
}
\usage{
diffscoreci(x1, n1, x2, n2, conf.level)
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
  Agresti, A. 2002. Categorical Data Analysis. Wiley, 2nd Edition.

  Mee RW. Confidence bounds for the difference between two
  probabilities. Biometrics 1984;40:1175-1176.

  Miettinen OS, Nurminen M. Comparative analysis of two
  rates. Statistics in Medicine 1985;4:213-226.

  Nurminen, 1986. Analysis of trends in proportions with an ordinally
  scaled determinant. Biometrical J. v28. 965-974
}
\keyword{ htest }

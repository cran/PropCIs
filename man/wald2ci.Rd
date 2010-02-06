\name{wald2ci}
\Rdversion{0.1-1}
\alias{wald2ci}
\title{
  Wald interval for difference in proportions and independent samples (p1 - p2)
}
\description{
  Wald interval for difference in proportions and independent samples (p1 - p2)
}
\usage{
wald2ci(x1, n1, x2, n2, conflev)
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
\details{
  Add 1 to x1 and x2 and add 2 to n1 and n2 to get Agresti-Caffo adjusted CI (American Statistician, 2000)
}
\value{
  \item{ll }{lower confidence limit}
  \item{ul }{upper confidence limit}
}
\references{
  Agresti, A. 2002. Categorical Data Analysis. Wiley, 2nd Edition.
}
\keyword{ htest }

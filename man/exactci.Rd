\name{exactci}
\alias{exactci}
\title{
  Clopper-Pearson exact CI
}
\description{
  Clopper-Pearson exact CI
}
\usage{
exactci(x, n, conflev)
}
\arguments{
  \item{x}{Number of successes}
  \item{n}{Total sample size}
  \item{conflev}{Confidence level}
}
\value{
  \item{ll }{lower confidence bound}
  \item{ul }{upper confidence bound}
}
\references{
  Clopper, C. and Pearson, E.S. (1934) The use of cenfidence or fiducial
  limits illustrated in the case of the binomial. \emph{Biometrika} 26, 404--413. 
}
\keyword{ htest }


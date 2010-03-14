\name{exactci}
\alias{exactci}
\title{
  Clopper-Pearson exact CI
}
\description{
  Clopper-Pearson exact CI
}
\usage{
exactci(x, n, conf.level)
}
\arguments{
  \item{x}{Number of successes}
  \item{n}{Total sample size}
  \item{conf.level}{Confidence level}
}
\value{
  \item{ll }{lower confidence bound}
  \item{ul }{upper confidence bound}
}
\examples{
exactci(x = 15, n = 112, conf.level = 0.95)
}
\keyword{ htest }


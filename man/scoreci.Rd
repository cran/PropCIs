\name{scoreci}
\alias{scoreci}
\title{Wilson's confidence interval for a single proportion
}
\description{Wilson's confidence interval for a single proportion. Score CI based on inverting the asymptotic normal test
  using the null standard error}
\usage{
scoreci(x, n, conf.level)
}
\arguments{
  \item{x}{Number of successes}
  \item{n}{Total sample size}
  \item{conf.level}{Confidence level}
}
\value{
  \item{lowlim }{Lower endpoint for the \eqn{100*conf.level} interval}
  \item{uplim }{Upper endpoint for the \eqn{100*conf.level} interval}
}
\references{
  Wilson, E.B. 1927. Probable inference, the law of succession, and
  statistical inference. J. Amer. Stat. Assoc. 22:209-212
}
\keyword{ htest }

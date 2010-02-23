\name{scoreci}
\Rdversion{0.1-1}
\alias{scoreci}
\title{Wilson's confidence interval for a single proportion
}
\description{Wilson's confidence interval for a single proportion. Score CI based on inverting the asymptotic normal test
  using the null standard error}
\usage{
scoreci(x, n, conflev)
}
\arguments{
  \item{x}{Number of successes}
  \item{n}{Total sample size}
  \item{conflev}{Confidence level}
}
\value{
  \item{lowlim }{Lower endpoint for the 100*conflev interval}
  \item{uplim }{Upper endpoint for the 100*conflev interval}
}
\references{
  Wilson, E.B. 1927. Probable inference, the law of succession, and
  statistical inference. J. Amer. Stat. Assoc. 22:209-212
}
\keyword{ htest }

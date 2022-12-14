% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RSA_wrapper.R
\name{RSAGenerateKey}
\alias{RSAGenerateKey}
\title{RSAGenerateKey}
\usage{
RSAGenerateKey(p = NULL, q = NULL)
}
\arguments{
\item{p}{An integer value that is prime.}

\item{q}{An integer value that is prime and unique to p. If p and q are NULL, then RSAPrimeGenerate will be called that will provide six bit prime numbers for both values.}
}
\value{
keys = (n, e, d) in which n is the key product component, e is the public key encryption component, and d is the private key decryption component
}
\description{
RSAGenerateKey
}
\examples{
RSAGenerateKey(41, 37)
}

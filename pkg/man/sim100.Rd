\encoding{UTF-8}

\name{testdata}

\alias{sim100}

\docType{data}

\title{ Simulated Test Data }

\description{
An object created by \code{\link{coenoflex}} with 100 plots and 200 species for testing purposes.
}

\format{
Vegsoup object.
}

\usage{
data(sim100)
}

\examples{
# the R-forge build machine get's trapped by unpredictable errors
# when calling the coenoflex function (implemented in Fortran) from package coenoflex
# so we use this fixed output instead of a call coenoflex(100, 200) in the examples.

data(sim100)
sim100
}

\keyword{ datasets }

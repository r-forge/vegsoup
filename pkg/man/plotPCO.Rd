\name{plotPCO}
\alias{plotPCO}

\title{
Plot Principal Coordinates
}

\description{
The function uses \code{\link[vegan:capscale]{capscale}} to compute the ordination.
}
\usage{
plotPCO(x, display = "sites", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ x }{
  A \code{Vegsoup*} obejct
  }
  \item{ display }{
  See \code{\link[vegan:ordiplot]{ordiplot}}.
  }
  \item{ \dots }{
  \code{thresh} argument passed to \code{\link{outlier}}.
}
}
\author{ Roland Kaiser }

\examples{
library(vegsoup)

data(testdata)
dta <- Vegsoup(spc, sts, txa, coverscale = "braun.blanquet")

plotPCO(dta, thresh = 0.2)
}

\keyword{ plot }

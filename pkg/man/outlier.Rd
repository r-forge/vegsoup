\name{outlier}
\docType{methods}
\alias{outlier}
\alias{outlier-methods}

\title{ Outlier Detection }
\description{
  Identifies outliers based on mean inter-plot dissimilarity or nearest neighbour criterion.
  Nearest neighbours can 
}
\usage{
## S4 method for signature 'Vegsoup*'
outlier(obj, type = c("mccune", "wildi"), thresh = 0.2, ...)
}
\arguments{
  \item{ obj }{
    A \code{Vegsoup*} object.
  }
  \item{ type }{
    See \sQuote{Details}.
  }
  \item{ thresh }{
    Threshold nearest neighbour distance for outliers. If specified and argument
    \code{type} is missing it triggers type \code{"wildi"}.
  }
  \item{ \dots }{
    Not used.
  }
}
\details{
   For type \code{"mccune"}, the default, average Bray-Curtis dissimilarity of an
   outlier plot to other plots is greater than two standard deviations from the mean inter-plot
   dissimilarity (McCune & Grace 2002). For type \code{"wildi"} a threshold value
   has to be specified (Wildi 2013).
}
\value{
  Returns a logical vector equal to the number of plots where outlieres are \code{TRUE}.
  If \code{type="wildi"} nearest neighbour distances are given as attribute. See
  \sQuote{Examples}.
}
\author{
  Roland Kaiser using code from function \code{outlier} of package \pkg{dave}.
}    
\references{
McCune, B. & Grace, J.B. 2002. Analysis of ecological communities. MjM Software design. Gleneden Beach OR, US.

Wildi, O. 2013. Data Analysis in Vegetation Ecology. 2nd ed. Wiley-Blackwell, Chichester.	
}
\seealso{
\code{\link{plotPCO}}, \code{\link[dave:outlier]{outlier}} in \pkg{dave}.
}
\examples{
library(vegsoup)
data(testdata)
dta <- Vegsoup(spc, sts, txa, "braun.blanquet")

(res1 <- outlier(dta))
(res2 <- outlier(dta, type = "wildi"))

# access nearest neighbour distances 
attr(res2, "distances")

# argument thresh triggers type "wildi"
res3 <- outlier(dta, thresh = 0.3)

# drop outliers
dim(dta)
dim(dta[!res3, ])

# outliers marked in diagnostic plots
plotPCO(dta)
plotPCO(dta, thresh = 0.2)

}
\keyword{methods}

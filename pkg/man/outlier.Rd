\name{outlier}
\docType{methods}
\alias{outlier}
\alias{outlier-methods}

\title{ Outlier Detection }
\description{
Identifies outliers based on the nearest neighbour criterion.
}
\arguments{
  \item{ obj }{
    a \code{Vegsoup*} object.
  }
  \item{ thresh }{
    threshold nearest neighbour distance for outliers.
  }
}
\details{
   The methods returns a logical vector equal to the number of plots
   where outlieres are \code{TRUE}.
   The resulting objects can be directly used to take a subset with
   outlier(s) removed. Nearest neighbour distances are given as attribute.
   See \sQuote{Examples}.
}
\author{
Implemented for vegsoup using code from function \code{outlier} of package \pkg{dave}.
}    
\references{
Wildi, O. 2013. Data Analysis in Vegetation Ecology. 2nd ed. Wiley-Blackwell, Chichester.	
}
\seealso{
\code{\link{plotPCO}}, \code{\link[dave:outlier]{outlier}} in \pkg{dave}.
}
\examples{
library(vegsoup)
data(testdata)
dta <- Vegsoup(spc, sts, txa, "braun.blanquet")

(res <- outlier(dta))

# access nearest neighbour distances 
attr(res, "distances")

dim(dta)
dim(dta[!outlier(dta), ])
dim(dta[!outlier(dta, thresh = 0.3), ])

plotPCO(dta, thresh = 0.3)
}
\keyword{methods}

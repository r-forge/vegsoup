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
    A \code{"Vegsoup*"} object.
  }
  \item{ thresh }{
    Threshold nearest neighbour distance for outliers.
  }
}
\details{
   The methods returns a logical vector where outlieres are \code{TRUE}.
   The resulting objects can be directly used to take a subset with
   outlier(s) removed. See \sQuote{Examples}.
}
\author{
Implemented for vegsoup using Otto Wildis \code{outlier} function of package \pkg{dave}.
}    
\references{
Wildi, O. 2013. Data Analysis in Vegetation Ecology. 2nd ed. Wiley-Blackwell, Chichester.	
}
\seealso{
\code{\link[dave:outlier]{outlier}} in \pkg{dave}.
}
\examples{
library(vegsoup)
data(testdata)
dta <- Vegsoup(spc, sts, txa, "braun.blanquet")
outlier(dta)

dim(dta)
dim(dta[!outlier(dta), ])
dim(dta[!outlier(dta, thresh = 0.3), ])
}
\keyword{methods}

\name{coldiss}

\alias{coldiss}
\alias{coldiss-method}

\title{
Color Plots of a Dissimilarity Matrix
}
\description{
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
}
\usage{
heatmap(obj, colors, byrank = TRUE, diag = FALSE, ...)
}
\arguments{
  \item{obj}{
An object of class \code{VegsoupData*}.
}
  \item{colors}{
The number of colors, or a vector of colors. See \sQuote{Details}.
}                          
  \item{byrank}{
Boolean, if \code{TRUE} create equal-sized categories, if \code{FALSE} find equal-length intervals.
}                          
  \item{diag}{
Boolean, if \code{TRUE} annotae the diagonal of the colored matrix.
}                          
  \item{\dots}{
Not implemented.
}                          

}
\details{
If \code{colors} is a single integer, the argument determines the number of colors to be used with \code{\link{cm.colors}}. If \code{colors} is vector of colors longer than 1 those colors will be used. The function uses \code{\link{dmat.color}} and \code{\link{order.single}} in package \pkg{gclus}. 
}
\author{
Implemented from Roland Kaiser for vegsoup from code in \code{colldiss.R} by Francois Gillet supplied with the Book Numerical Ecology with Rm (Borcard et al. 2011)
}
\references{
Borcard, D., Gillet, F., and Legendre, P. (2011). Numerical Ecology with R. Springer.
}
\examples{

library(vegsoup)
data(testdata)

dta <- VegsoupData(Vegsoup(species, sites, taxonomy,
		scale = list(scale = "Braun-Blanquet")))

coldiss(dta, 4)

require(RColorBrewer)


cols <- colorRampPalette(brewer.pal(11, "Spectral"))

coldiss(dta, cols(20), byrank = FALSE, diag = TRUE)
}
\keyword{datasets}

\name{coldiss}

\alias{coldiss}
\alias{coldiss-method}

\title{
Plots of the Dissimilarity Matrix of an Object of class VegsoupData*
}
\description{
The method returns two plots corresponding to an unordered and an ordered colored matrix of dissimilarities. 
}
\usage{
heatmap(obj, colors, byrank = TRUE, diag = FALSE, ordered.only = FALSE, translate = FALSE, ...)
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
  \item{ordered.only}{
Only display the ordered dissimilarity matrix. Defaults to \code{FALSE}.
}
  \item{translate}{
Dispaly scientific taxon names as defined in \code{Taxonomy(obj)}. Defaults to \code{FALSE} and only applies if additional argument \code{mode} is supplied and it's value is \code{"R"}, setting up a \emph{R mode} analysis. See \sQuote{Examples}.
}
  \item{\dots}{
Arguments passed to \code{as.dist}. See \sQuote{Examples}.
}                          

}
\details{
If \code{colors} is a single integer, the argument determines the number of colors to be used with \code{\link{cm.colors}}. If \code{colors} is vector of colors longer than 1 those colors will be used. The function uses \code{\link{dmat.color}} and \code{\link{order.single}} in package \pkg{gclus}.
%Combinig \code{translate = TRUE} and \code{diag = TRUE} will result in over-plotting.
}
\author{
Implemented from Roland Kaiser for vegsoup from code in \code{colldiss.R} by Francois Gillet supplied with the Book Numerical Ecology with Rm (Borcard et al. 2011)
}
\references{
Borcard, D., Gillet, F., and Legendre, P. (2011). Numerical Ecology with R. Springer.
}
\seealso{
\code{\link{dmat.color}} in package \pkg{gclus}
}
\examples{

library(vegsoup)
data(testdata)

dta <- VegsoupData(Vegsoup(species, sites, taxonomy,
		scale = list(scale = "Braun-Blanquet")))

coldiss(dta) # default


# other dissimilarity
vegdist(dta) <- "bray"
coldiss(dta, 4)

# more colors
require(RColorBrewer)

cols <- colorRampPalette(brewer.pal(11, "Spectral"))

coldiss(dta, cols(4), byrank = FALSE, diag = TRUE)

# R-mode analysis, see ?as.dist
# first collapse layers
dta.ol <- Layers(dta, aggregate = "layer",
	collapse = "ol", verbose = TRUE)
decostand(dta.ol) <- "chi.square"
vegdist(dta.ol)	

# annotate scientific species names
# label only the diagonal to avoid cluttered display   
coldiss(dta.ol, rev(cols(20)), mode = "R",
	ordered.only = TRUE, translate = TRUE, diag = TRUE)
}



\keyword{datasets}

\name{coldiss}

\alias{coldiss}
\alias{coldiss-method}

\title{
Plots of the Dissimilarity Matrix of an Object of class Vegsoup*
}
\description{
The method returns two plots corresponding to an unordered and an ordered colored matrix of dissimilarities. 
}
\usage{
coldiss(obj, colors, byrank = TRUE, diag = FALSE, seriation,
        ordered.only = FALSE, translate = FALSE, ...)
}
\arguments{
  \item{ obj }{
An object of class \code{Vegsoup*}.
}
  \item{ colors }{
The number of colors, or a vector of colors. See \sQuote{Details}.
}                          
  \item{ byrank }{
Boolean, if \code{TRUE} create equal-sized categories, if \code{FALSE} find equal-length intervals.
}                          
  \item{ diag }{
Boolean, if \code{TRUE} annotae the diagonal of the colored matrix.
}                          
  \item{ method }{
Method argument passed to \code{\link{seriation}}.
}                          
  \item{ ordered.only }{
Only display the ordered dissimilarity matrix. Defaults to \code{FALSE}.
}
  \item{ translate }{
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
require(vegsoup)
data(testdata)

dta <- Vegsoup(spc, sts, txa, "braun.blanquet")

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
dta.0l <- Layers(dta, aggregate = "layer",
                 collapse = "0l", verbose = TRUE)
decostand(dta.0l) <- "chi.square"
vegdist(dta.0l) 

# annotate scientific species names
# label only the diagonal to avoid cluttered display
# mode = "R" defunct due to generic dispatch of as.dist
coldiss(dta.0l, rev(cols(20)), mode = "R",
    ordered.only = TRUE, translate = TRUE, diag = TRUE)
}
\keyword{ methods }
\keyword{ hplot }

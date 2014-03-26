\encoding{UTF-8}

\name{coldiss}

\alias{coldiss}
\alias{coldiss,Vegsoup-method}

\title{
Plots of the Dissimilarity Matrix of an Object of class Vegsoup*
}
\description{
The method returns two plots corresponding to an unordered and an ordered colored matrix of dissimilarities. 
}
\usage{
\S4method{coldiss}{Vegsoup}(obj, colors, byrank = TRUE, diag = FALSE, method = "dca",
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
    Boolean, if \code{TRUE} create equal-sized categories, if \code{FALSE} find equal-length
    intervals.
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
    Dispaly scientific taxon names as defined in \code{Taxonomy(obj)}. Defaults to \code{FALSE}
    and only applies if additional argument \code{mode} is supplied and it's value is
    \code{"R"}, setting up a \emph{R mode} analysis. See \sQuote{Examples}.
  }
  \item{ \dots }{
    Arguments passed to \code{as.dist}. See \sQuote{Examples}.
  }
}

\details{
If \code{colors} is a single integer, the argument determines the number of colors to be used with \code{\link{cm.colors}}. If \code{colors} is vector of colors longer than 1 those colors will be used. The function uses \code{\link[gclus]{dmat.color}} and \code{\link[gclus:order.single]{order.single}} in package \pkg{gclus}.
%Combinig \code{translate = TRUE} and \code{diag = TRUE} will result in over-plotting.
}

\author{ Roland Kaiser, implemented for vegsoup inspired by code in \code{colldiss.R} by \emph{Francois Gillet} supplied with the Book Numerical Ecology with R (Borcard et al. 2011)
}

\references{
Borcard, D., Gillet, F., and Legendre, P. (2011). Numerical Ecology with R. Springer.
}
\seealso{
\code{\link[gclus]{dmat.color}} in package \pkg{gclus}
}
\examples{
require(vegsoup)

data(barmstein)
x <- barmstein

coldiss(x) # default

# other dissimilarity
vegdist(x) <- "bray"
coldiss(x, 4)

# more colors
require(RColorBrewer)

cols <- colorRampPalette(brewer.pal(11, "Spectral"))

coldiss(x, cols(4), byrank = FALSE, diag = TRUE)

# R-mode analysis, see ?as.dist
# first collapse layers
x.0l <- Layers(x, aggregate = "layer",
                 collapse = "0l", verbose = TRUE)
decostand(x.0l) <- "chi.square"
vegdist(x.0l) 

\dontrun{
# annotate scientific species names
# label only the diagonal to avoid cluttered display
# mode = "R" defunct due to generic dispatch of as.dist(x)
coldiss(dta.0l, rev(cols(20)), mode = "R",
    ordered.only = TRUE, translate = TRUE, diag = TRUE)
}
}
\keyword{ methods }
\keyword{ hplot }


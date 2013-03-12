\name{rectangles}
\alias{rectangles}

\title{
Create a Plot of Matrix Dimensions for Each Partition
}
\description{
Retrieves the number of species and sites for each partition and plots a box according to its dimension.
}
\usage{
rectangles(obj, plot = TRUE, ...)
}
\arguments{
  \item{ obj }{
    An object inherting from class \code{"VegsoupPartiton"}
  }
  \item{ plot }{
    If \code{TRUE} create a plot, otherwise just return a matrix of partition
    dimensons used to create boxes. Defaults to \code{TRUE}.
  }
  \item{ \dots }{
    graphical parameters. See \sQuote{Details}.
  }
}
\details{
A lying box idicates species poor and an upright box indiactes species rich partitions. Currently graphical parameters are only passed to \code{\link{rect}}. The functions adds a line to mark all partitions that conatin only one site. See argument \code{polish} in function \code{\link{VegsoupPartition}} to get rid of singletons in your analysis.
}
\value{
Returns a \code{matrix} with two columns as returned by  \code{dim(obj[Partitioning(obj) == i,])}, where \code{i} is a specific partition. The first column shows number of plots and the second column shows number of species.
}
\author{ Roland Kaiser }
\note{
Any warings that typically arise when a single partition is subsetted from an object are silenced. These warnings are harmless and not of interessent at this point.
}
\examples{
data(bigtestdata)
dta <- Vegsoup(spc.big, sts.big, txa.big,
               coverscale = "braun.blanquet")

prt <- VegsoupPartition(dta, k = 20)
rectangles(prt)
#   add color to the boxes
rectangles(prt, col = rgb(0,0,0,0.2))
#   returned matrix only
res <- rectangles(prt, plot = FALSE)
res

# use polish = TRUE
prt <- VegsoupPartition(dta, k = 10, polish = TRUE)

#   add fancy colors to the boxes
require(RColorBrewer)
rectangles(prt, plot = TRUE,
           col = rgb(0,0,0,0.2),
           border = brewer.pal(getK(prt), "Spectral"),
           lwd = 2)
}
\keyword{ methods }
\keyword{ hplot }

\name{rectangles}
\alias{rectangles}

\title{
Create a Plot of Matrix Dimensions for Each Partition
}
\description{
Retrieves the number of species and sites for each partition and plots a box according to its dimension.
}
\usage{
rectangles(obj, plot = TRUE)
}
\arguments{
\item{obj}{
An object inherting from class \code{VegsoupDataPartiton}
}
\item{plot}{
If \code{TRUE} create a plot, otherwise just return a matrix of partition dimensons used to create boxes.
}
\item{\dots}{
graphical parameters, see details
}
}
\details{
A lying box idicates species poor and an upright box indiactes species rich partitions. Currently graphical parameters are only passed to \code{\link{rect}}. The functions adds a line to mark all partitions that conatin only one site. See argument \code{polish} in function \code{\link{VegsoupDataPartition}} to get rid of singletons in your analysis.
}
\value{
Returns a \code{matrix} with two columns as returned by  \code{dim(obj[Partitioning(obj) == i,])}, where \code{i} is a specific partition. The first column shows number of plots and the second column shows number of species.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
Any warings that typically arise when a single partition is subsetted from an object are silenced. These warnings are harmless and not of interessent at this point.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
data(bigtestdata)
dta <- VegsoupData(Vegsoup(species.big, sites.big, taxonomy.big,
	scale = list(scale = "Braun-Blanquet")))

prt <- VegsoupDataPartition(dta, k = 10)
rectangles(prt)
#	add color to the boxes
rectangles(prt, col = rgb(0,0,0,0.2))
#	returned matrix only
res <- rectangles(prt, plot = FALSE)
res

#	use polish = TRUE
prt <- VegsoupDataPartition(dta, k = 10, polish = TRUE)

#	add fancy colors to the boxes
require(RColorBrewer)
rectangles(prt, plot = TRUE,
	col = rgb(0,0,0,0.2),
	border = brewer.pal(getK(prt), "Spectral"),
	lwd = 2)
}
\keyword{ methods }
\keyword{ hplot }



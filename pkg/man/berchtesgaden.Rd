\name{ Grasslands National Park Berechtesgaden }
\alias{berchtesgaden}
\docType{data}
\title{ Grassland survey in National Park Berchtesgaden }
\description{
A survey of grasslands in the National Park Berechtesgaden region.
}
\format{
A \code{\link{Vegsoup}} object
}
\source{
Subalpine and alpine communities of wind exposed sites.

Species abundances were recorded using the Domin scale (see \code{\link{Coverscale}}).
Taxonomy of vascular plants agrees with Fischer et al. (2008),
The data set has two layers \code{hl} and  \code{sl}, where hl means herb layer and sl shrub layer.
The sites data contains e.\,g. information about stand heights, cover of layers, slope. Precise sampling locations are provided.
}
\usage{
data(berchtesgaden)
}
\references{
Fischer, M., Oswald, K., and Adler, W. (2008). Exkursionsflora für Österreich, Liechtenstein und Sudtirol, Volume 3. Biologiezentrum der Oberösterreichischen Lansdesmuseen.
}
\seealso{
\code{\link{testdata}}
}
\examples{
data(berchtesgaden)
}
\keyword{datasets}
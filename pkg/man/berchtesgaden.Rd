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
Grassland in the National Park Berechtesgaden spanning montane and alpine communities.
Species abundances were recorded using the Domin scale (see \code{\link{Coverscale}}).
Taxonomy of vascular plants agrees with Fischer et al. (2008).
The data set has two layers \code{hl} and  \code{sl}, where hl means herb layer and sl shrub layer.
The sites data contains e.\,g. information about stand heights, cover of layers, slope. Precise sampling locations are provided.
}
\usage{
data(berchtesgaden)
}
\references{
Fischer, M., Oswald, K., and Adler, W. (2008). Exkursionsflora fu"r O"sterreich, Liechtenstein und Su"dtirol, Volume 3. Biologiezentrum der Obero"sterreichischen Lansdesmuseen.
}
\seealso{
\code{\link{testdata}}
}
\examples{
data(berchtesgaden)
berchtesgaden
}
\keyword{datasets}
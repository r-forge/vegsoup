\name{ Windsfeld }
\alias{ windsfeld}
\docType{data}

\title{ Small Scale Survey of Alpine Vegetation }

\description{
A survey of wind exposed alpine vegetation developed on calcareous and siliceous bedrock.
}
\format{
A \code{\link{Vegsoup}} object
}
\source{
Species abundances were estimated adopting the nine-point Braun-Blanquet scale (see \code{\link{Coverscale}}).
Taxonomy of vascular plants agrees with Fischer et al. (2008), lichens with Türk \& Hafellner 2010 and  bryophytes with Köckinger et al. (2012). On sample plot (wf60 to wf70) no cryptogams were recorded.
The data set has five layers \code{hl}, \code{ml}, \code{sl}, \code{tl1} and  \code{tl2}, where hl means herb layer, ml moos layer, sl shrub layer and tl1 lower tree layer and tl2 upper tree layer, respectively. The sites data contains e.\,g. information about stand heights, cover of layers, snow cover. Precise sampling locations are provided.
}
\usage{
data(windsfeld)
}
\references{
Fischer, M., Oswald, K., and Adler, W. (2008). Exkursionsflora für Österreich, Liechtenstein und Sudtirol, volume 3. Biologiezentrum der Oberösterreichischen Lansdesmuseen.

Türk, R. and Hafellner, J. (2010). Nachtrag zur bibliographie der Flechten in  Österreich. In Ehrendorfer, F., editor, Biosystematics and Ecology Series, volume 27. Austrian Academy of Sciences Press.

Köckinger H., Schröck C., Krisai, R. and Zechmeister H.G. (2012). Checklist of Austrian Bryophytes \href{http://131.130.59.133/projekte/moose/}{available here}
}
\seealso{
\code{\link{testdata}}
}
\examples{
data(windsfeld)
}
\keyword{datasets}
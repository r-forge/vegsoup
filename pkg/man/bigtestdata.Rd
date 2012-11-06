\name{bigtestdata}
\alias{bigtestdata}
\docType{data}
\title{Big Test Data Set with Objects Suitable for Vegsoup}
\description{
Simple data structures for testing. A species and corresponding taxonomy data.frame plus a sites data.
}
\format{
The three objects \code{"species.big"}, \code{"sites.big"} and \code{"taxonomy.big"} each contain the following columns:
\describe{
\strong{species:}
	\item{plot}{plot identifier, mode \code{"character"}}
	\item{abbr}{species names, mode \code{"character"}}
	\item{layer}{layer code, mode \code{"character"}}
	\item{cov}{species abundance, mode \code{"character"}}

\strong{sites:}	
	\item{plot}{plot identifier, mode \code{"character"}}
	\item{variable}{name of the variable, mode \code{"character"}}
	\item{value}{value for each variable, mode \code{"character"}}

\strong{taxonomy:}	
	\item{abbr}{species identifier, mode \code{"character"}}
	\item{taxon}{scientific species name, mode \code{"character"}}
}
}
\source{
Supalpine and alpine communities of wind exposed sites.
Species abundances were estimated adopting the nine-point Braun-Blanquet scale (see \code{\link{AbundanceScale}}).
Taxonomy of vascular plants agrees with Fischer et al. (2008), lichens with Türk \& Hafellner 2010 and  bryophytes with Köckinger et al. (2012). On sample plot (wf60 to wf70) no cryptogams were recorded.
The data set has five layers \code{hl}, \code{ml}, \code{sl}, \code{tl1} and  \code{tl2}, where hl means herb layer, ml moos layer, sl shrub layer and tl1 lower tree layer and tl2 upper tree layer, respectively. The sites data contains e.\,g. information about stand heights, cover of layers, snow cover. Precise sampling locations are provided.
}
\usage{
data(bigtestdata)
}
\references{
Fischer, M., Oswald, K., and Adler, W. (2008). Exkursionsflora für Österreich, Liechtenstein und Sudtirol, volume 3. Biologiezentrum der Oberösterreichischen Lansdesmuseen.

Türk, R. and Hafellner, J. (2010). Nachtrag zur bibliographie der Flechten in  Österreich. In Ehrendorfer, F., editor, Biosystematics and Ecology Series, volume 27. Austrian Academy of Sciences Press.

Köckinger H., Schröck C., Krisai, R. and Zechmeister H.G. (2012). Checklist of Austrian Bryophytes \href{http://131.130.59.133/projekte/moose/}{available here}
}
\seealso{
\code{\link{testdata}} for a small data set
}
\examples{
data(bigtestdata)
}
\keyword{datasets}
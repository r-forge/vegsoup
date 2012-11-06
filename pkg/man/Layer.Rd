\name{Layer}
\alias{Layer}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Return Layer for Each Observation of Class extending Vegsoup
}
\description{
Simply return the layer for each observation.
}
\usage{
Layer(obj, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{obj}{ a class inheriting from class \code{vegsoup}}
  \item{\dots}{ passed from to methods}
}
\details{
Simply returns the layer for each observation, equivalent to \code{SpeciesLong(obj)\$layer}.
}
\value{\code{Layer} returns a character vector equal in length to \code{nrow(SpeciesLong(obj)}}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Roland Kaiser %(\email{kardinal.eros@gmail.com}
}
\note{
The function will be renamed to avoid typos with method \code{Layers} or even dropped -- it is not very useful?.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\link{Vegsoup-class}
}
\examples{
data(bigtestdata)
dta <- VegsoupData(Vegsoup(species.big, sites.big, taxonomy.big,
	scale = list(scale = "Braun-Blanquet")))

#	see how many occurences per layer
table(Layer(dta))

#	alternatively
table(SpeciesLong(dta)$layer)
}
\keyword{methods}


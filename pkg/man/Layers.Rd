\name{Layers}
\docType{methods}
\alias{Layers}
\alias{Layers-methods}
\alias{Layers,Vegsoup-method}
\alias{Layers,VegsoupData-method}
\alias{Layers<-}
\alias{Layers<--methods}
\alias{Layers<-,Vegsoup-method}
\title{Methods for Function Layers<-}

\title{
Modify the Layer Structure of Objects of Class VegsoupData*
}
\description{
Modify an object's layer structure by suppling a layer combination vector as a character argument \code{collapse}. A layer that is intended to be collpased with one or more layers is simply given the same value of collapse. Different aggregation modes are provided.
}
\usage{
Layers(obj, collapse, aggregate = c("layer", "mean", "min", "max", "sum"), dec = 0, verbose = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{obj}{
an object inheriting from class \code{Vegsoup} or \code{VegsoupData}.
}
  \item{collapse}{
An character vector where giving a combination of layers. The order of strings matters!
}
  \item{aggregate}{
Layer aggregation mode. Possible values are: layer (default), mean, min, max and sum, see details.}

  \item{dec}{
Number of decimals if abundance scale is treated as numeric, defaults to \code{0}.
}
  \item{verbose}{
Flag to print extra output.
}
}
\details{
Aggregation mode \code{"layer"} combines layers assuming their independence. For example, a species occurring in two layers with a cover of 50 \% will result in a overall cover of 75 \%. Mode \code{"sum"} will sum up cover values of all layers (see \code{\link{tv.veg}}). If \code{collapse} is missing, or of length 1 all layers are collapsed and labeled according to the value of \code{collapse}, or \code{0l} if \code{collapse} is missing. If \code{collapse} is a character vector it has to match \code{length(Layers(obj))} (see \sQuote{Examples}).
}
\note{
If there are any \code{NAs} in \code{collapse} all species that occur only on these layers will be dropped from the data set and a warning is issued. This can be useful e.g. if you want to drop cryptogams (bryophytes and/or lichens) from a dataset that has no complete coverage for these taxa and would otherwise influence the analysis (see \sQuote{Examples}).
}
\value{
An object depending on the input class.
}
\author{
Roland Kaiser

Based on code from Florian Jansen for function \code{tv.veg} in package \pkg{vegdata}.
}

\examples{
data(bigtestdata)
dta <- VegsoupData(Vegsoup(species.big, sites.big, taxonomy.big,
	scale = list(scale = "Braun-Blanquet")))
#	print layers
Layers(dta)

#	assign wl to sl, tl1 and tl2
#	see ?bigdataset for details
dta.1 <- Layers(dta, aggregate = "layer",
	collapse = c("hl", "ml", "wl", "wl", "wl"), verbose = TRUE)
Layers(dta.1)
#	check dimension of data set
#	dim(obj)[1] gives number of sites (unique sample plots)
#	dim(obj)[2] gives number of species
dim(dta.1)

#	collpase all layers
dta.2 <- Layers(dta, aggregate = "layer",
	collapse = c("ol"), verbose = TRUE)
Layers(dta.2); dim(dta.2)

#	... another way adopting default labeling
dta.3 <- Layers(dta, aggregate = "layer",
	verbose = TRUE)
Layers(dta.3); dim(dta.3)

#	drop layer sl aswell as tl1 and tl2
dta.4 <- Layers(dta, aggregate = "layer",
	collapse = c("hl", NA, "sl", NA, NA), verbose = TRUE)
Layers(dta.4)
dim(dta.4)

#	this way we also loose some plots
dta.5 <- Layers(dta, aggregate = "layer",
	collapse = c(NA, NA, "wl", "wl", "wl"), verbose = TRUE)
Layers(dta.5)
dim(dta.5)

#	subsetting can also be used 
Layers(dta[, grep("@wl", names(dta))])
}
\seealso{
\code{\link{Layer}},
\code{\link{Vegsoup}}
and \code{\link{VegsoupData}}
}

\keyword{ methods }

\name{Layers}
\docType{methods}

\alias{Layers}
\alias{Layers,Vegsoup-method}

\alias{Layers<-}
\alias{Layers<-,Vegsoup-method}
\alias{Layers<-,Vegsoup,character-method}

\title{ Get or Modify Layers }

\description{
Modify an object's layer structure by suppling a layer combination vector as a character argument \code{collapse}. A layer that is intended to be collapsed with one or more layers is simply given the same value of collapse. Different aggregation modes are provided.
}

\usage{
\S4method{Layers}{Vegsoup}(obj, collapse, aggregate = c("layer", "mean", "min", "max", "sum"), dec = 0,
verbose = FALSE)

\S4method{Layers}{Vegsoup}(obj)<-value
}

\arguments{
  \item{ obj }{
    a \code{Vegsoup*} object.
  }
  \item{ collapse }{
    character. Vector defining a combination of layers. The order of strings
    matters!
  }
  \item{ aggregate }{
    character. Layer aggregation mode, defaults to \code{"layer"}.
  }
  \item{ dec }{
    integer. Number of decimals for calculation, defaults to \code{0}. If this
    is not appropriate for \code{coverscale(obj)}, the value is increased as
    necessary.
  }
  \item{ value }{
    character. Permutation of \code{Layers(obj)} to reorder layers.
  }
  \item{ verbose }{
    logical. Prints messages.
  }
}

\details{

A data set composed of more than one layer might be represented as a multidimensional dimensional array, where there are as many dimensions as there are layers. Unfortunately, many multivariate methods requires a matrix-valued response variable.

For practical issues, the vegsoup package collapses the species and layer dimension into a single dimension by using a special mark-up character \code{'@'}. This comes in effect if a species matrix is requested from the object. The order in which the collapsed dimension (pseudo-species) appears in the column names of the resulting matrix is controlled by the order of \code{Layers(obj)}. Permuting this reorder also orders the blocks of layers.

In stage of aggregating several data sources it might be necessary to homogenize the various layers present in the object. For this task some basic aggregation mode are provided. Aggregation mode \code{"layer"} combines layers assuming their independence. For example, a species occurring in two layers with a cover of 50 \% will result in a overall cover of 75 \%. Mode \code{"sum"} will sum up cover values of all layers (see \code{\link[vegdata:tv.veg]{tv.veg}}). If \code{collapse} is missing, or of length 1 all layers are collapsed and labeled according to the value of \code{collapse}, or \code{0l} if \code{collapse} is missing. If \code{collapse} is a character vector it has to match \code{length(Layers(obj))} (see \sQuote{Examples}).
}

\note{
The returned object is reordered by plot, abbr and layer!

If there are any \code{NAs} in \code{collapse} all species that occur only on these layers will be dropped from the data set and a warning is issued. This can be useful e.g. if you want to drop cryptogams (bryophytes and/or lichens) from a dataset that has no complete coverage for these taxa and would otherwise influence the analysis (see \sQuote{Examples}).
}
\references{
Mucina, L., Schaminee, J., and Rodwell, J. (2000). Common data standards for recording relevés in field survey for vegetation classification. Journal of Vegetation Science, 11: 769–772.
}
\value{
An object depending on the input class.
}

\author{ Roland Kaiser, impsired by code from \emph{Florian Jansen} for function \code{tv.veg} in package \pkg{vegdata}.
}

\examples{
require(vegsoup)

data(windsfeld)
x <- windsfeld

#   print layers
Layers(x)

# change order of layers
Layers(x) <- c("ml", "hl", "sl", "tl2", "tl1")
Layers(x)

# assign wl to sl, tl1 and tl2
# see ?windsfeld for details
x1 <- Layers(x, aggregate = "layer",
                collapse = c("hl", "ml", "wl", "wl", "wl"),
                verbose = TRUE)
Layers(x1)
# check dimension of data set
# dim(obj)[1] gives number of sites (unique sample plots)
# dim(obj)[2] gives number of species
dim(x1)

# collpase all layers
x2 <- Layers(x, aggregate = "layer", collapse = c("ol"),
                verbose = TRUE)
Layers(x2); dim(x2)

# ... another way adopting default labeling
x3 <- Layers(x, aggregate = "layer",
                verbose = TRUE)
Layers(x3); dim(x3)

# drop layer sl aswell as tl1 and tl2
x4 <- Layers(x, aggregate = "layer",
                collapse = c("hl", NA, "sl", NA, NA),
                verbose = TRUE)
Layers(x4)
dim(x4)

# this way we also loose some plots
x5 <- Layers(x, aggregate = "layer",
                collapse = c(NA, NA, "wl", "wl", "wl"),
                verbose = TRUE)
Layers(x5)
dim(x5)

# subsetting will also extract layers appropiately
Layers(x[, grep("@tl", colnames(x))])
}
\seealso{
\code{\link{Layer}},
\code{\link{abbr.layer}},
\code{\link{Vegsoup}}
and class \code{\linkS4class{Vegsoup}}
}

\keyword{ methods }
\keyword{ manip }


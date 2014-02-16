\name{as.matrix}

\alias{as.logical}
\alias{as.logical,Vegsoup-method}

\alias{as.numeric}
\alias{as.numeric,Vegsoup-method}

\alias{as.character}
\alias{as.character,Vegsoup-method}

\alias{as.matrix}
\alias{as.matrix,Vegsoup-method}

\alias{as.array}
\alias{as.array,Vegsoup-method}

\alias{as.vector}
\alias{as.vector,Vegsoup-method}
\alias{as.vector,Vegsoup,missing-method}

\alias{indices}
\alias{indices,Vegsoup-method}

\title{ Retrieve Species Matrix with Specified Storage Mode }

\description{
The functions retrieve a species matrix of mode according the methods name and \code{as.matrix} returns corresponding types of matrices but specified by its argument \code{mode}.
%cross-classifying factors
}

\usage{
\S4method{as.logical}{Vegsoup}(x, mode)
\S4method{as.numeric}{Vegsoup}(x, mode)
\S4method{as.character}{Vegsoup}(x, mode)
\S4method{as.matrix}{Vegsoup}(x, typeof, ...)
\S4method{as.array}{Vegsoup}(x, typeof, ...)
\S4method{indices}{Vegsoup}(x, typeof)
\S4method{as.vector}{Vegsoup}(x, mode)
}

\arguments{
  \item{ x }{
    \code{'Vegsoup'} object.
  }
  \item{ mode }{
    character. If \code{mode = "R"} return the transposed matrix. If missing, defaults to
    \code{"Q"} no transposition. Lower and upper case letters are treated the same.
    See \sQuote{Details}.
  }
  \item{ typeof }{
    character. Selectes the desired storage mode or type of the object.
    Can be one of \code{"character"}, \code{"numeric"} or \code{"logical"}.
  }
  \item{ \dots }{
    Arguments passed to \code{as.logical} etc. to supply a value for the
    argument \code{mode}. See argument \code{'mode'} and \sQuote{Details}.
  }
}

\details{
For any object inheriting from class \code{Vegsoup} the methods return an object of class \code{"matrix"} with dimnames giving plot and species names. The latter are strings where any occurrence in a layer is labeled using the layer name and the species abbreviations, collapsed with the \code{"@"} glyph (pseudo-species, see \code{\link{Layers}} and \code{\link{splitAbbr}}). The returned object is suitable as input to functions in other packages that require an object of class \code{matrix}. Dispatch is guaranteed if these functions internally coerce objects to class \code{"matrix"} by calling \code{as.matrix}. Besides that, it is possible to supply a \code{Vegsoup*} object directly to a base function (see \sQuote{Examples}). As there is no way to specify the \code{typeof} or \code{mode} arguments in a call to base function the S3 method for \code{as.matrix()} uses \code{typeof = "numeric"} which is suitable for most applications. If the user wants to ensure that presence/absence data should be returned by calls to \code{as.matrix()} inside base functions, simply assign a standardization method beforehand (e.g. \code{decostand(obj) <- "pa"}). See also \code{\link{as.data.frame}} for passing data to model-fitting and plotting functions.

\code{as.numeric} returns a matrix of mode \code{"numeric"} with species abundances coded by \code{coverscale(obj)\$lims}. Absences are zeros. Equals \code{as.matrix(x, typeof = "numeric")}.

\code{as.character} returns a matrix of mode \code{"character"} with species abundances in the data set's original scale \code{coverscale(obj)\$codes}. Absences are coded as zeros! Equals \code{as.matrix(x, typeof = "character")}.

\code{as.logical} returns a matrix of mode \code{"numeric"} with zeros coding for species absences and ones for presences. Equals \code{as.matrix(obj, mode = "logical")}.

Note, \code{as.logical} and \code{as.matrix(x, "logical")} do not return a boolean or logical matrix composed of \code{TRUE} and \code{FALSE} but use ones to code presences (\code{TRUE}) and zeros (\code{FALSE}) for absences. The \code{mode} (\code{storage.mode}) of the returned matrix is of type \code{"numeric"} (\code{"integer"}).

As defined above, the some how misleading named argument \code{mode} refers to the type of analysis intended on the returned object (recall, \code{"typeof"} determines \code{mode} (\code{storage.mode}) of the returned matrix). The argument \code{mode} can be passed to all methods and has two possible values. If \code{mode = "Q"} pairs of objects are compared and the analysis is said to be in \emph{Q mode}. This is what users usally want or expect. When pairs of descriptors are compared, the analysis is said to be in \emph{Q mode} (Legendre & Legendre 2012). For example, \code{as.matrix(x, "logical", "R")} returns a transposed presence/absence matrix. Of course, this can also be written as \code{t(as.matrix(x, "logical"))}.

A data set composed of more than one layer might also be represented as a multidimensional dimensional array, where there are as many dimensions as there are layers. Method \code{as.array} returns such an array. Note, the order in which species appear in the column names is alphabetic and there is currently no implementation of the \code{'mode'} argument!

Method \code{indices} returns the locations (row and column pointers) and values of nonzero entries in a matrix. Those indices can be used to construct a \code{\link{sparseMatrix}}. Coercion method \code{as(x, "sparseMatrix")} helps in doing so.
}
\value{
A \code{"matrix"} (or \code{"array"}) of mode \code{"numeric"} or \code{"character"}. See \sQuote{Details}.
}
\note{
If the object has non \code{NULL} values in its data standardization slot the method defined by \code{decostand(obj)} applies!
}

\author{ Roland Kaiser }

\references{
Legendre, P. and Legendre, L. (2012). Chapter 7 in Numerical ecology. Developments in environmental modeling, 24.
}
\seealso{
\code{\link{as.data.frame}},
\code{\link{Vegsoup}},
\code{\link{decostand}} and
\code{\link{Coverscale}}
}
\examples{
require(vegsoup)
data(barmstein)

x <- barmstein

# presence/absence representation
x.pa <- as.logical(x)
# the same results can be achieved using the classes decostand property
decostand(x) <- "pa"

# note, results of as.numeric() and as.logical()
# are equal if presence/absence (transformation) is applied
x.pa2 <- as.numeric(x)

# for a true boolean matrix change mode
mode(x.pa) <- "logical"
head(x.pa)
all.equal(x.pa, x.pa2, check.attributes = FALSE)

# using as.matrix and argument mode
as.matrix(x, typeof = "character", mode = "R")

# compact display
as(as.matrix(x, "logical", "R"), "sparseMatrix")

pro <- protest(as.logical(x), as.numeric(x))

# objects of class Vegsoup used in arguments of base functions
# where an object of class matrix is expected
decostand(x) <- "hellinger"
decorana(x)
decostand(x) <- NULL # reset the default

# species matrix representations and memory demands
# class "matrix"
object.size(as.matrix(x))          # most efficient
# class "list" (pointers and values of all non zero entries)
object.size(indices(x))            # almost equal
# coercion to class "sparseMatrix" is based on method indices
object.size(as(x, "sparseMatrix")) # efficient
# the long format as stored in Vegsoup* objects (slot 'species')
object.size(Species(x))            # most general but least efficent
# a multi-dimensional array of layer replicates
object.size(as.array(x))           # least efficent

# comparison of memory demands of objects of differnt classes
object.size(as(x, "data.list"))  # least efficent, but highly generic
object.size(as.data.frame(x))    # also efficent
sp.object.size <- object.size(x@sp.points) + object.size(x@sp.polygons) # for comparison subtract sp slots
object.size(x) - sp.object.size # most general and most efficent
\dontrun{
#   a little bit more to add here
#   object.size(as(dta, "mefa")) # very efficent, but can't cope with strata
}
}
\keyword{ methods }
\keyword{ manip }

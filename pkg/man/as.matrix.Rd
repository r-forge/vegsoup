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
For any object inheriting from class \code{Vegsoup} the methods return an object of class \code{"matrix"} with dimnames giving plot and species names. The latter are strings where any occurrence in a layer is labeled using the layer name and the species abbreviations, collapased with the \code{"@"} gylph (pseudo-species, see \code{\link{Layers}} and \code{\link{split.abbr}}). The returned object is suitable as input to functions in other packages that require an object of class \code{matrix}. Dispatch is guranteed if these functions internally coerce objects to class \code{"matrix"} by calling \code{as.matrix}. Besides that, it is possible to supply a \code{Vegsoup*} object directly to a base function (see \sQuote{Examples}). As there is no way to specify the \code{typeof} or \code{mode} arguments in a call to base function the S3 method for \code{as.matrix()} uses \code{typeof = "numeric"} which is suitable for most applications. If the user wants to ensure that presence/absence data should be returned by calls to \code{as.matrix()} inside base functions, simply assign a standardization method beforehand (e.g. \code{decostand(obj) <- "pa"}). See also \code{\link{as.data.frame}} for passing data to model-fitting and plotting functions.

\code{as.numeric} returns a matrix of mode \code{"numeric"} with species abundances coded by \code{coverscale(obj)\$lims}. Absences are zeros. Equals \code{as.matrix(x, typeof = "numeric")}.

\code{as.character} returns a matrix of mode \code{"character"} with species abundances in the data set's original scale \code{coverscale(obj)\$codes}. Absences are coded as zeros! Equals \code{as.matrix(x, typeof = "character")}.

\code{as.logical} returns a matrix of mode \code{"numeric"} with zeros coding for species absences and ones for presences. Equals \code{as.matrix(obj, mode = "logical")}.

Note, \code{as.logical} and \code{as.matrix(x, "logical")} do not return a boolean or logical matrix composed of \code{TRUE} and \code{FALSE} but use ones to code presences (\code{TRUE}) and zeros (\code{FALSE}) for absences. The \code{mode} (\code{storage.mode}) of the returned matrix is of type \code{"numeric"} (\code{"integer"}).

As defined above, the some how misleading named argument \code{mode} refers to the type of analysis intended on the returned object (recall, \code{"typeof"} determines \code{mode} (\code{storage.mode}) of the returned matrix). The argument \code{mode} can be passed to all methods and has two possible values. If \code{mode = "Q"} pairs of objects are compared and the analysis is said to be in \emph{Q mode}. This is what users usally want or expect. When pairs of descriptors are compared, the analysis is said to be in \emph{Q mode} (Legendre & Legendre 2012). For example, \code{as.matrix(x, "logical", "R")} returns a transposed presence/absence matrix. Of cource, this can also be written as \code{t(as.matrix(x, "logical"))}.

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
Legendre, P. and Legendre, L. (2012). Chaper 7 in Numerical ecology. Developments in environmental modelling, 24.
}
\seealso{
\code{\link{as.data.frame}},
\code{\link{Vegsoup}},
\code{\link{decostand}} and
\code{\link{Coverscale}}
}
\examples{
library(vegsoup)
data(testdata)
dta <- Vegsoup(spc, sts, txa, "braun.blanquet")

# presence/absence representation
dta.pa <- as.logical(dta)
# the same results can be achieved using the classes decostand property
decostand(dta) <- "pa"

# note, that the results of as.numeric() and as.logical()
# are the same if presence/absence (transformation) is applied
dta.pa2 <- as.numeric(dta)

#   for a true boolean matrix change mode
mode(dta.pa) <- "logical"
head(dta.pa)
all.equal(dta.pa, dta.pa2, check.attributes = FALSE)

#   using as.matrix and argument mode
as.matrix(dta, typeof = "character", mode = "R")

#   compact display
as(as.matrix(dta, "logical", "R"), "sparseMatrix")


pro <- protest(as.logical(dta), as.numeric(dta))

# objects of class Vegsoup used in arguments of base functions
# where an object of class matrix is expected
decostand(dta) <- "hellinger"
decorana(dta)
decostand(dta) <- NULL # reset the default

# species matrix representations and memory demands
# class "matrix"
object.size(as.matrix(dta))          # most efficient
# class "list" (pointers and values of all non zero entries)
object.size(indices(dta))            # almost equal 
# coercion to class "sparseMatrix" is based on indices()
object.size(as(dta, "sparseMatrix")) # efficient
# the long format as stored in Vegsoup objects (slot 'species')
object.size(Species(dta))            # most general but least efficent
# a multi-dimensional array of layer replicates
object.size(as.array(dta))           # least efficent

# comparison of objects
#object.size(as(dta, "mefa"))       # most efficent, but can't cope with strata
object.size(as.data.list(dta))  # least efficent, but highly generic
object.size(as.data.frame(dta)) # also efficent
object.size(dta) - object.size(dta@sp.points) - object.size(dta@sp.polygons) # most general most efficent               
}
\keyword{ methods }
\keyword{ manip }
